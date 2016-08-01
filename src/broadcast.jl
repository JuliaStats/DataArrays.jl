using DataArrays
using Base: @get!, promote_eltype
using Base.Broadcast: bitcache_chunks, bitcache_size, dumpbitcache, broadcast_shape
using Compat: promote_eltype_op

if isdefined(Base, :OneTo)
    _broadcast_shape(x...) = Base.to_shape(broadcast_shape(x...))
else
    const _broadcast_shape = broadcast_shape
end

# Check that all arguments are broadcast compatible with shape
# Differs from Base in that we check for exact matches
function check_broadcast_shape(shape::Dims, As::(@compat Union{AbstractArray,Number})...)
    samesize = true
    for A in As
        if ndims(A) > length(shape)
            throw(DimensionMismatch("cannot broadcast array to have fewer dimensions"))
        end
        for k in 1:length(shape)
            n, nA = shape[k], size(A, k)
            samesize &= (n == nA)
            if n != nA != 1
                throw(DimensionMismatch("array could not be broadcast to match destination"))
            end
        end
    end
    samesize
end

# Get ref for value for a PooledDataArray, adding to the pool if
# necessary
_unsafe_pdaref!(Bpool, Brefdict::Dict, val::NAtype) = 0
function _unsafe_pdaref!(Bpool, Brefdict::Dict, val)
    @get! Brefdict val begin
        push!(Bpool, val)
        length(Bpool)
    end
end

# Generate a branch for each possible combination of NA/not NA. This
# gives good performance at the cost of 2^narrays branches.
function gen_na_conds(f, nd, arrtype, outtype, daidx=find([arrtype...] .!= AbstractArray), pos=1, isna=())
    if pos > length(daidx)
        args = Any[Symbol("v_$(k)") for k = 1:length(arrtype)]
        for i = 1:length(daidx)
            if isna[i]
                args[daidx[i]] = NA
            end
        end

        # Needs to be gensymmed so that the compiler won't box it
        val = gensym("val")
        quote
            $val = $(Expr(:call, f, args...))
            $(if outtype == DataArray
                :(@inbounds unsafe_dasetindex!(Bdata, Bc, $val, ind))
            elseif outtype == PooledDataArray
                :(@inbounds (@nref $nd Brefs i) = _unsafe_pdaref!(Bpool, Brefdict, $val))
            end)
        end
    else
        k = daidx[pos]
        quote
            if $(Symbol("isna_$(k)"))
                $(gen_na_conds(f, nd, arrtype, outtype, daidx, pos+1, tuple(isna..., true)))
            else
                $(if arrtype[k] == DataArray
                    :(@inbounds $(Symbol("v_$(k)")) = $(Symbol("data_$(k)"))[$(Symbol("state_$(k)_0"))])
                else
                    :(@inbounds $(Symbol("v_$(k)")) = $(Symbol("pool_$(k)"))[$(Symbol("r_$(k)"))])
                end)
                $(gen_na_conds(f, nd, arrtype, outtype, daidx, pos+1, tuple(isna..., false)))
            end
        end
    end
end

# Broadcast implementation for DataArrays
#
# TODO: Fall back on faster implementation for same-sized inputs when
# it is safe to do so.
function gen_broadcast_dataarray(nd::Int, arrtype::@compat(Tuple{Vararg{DataType}}), outtype, f::Function)
    F = Expr(:quote, f)
    narrays = length(arrtype)
    As = [Symbol("A_$(i)") for i = 1:narrays]
    dataarrays = find([arrtype...] .== DataArray)
    abstractdataarrays = find([arrtype...] .!= AbstractArray)
    have_fastpath = outtype == DataArray && all(x->!(x <: PooledDataArray), arrtype)

    @eval let
        function _F_(B::$(outtype), $(As...))
            @assert ndims(B) == $nd

            # Set up input DataArray/PooledDataArrays
            $(Expr(:block, [
                arrtype[k] == DataArray ? quote
                    $(Symbol("na_$(k)")) = $(Symbol("A_$(k)")).na.chunks
                    $(Symbol("data_$(k)")) = $(Symbol("A_$(k)")).data
                    $(Symbol("state_$(k)_0")) = $(Symbol("state_$(k)_$(nd)")) = 1
                    @nexprs $nd d->($(Symbol("skip_$(k)_d")) = size($(Symbol("data_$(k)")), d) == 1)
                end : arrtype[k] == PooledDataArray ? quote
                    $(Symbol("refs_$(k)")) = $(Symbol("A_$(k)")).refs
                    $(Symbol("pool_$(k)")) = $(Symbol("A_$(k)")).pool
                end : nothing
            for k = 1:narrays]...))

            # Set up output DataArray/PooledDataArray
            $(if outtype == DataArray
                quote
                    Bdata = B.data
                    # Copy in case aliased
                    # TODO: check for aliasing?
                    Bna = falses(size(Bdata))
                    Bc = Bna.chunks
                    ind = 1
                end
            elseif outtype == PooledDataArray
                quote
                    Bpool = B.pool = similar(B.pool, 0)
                    Brefs = B.refs
                    Brefdict = Dict{eltype(Bpool),eltype(Brefs)}()
                end
            end)

            @nloops($nd, i, $(outtype == DataArray ? (:Bdata) : (:Brefs)),
                # pre
                d->($(Expr(:block, [
                    arrtype[k] == DataArray ? quote
                        $(Symbol("state_$(k)_")){d-1} = $(Symbol("state_$(k)_d"));
                        $(Symbol("j_$(k)_d")) = $(Symbol("skip_$(k)_d")) ? 1 : i_d
                    end : quote
                        $(Symbol("j_$(k)_d")) = size($(Symbol("A_$(k)")), d) == 1 ? 1 : i_d
                    end
                for k = 1:narrays]...))),

                # post
                d->($(Expr(:block, [quote
                    $(Symbol("skip_$(k)_d")) || ($(Symbol("state_$(k)_d")) = $(Symbol("state_$(k)_0")))
                end for k in dataarrays]...))),

                # body
                begin
                    # Advance iterators for DataArray and determine NA status
                    $(Expr(:block, [
                        arrtype[k] == DataArray ? quote
                            @inbounds $(Symbol("isna_$(k)")) = Base.unsafe_bitgetindex($(Symbol("na_$(k)")), $(Symbol("state_$(k)_0")))
                        end : arrtype[k] == PooledDataArray ? quote
                            @inbounds $(Symbol("r_$(k)")) = @nref $nd $(Symbol("refs_$(k)")) d->$(Symbol("j_$(k)_d"))
                            $(Symbol("isna_$(k)")) = $(Symbol("r_$(k)")) == 0
                        end : nothing
                    for k = 1:narrays]...))

                    # Extract values for ordinary AbstractArrays
                    $(Expr(:block, [
                        :(@inbounds $(Symbol("v_$(k)")) = @nref $nd $(Symbol("A_$(k)")) d->$(Symbol("j_$(k)_d")))
                    for k = find([arrtype...] .== AbstractArray)]...))

                    # Compute and store return value
                    $(gen_na_conds(F, nd, arrtype, outtype))

                    # Increment state
                    $(Expr(:block, [:($(Symbol("state_$(k)_0")) += 1) for k in dataarrays]...))
                    $(if outtype == DataArray
                        :(ind += 1)
                    end)
                end)

            $(if outtype == DataArray
                :(B.na = Bna)
            end)
        end
        _F_
    end
end

datype(A_1::PooledDataArray, As...) = tuple(PooledDataArray, datype(As...)...)
datype(A_1::DataArray, As...) = tuple(DataArray, datype(As...)...)
datype(A_1, As...) = tuple(AbstractArray, datype(As...)...)
datype() = ()

datype_int(A_1::PooledDataArray, As...) = (@compat(UInt64(2)) | (datype_int(As...) << 2))
datype_int(A_1::DataArray, As...) = (@compat(UInt64(1)) | (datype_int(As...) << 2))
datype_int(A_1, As...) = (datype_int(As...) << 2)
datype_int() = @compat UInt64(0)

# The following four methods are to avoid ambiguity warnings on 0.4
Base.map!(f::Base.Callable, B::DataArray) =
    invoke(map!, Tuple{Base.Callable, AbstractArray}, f, B)
Base.map!(f::Base.Callable, B::PooledDataArray) =
    invoke(map!, Tuple{Base.Callable, AbstractArray}, f, B)
Base.broadcast!(f::Base.Function, B::DataArray) =
    invoke(map!, Tuple{Base.Callable, AbstractArray}, f, B)
Base.broadcast!(f::Base.Function, B::PooledDataArray) =
    invoke(map!, Tuple{Base.Callable, AbstractArray}, f, B)

for bsig in (DataArray, PooledDataArray), asig in (Union{Array,BitArray,Number},DataArray, PooledDataArray,)
    @eval let cache = Dict{Function,Dict{UInt64,Dict{Int,Function}}}()
        function Base.map!(f::Base.Callable, B::$bsig, As::$asig...)
            nd = ndims(B)
            length(As) <= 8 || throw(ArgumentError("too many arguments"))
            samesize = check_broadcast_shape(size(B), As...)
            samesize || throw(DimensionMismatch("Argument dimensions must match"))
            arrtype = datype_int(As...)

            cache_f    = @get! cache      f        Dict{UInt64,Dict{Int,Function}}()
            cache_f_na = @get! cache_f    arrtype  Dict{Int,Function}()
            func       = @get! cache_f_na nd       gen_broadcast_dataarray(nd, datype(As...), $bsig, f)

            func(B, As...)
            B
        end

        function Base.broadcast!(f::Function, B::$bsig, As::$asig...)
            nd = ndims(B)
            length(As) <= 8 || throw(ArgumentError("too many arguments"))
            samesize = check_broadcast_shape(size(B), As...)
            arrtype = datype_int(As...)

            cache_f    = @get! cache      f        Dict{UInt64,Dict{Int,Function}}()
            cache_f_na = @get! cache_f    arrtype  Dict{Int,Function}()
            func       = @get! cache_f_na nd       gen_broadcast_dataarray(nd, datype(As...), $bsig, f)

            # println(code_typed(func, typeof(tuple(B, As...))))
            func(B, As...)
            B
        end
    end
end

databroadcast(f::Function, As...) = broadcast!(f, DataArray(promote_eltype(As...), _broadcast_shape(As...)), As...)
pdabroadcast(f::Function, As...) = broadcast!(f, PooledDataArray(promote_eltype(As...), _broadcast_shape(As...)), As...)

function exreplace!(ex::Expr, search, rep)
    for i = 1:length(ex.args)
        if ex.args[i] == search
            splice!(ex.args, i, rep)
            break
        else
            exreplace!(ex.args[i], search, rep)
        end
    end
    ex
end
exreplace!(ex, search, rep) = ex

macro da_broadcast_vararg(func)
    if (func.head != :function && func.head != :(=)) ||
       func.args[1].head != :call || !isa(func.args[1].args[end], Expr) ||
       func.args[1].args[end].head != :...
        throw(ArgumentError("@da_broadcast_vararg may only be applied to vararg functions"))
    end

    va = func.args[1].args[end]
    defs = Any[]
    for n = 1:4, aa = 0:n-1
        def = deepcopy(func)
        rep = Any[Symbol("A_$(i)") for i = 1:n]
        push!(rep, va)
        exreplace!(def.args[2], va, rep)
        rep = Vector{Any}(n+1)
        for i = 1:aa
            rep[i] = Expr(:(::), Symbol("A_$i"), AbstractArray)
        end
        for i = aa+1:n
            rep[i] = Expr(:(::), Symbol("A_$i"), (@compat Union{DataArray, PooledDataArray}))
        end
        rep[end] = Expr(:..., Expr(:(::), va.args[1], AbstractArray))
        exreplace!(def.args[1], va, rep)
        push!(defs, def)
    end
    esc(Expr(:block, defs...))
end

macro da_broadcast_binary(func)
    if (func.head != :function && func.head != :(=)) ||
       func.args[1].head != :call ||
       length(func.args[1].args) != 3
        throw(ArgumentError("@da_broadcast_binary may only be applied to two-argument functions"))
    end
    (ff, A, B) = func.args[1].args
    f = esc(ff)
    body = func.args[2]
    quote
        ($f)($A::(@compat Union{DataArray, PooledDataArray}), $B::(@compat Union{DataArray, PooledDataArray})) = $(body)
        ($f)($A::(@compat Union{DataArray, PooledDataArray}), $B::AbstractArray) = $(body)
        ($f)($A::AbstractArray, $B::(@compat Union{DataArray, PooledDataArray})) = $(body)
    end
end

# Broadcasting DataArrays returns a DataArray
@da_broadcast_vararg Base.broadcast(f::Function, As...) = databroadcast(f, As...)

# Definitions for operators,
(.*)(A::BitArray, B::(@compat Union{DataArray{Bool}, PooledDataArray{Bool}})) = databroadcast(*, A, B)
(.*)(A::(@compat Union{DataArray{Bool}, PooledDataArray{Bool}}), B::BitArray) = databroadcast(*, A, B)
@da_broadcast_vararg (.*)(As...) = databroadcast(*, As...)
@da_broadcast_binary (.%)(A, B) = databroadcast(%, A, B)
@da_broadcast_vararg (.+)(As...) = broadcast!(+, DataArray(promote_eltype_op(@functorize(+), As...), _broadcast_shape(As...)), As...)
@da_broadcast_binary (.-)(A, B) =
    broadcast!(-, DataArray(promote_op(@functorize(-), eltype(A), eltype(B)),
                            _broadcast_shape(A,B)), A, B)
@da_broadcast_binary (./)(A, B) =
    broadcast!(/, DataArray(promote_op(@functorize(/), eltype(A), eltype(B)),
                            _broadcast_shape(A, B)), A, B)
@da_broadcast_binary (.\)(A, B) =
    broadcast!(\, DataArray(promote_op(@functorize(\), eltype(A), eltype(B)),
                            _broadcast_shape(A, B)), A, B)
(.^)(A::(@compat Union{DataArray{Bool}, PooledDataArray{Bool}}), B::(@compat Union{DataArray{Bool}, PooledDataArray{Bool}})) = databroadcast(>=, A, B)
(.^)(A::BitArray, B::(@compat Union{DataArray{Bool}, PooledDataArray{Bool}})) = databroadcast(>=, A, B)
(.^)(A::AbstractArray{Bool}, B::(@compat Union{DataArray{Bool}, PooledDataArray{Bool}})) = databroadcast(>=, A, B)
(.^)(A::(@compat Union{DataArray{Bool}, PooledDataArray{Bool}}), B::BitArray) = databroadcast(>=, A, B)
(.^)(A::(@compat Union{DataArray{Bool}, PooledDataArray{Bool}}), B::AbstractArray{Bool}) = databroadcast(>=, A, B)
@da_broadcast_binary (.^)(A, B) =
    broadcast!(^, DataArray(promote_op(@functorize(^), eltype(A), eltype(B)),
                            _broadcast_shape(A, B)), A, B)

# XXX is a PDA the right return type for these?
Base.broadcast(f::Function, As::PooledDataArray...) = pdabroadcast(f, As...)
(.*)(As::PooledDataArray...) = pdabroadcast(*, As...)
(.%)(A::PooledDataArray, B::PooledDataArray) = pdabroadcast(%, A, B)
(.+)(As::PooledDataArray...) =
    broadcast!(+, PooledDataArray(promote_eltype_op(@functorize(+), As...), _broadcast_shape(As...)), As...)
(.-)(A::PooledDataArray, B::PooledDataArray) =
    broadcast!(-, PooledDataArray(promote_op(@functorize(-), eltype(A), eltype(B)),
                                  _broadcast_shape(A,B)), A, B)
(./)(A::PooledDataArray, B::PooledDataArray) =
    broadcast!(/, PooledDataArray(promote_op(@functorize(/), eltype(A), eltype(B)),
                                  _broadcast_shape(A, B)), A, B)
(.\)(A::PooledDataArray, B::PooledDataArray) =
    broadcast!(\, PooledDataArray(promote_op(@functorize(\), eltype(A), eltype(B)),
                                  _broadcast_shape(A, B)), A, B)
(.^)(A::PooledDataArray{Bool}, B::PooledDataArray{Bool}) = databroadcast(>=, A, B)
(.^)(A::PooledDataArray, B::PooledDataArray) =
    broadcast!(^, PooledDataArray(promote_op(@functorize(^), eltype(A), eltype(B)),
                                  _broadcast_shape(A, B)), A, B)

for (sf, vf) in zip(scalar_comparison_operators, array_comparison_operators)
    @eval begin
        # ambiguity
        $(vf)(A::(@compat Union{PooledDataArray{Bool},DataArray{Bool}}), B::(@compat Union{PooledDataArray{Bool},DataArray{Bool}})) =
            broadcast!($sf, DataArray(Bool, _broadcast_shape(A, B)), A, B)
        $(vf)(A::(@compat Union{PooledDataArray{Bool},DataArray{Bool}}), B::AbstractArray{Bool}) =
            broadcast!($sf, DataArray(Bool, _broadcast_shape(A, B)), A, B)
        $(vf)(A::AbstractArray{Bool}, B::(@compat Union{PooledDataArray{Bool},DataArray{Bool}})) =
            broadcast!($sf, DataArray(Bool, _broadcast_shape(A, B)), A, B)

        @da_broadcast_binary $(vf)(A, B) = broadcast!($sf, DataArray(Bool, _broadcast_shape(A, B)), A, B)
    end
end
