using DataArrays, Base.Cartesian, Base.@get!
using Base.Broadcast: bitcache_chunks, bitcache_size, dumpbitcache, check_broadcast_shape,
                      promote_eltype, broadcast_shape, eltype_plus, type_minus, type_div,
                      type_pow

function gen_broadcast_body_dataarray(nd::Int, arrtype::(DataType...), outtype, f::Function)
    F = Expr(:quote, f)
    narrays = length(arrtype)
    quote
        @assert ndims(B) == $nd
        @ncall $narrays check_broadcast_shape size(B) k->A_k

        # Set up input DataArray/PooledDataArrays
        $(Expr(:block, [
            arrtype[k] == DataArray ? quote
                $(symbol("na_$(k)")) = $(symbol("A_$(k)")).na.chunks
                $(symbol("data_$(k)")) = $(symbol("A_$(k)")).data
                $(symbol("state_$(k)_0")) = $(symbol("state_$(k)_$(nd)")) = 1
                @nexprs $nd d->($(symbol("skip_$(k)_d")) = size($(symbol("data_$(k)")), d) == 1)
            end : arrtype[k] == PooledDataArray ? quote
                $(symbol("refs_$(k)")) = $(symbol("A_$(k)")).refs
                $(symbol("pool_$(k)")) = $(symbol("A_$(k)")).pool
            end : nothing
        for k = 1:length(arrtype)]...))

        # Set up output DataArray/PooledDataArray
        $(if outtype == DataArray
            quote
                Bc = B.na.chunks
                fill!(Bc, 0)
                Bdata = B.data
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
                    $(symbol("state_$(k)_")){d-1} = $(symbol("state_$(k)_d"));
                    $(symbol("j_$(k)_d")) = $(symbol("skip_$(k)_d")) ? 1 : i_d
                end : quote
                    $(symbol("j_$(k)_d")) = size($(symbol("A_$(k)")), d) == 1 ? 1 : i_d
                end
            for k = 1:length(arrtype)]...))),

            # post
            d->($(Expr(:block, [quote
                $(symbol("skip_$(k)_d")) || ($(symbol("state_$(k)_d")) = $(symbol("state_$(k)_0")))
            end for k in find([arrtype...] .== DataArray)]...))),

            # body
            begin
                # Advance iterators for DataArray and extract NA values
                $(Expr(:block, [
                    arrtype[k] == DataArray ? quote
                        #@inbounds $(symbol("isna_$(k)")) = Base.unsafe_bitgetindex($(symbol("na_$(k)")), $(symbol("state_$(k)_0")))
                        # XXX use the code above if/when unsafe_bitgetindex is inlined
                        @inbounds $(symbol("isna_$(k)")) = ($(symbol("na_$(k)"))[Base.@_div64($(symbol("state_$(k)_0"))-1)+1] & (uint64(1)<<Base.@_mod64($(symbol("state_$(k)_0"))-1))) != 0
                    end : arrtype[k] == PooledDataArray ? quote
                        @inbounds $(symbol("r_$(k)")) = @nref $nd $(symbol("refs_$(k)")) d->$(symbol("j_$(k)_d"))
                        $(symbol("isna_$(k)")) = $(symbol("r_$(k)")) == 0
                    end : nothing
                for k = 1:length(arrtype)]...))
                
                # Check if NA
                if $(Expr(:call, :|, [symbol("isna_$k") for k in find([arrtype...] .!= AbstractArray)]...))
                    # Set NA
                    $(if outtype == DataArray
                        quote
                            i1, i2 = Base.get_chunks_id(ind)
                            @inbounds Bc[i1] |= uint64(1) << i2
                        end
                    elseif outtype == PooledDataArray
                        :(@inbounds (@nref $nd Brefs i) = zero(eltype(Brefs)))
                    end)
                else
                    # If not NA, extract data values
                    $(Expr(:block, [
                        arrtype[k] == DataArray ? quote
                            @inbounds $(symbol("v_$(k)")) = $(symbol("data_$(k)"))[$(symbol("state_$(k)_0"))]
                        end : arrtype[k] == PooledDataArray ? quote
                            @inbounds $(symbol("v_$(k)")) = $(symbol("pool_$(k)"))[$(symbol("r_$(k)"))]
                        end : quote
                            @inbounds $(symbol("v_$(k)")) = @nref $nd $(symbol("A_$(k)")) d->$(symbol("j_$(k)_d"))
                        end
                    for k = 1:narrays]...))

                    # Compute and store return value
                    x = (@ncall $narrays $F v)
                    $(if outtype == DataArray
                        :(@inbounds Bdata[ind] = x)
                    elseif outtype == PooledDataArray
                        quote
                            @inbounds (@nref $nd Brefs i) = @get! Brefdict x begin
                                push!(Bpool, x)
                                convert(eltype(Brefs), length(Bpool))
                            end
                        end
                    end)
                end

                $(Expr(:block, [:($(symbol("state_$(k)_0")) += 1) for k in find([arrtype...] .== DataArray)]...))
                $(if outtype == DataArray
                    :(ind += 1)
                end)
            end)
    end
end

function gen_broadcast_function_to(genbody::Function, nd::Int, arrtype::(DataType...), outtype::DataType, f::Function)
    As = [symbol("A_"*string(i)) for i = 1:length(arrtype)]
    body = genbody(nd, arrtype, outtype, f)
    @eval begin
        local _F_
        function _F_(B::$(outtype), $(As...))
            $body
        end
        _F_
    end
end

datype(A_1::PooledDataArray, As...) = tuple(PooledDataArray, datype(As...)...)
datype(A_1::DataArray, As...) = tuple(DataArray, datype(As...)...)
datype(A_1, As...) = tuple(AbstractArray, datype(As...)...)
datype() = ()

for bsig in (DataArray, PooledDataArray), asig in (Union(Array,BitArray,Number), Any)
    @eval let cache = Dict{Function,Dict{(DataType...),Dict{Int,Function}}}()
        function Base.broadcast!(f::Function, B::$bsig, As::$asig...)
            nd = ndims(B)
            arrtype = datype(As...)

            cache_f    = @get! cache      f        Dict{(DataType...),Dict{Int,Function}}()
            cache_f_na = @get! cache_f    arrtype  Dict{Int,Function}()
            func       = @get! cache_f_na nd       gen_broadcast_function_to(gen_broadcast_body_dataarray, nd, arrtype, $bsig, f)

            # println(code_typed(func, typeof(tuple(B, As...))))
            # println(code_llvm(func, typeof(tuple(B, As...))))
            # println(code_native(func, typeof(tuple(B, As...))))
            func(B, As...)
            B
        end
    end
end

databroadcast(f::Function, As...) = broadcast!(f, DataArray(promote_eltype(As...), broadcast_shape(As...)), As...)
pdabroadcast(f::Function, As...) = broadcast!(f, PooledDataArray(promote_eltype(As...), broadcast_shape(As...)), As...)

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
        error("@da_broadcast_vararg may only be applied to vararg functions")
    end

    va = func.args[1].args[end]
    defs = {}
    for n = 1:4, aa = 0:n-1
        def = deepcopy(func)
        rep = Any[symbol("A_$(i)") for i = 1:n]
        push!(rep, va)
        exreplace!(def.args[2], va, rep)
        rep = cell(n+1)
        for i = 1:aa
            rep[i] = Expr(:(::), symbol("A_$i"), AbstractArray)
        end
        for i = aa+1:n
            rep[i] = Expr(:(::), symbol("A_$i"), Union(DataArray, PooledDataArray))
        end
        rep[end] = Expr(:..., Expr(:(::), va.args[1], AbstractArray))
        exreplace!(def.args[1], va, rep)
        push!(defs, def)
    end
    esc(Expr(:block, defs...))
end

macro da_broadcast_binary(func)
    if (func.head != :function && func.head != :(=)) ||
       func.args[1].head != :call || length(func.args[1].args) != 3
        error("@da_broadcast_binary may only be applied to two-argument functions")
    end
    (f, A, B) = func.args[1].args
    body = func.args[2]
    quote
        $f($A::Union(DataArray, PooledDataArray), $B::Union(DataArray, PooledDataArray)) = $(body)
        $f($A::Union(DataArray, PooledDataArray), $B::AbstractArray) = $(body)
        $f($A::AbstractArray, $B::Union(DataArray, PooledDataArray)) = $(body)
    end
end

# Broadcasting DataArrays returns a DataArray
@da_broadcast_vararg Base.broadcast(f::Function, As...) = databroadcast(f, As...)

# Definitions for operators, 
Base.(:(.*))(A::BitArray, B::Union(DataArray{Bool}, PooledDataArray{Bool})) = databroadcast(*, A, B)
Base.(:(.*))(A::Union(DataArray{Bool}, PooledDataArray{Bool}), B::BitArray) = databroadcast(*, A, B)
@da_broadcast_vararg Base.(:(.*))(As...) = databroadcast(*, As...)
@da_broadcast_binary Base.(:(.%))(A, B) = databroadcast(%, A, B)
@da_broadcast_vararg Base.(:(.+))(As...) = broadcast!(+, DataArray(eltype_plus(As...), broadcast_shape(As...)), As...)
@da_broadcast_binary Base.(:(.-))(A, B) = broadcast!(-, DataArray(type_minus(eltype(A), eltype(B)), broadcast_shape(A,B)), A, B)
@da_broadcast_binary Base.(:(./))(A, B) = broadcast!(/, DataArray(type_div(eltype(A), eltype(B)), broadcast_shape(A, B)), A, B)
@da_broadcast_binary Base.(:(.\))(A, B) = broadcast!(\, DataArray(type_div(eltype(A), eltype(B)), broadcast_shape(A, B)), A, B)
Base.(:(.^))(A::Union(DataArray{Bool}, PooledDataArray{Bool}), B::Union(DataArray{Bool}, PooledDataArray{Bool})) = databroadcast(>=, A, B)
Base.(:(.^))(A::BitArray, B::Union(DataArray{Bool}, PooledDataArray{Bool})) = databroadcast(>=, A, B)
Base.(:(.^))(A::AbstractArray{Bool}, B::Union(DataArray{Bool}, PooledDataArray{Bool})) = databroadcast(>=, A, B)
Base.(:(.^))(A::Union(DataArray{Bool}, PooledDataArray{Bool}), B::BitArray) = databroadcast(>=, A, B)
Base.(:(.^))(A::Union(DataArray{Bool}, PooledDataArray{Bool}), B::AbstractArray{Bool}) = databroadcast(>=, A, B)
@da_broadcast_binary Base.(:(.^))(A, B) = broadcast!(^, DataArray(type_pow(eltype(A), eltype(B)), broadcast_shape(A, B)), A, B)

# XXX is a PDA the right return type for these?
Base.broadcast(f::Function, As::PooledDataArray...) = pdabroadcast(f, As...)
Base.(:(.*))(As::PooledDataArray...) = pdabroadcast(*, As...)
Base.(:(.%))(A::PooledDataArray, B::PooledDataArray) = pdabroadcast(%, A, B)
Base.(:(.+))(As::PooledDataArray...) = broadcast!(+, PooledDataArray(eltype_plus(As...), broadcast_shape(As...)), As...)
Base.(:(.-))(A::PooledDataArray, B::PooledDataArray) =
    broadcast!(-, PooledDataArray(type_minus(eltype(A), eltype(B)), broadcast_shape(A,B)), A, B)
Base.(:(./))(A::PooledDataArray, B::PooledDataArray) =
    broadcast!(/, PooledDataArray(type_div(eltype(A), eltype(B)), broadcast_shape(A, B)), A, B)
Base.(:(.\))(A::PooledDataArray, B::PooledDataArray) =
    broadcast!(\, PooledDataArray(type_div(eltype(A), eltype(B)), broadcast_shape(A, B)), A, B)
Base.(:(.^))(A::PooledDataArray{Bool}, B::PooledDataArray{Bool}) = databroadcast(>=, A, B)
Base.(:(.^))(A::PooledDataArray, B::PooledDataArray) =
    broadcast!(^, PooledDataArray(type_pow(eltype(A), eltype(B)), broadcast_shape(A, B)), A, B)
