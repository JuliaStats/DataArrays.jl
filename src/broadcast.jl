using DataArrays
using Base: @get!, promote_eltype
using Base.Broadcast: bitcache_chunks, bitcache_size, dumpbitcache
using Compat: promote_eltype_op

_broadcast_shape(x...) = Base.to_shape(Base.Broadcast.broadcast_indices(x...))

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
function gen_na_conds(f, nd, arrtype, outtype,
    daidx=find(t -> t <: DataArray || t <: PooledDataArray, arrtype), pos=1, isna=())

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
            $(if outtype <: DataArray
                :(@inbounds unsafe_dasetindex!(Bdata, Bc, $val, ind))
            elseif outtype <: PooledDataArray
                :(@inbounds (@nref $nd Brefs i) = _unsafe_pdaref!(Bpool, Brefdict, $val))
            end)
        end
    else
        k = daidx[pos]
        quote
            if $(Symbol("isna_$(k)"))
                $(gen_na_conds(f, nd, arrtype, outtype, daidx, pos+1, tuple(isna..., true)))
            else
                $(if arrtype[k] <: DataArray
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
Base.map!{F}(f::F, B::Union{DataArray, PooledDataArray}, A0::AbstractArray, As::AbstractArray...) =
        broadcast!(f, B, A0, As...)
Base.map!{F}(f::F, B::Union{DataArray, PooledDataArray}, A0, As...) =
        broadcast!(f, B, A0, As...)

@generated function _broadcast!(f, B::Union{DataArray, PooledDataArray}, As...)

    F  = :(f)
    nd = ndims(B)
    N  = length(As)

    dataarrays = find(t -> t <: DataArray, As)

    quote
        @boundscheck Base.Broadcast.check_broadcast_indices(indices(B), As...)
        # check_broadcast_shape(size(B), As...)
        @nexprs $N i->(A_i = As[i])

        @assert ndims(B) == $nd

        # Set up input DataArray/PooledDataArrays
        $(Expr(:block, [
            As[k] <: DataArray ? quote
                $(Symbol("na_$(k)")) = $(Symbol("A_$(k)")).na.chunks
                $(Symbol("data_$(k)")) = $(Symbol("A_$(k)")).data
                $(Symbol("state_$(k)_0")) = $(Symbol("state_$(k)_$(nd)")) = 1
                @nexprs $nd d->($(Symbol("skip_$(k)_d")) = size($(Symbol("data_$(k)")), d) == 1)
            end : As[k] <: PooledDataArray ? quote
                $(Symbol("refs_$(k)")) = $(Symbol("A_$(k)")).refs
                $(Symbol("pool_$(k)")) = $(Symbol("A_$(k)")).pool
            end : nothing
        for k = 1:N]...))

        # Set up output DataArray/PooledDataArray
        $(if B <: DataArray
            quote
                Bdata = B.data
                # Copy in case aliased
                # TODO: check for aliasing?
                Bna = falses(size(Bdata))
                Bc = Bna.chunks
                ind = 1
            end
        elseif B <: PooledDataArray
            quote
                Bpool = B.pool = similar(B.pool, 0)
                Brefs = B.refs
                Brefdict = Dict{eltype(Bpool),eltype(Brefs)}()
            end
        end)

        @nloops($nd, i, $(B <: DataArray ? (:Bdata) : (:Brefs)),
            # pre
            d->($(Expr(:block, [
                As[k] <: DataArray ? quote
                    $(Symbol("state_$(k)_")){d-1} = $(Symbol("state_$(k)_d"));
                    $(Symbol("j_$(k)_d")) = $(Symbol("skip_$(k)_d")) ? 1 : i_d
                end : (As[k] <: AbstractArray ? quote
                    $(Symbol("j_$(k)_d")) = size($(Symbol("A_$(k)")), d) == 1 ? 1 : i_d
                end : quote
                    $(Symbol("j_$(k)_d")) = 1
                end)
            for k = 1:N]...))),

            # post
            d->($(Expr(:block, [quote
                $(Symbol("skip_$(k)_d")) || ($(Symbol("state_$(k)_d")) = $(Symbol("state_$(k)_0")))
            end for k in dataarrays]...))),

            # body
            begin
                # Advance iterators for DataArray and determine NA status
                $(Expr(:block, [
                    As[k] <: DataArray ? quote
                        @inbounds $(Symbol("isna_$(k)")) = Base.unsafe_bitgetindex($(Symbol("na_$(k)")), $(Symbol("state_$(k)_0")))
                    end : As[k] <: PooledDataArray ? quote
                        @inbounds $(Symbol("r_$(k)")) = @nref $nd $(Symbol("refs_$(k)")) d->$(Symbol("j_$(k)_d"))
                        $(Symbol("isna_$(k)")) = $(Symbol("r_$(k)")) == 0
                    end : nothing
                for k = 1:N]...))

                # Extract values for other type
                $(Expr(:block, [
                    As[k] <: AbstractArray  && !(As[k] <: AbstractDataArray) ? quote
                        # ordinary AbstractArrays
                        @inbounds $(Symbol("v_$(k)")) = @nref $nd $(Symbol("A_$(k)")) d->$(Symbol("j_$(k)_d"))
                    end : quote
                        # non AbstractArrays (e.g. Strings and Numbers)
                        @inbounds $(Symbol("v_$(k)")) = $(Symbol("A_$(k)"))
                    end
                for k = 1:N]...))

                # Compute and store return value
                $(gen_na_conds(F, nd, As, B))

                # Increment state
                $(Expr(:block, [:($(Symbol("state_$(k)_0")) += 1) for k in dataarrays]...))
                $(if B <: DataArray
                    :(ind += 1)
                end)
            end)

        $(if B <: DataArray
            :(B.na = Bna)
        end)

        return B
    end
end
Base.Broadcast.broadcast!(f, B::Union{DataArray, PooledDataArray}, ::Type{T}, As...) where T =
    _broadcast!((t...) -> f(T, t...), B, As...)
Base.Broadcast.broadcast!(f, B::Union{DataArray, PooledDataArray}, A0::Number, As::Number...) =
    _broadcast!(f, B, A0, As...)
Base.Broadcast.broadcast!(f, B::Union{DataArray, PooledDataArray}, A0, As...) =
    _broadcast!(f, B, A0, As...)

Base.Broadcast.promote_containertype(::Type{DataArray}, ::Type{DataArray})             = DataArray
Base.Broadcast.promote_containertype(::Type{PooledDataArray}, ::Type{PooledDataArray}) = PooledDataArray
Base.Broadcast.promote_containertype(::Type{DataArray}, ::Type{Array})                 = DataArray
Base.Broadcast.promote_containertype(::Type{PooledDataArray}, ::Type{Array})           = PooledDataArray
Base.Broadcast.promote_containertype(::Type{Array}, ::Type{DataArray})                 = DataArray
Base.Broadcast.promote_containertype(::Type{Array}, ::Type{PooledDataArray})           = PooledDataArray
Base.Broadcast.promote_containertype(::Type{DataArray}, ::Type{PooledDataArray})       = DataArray
Base.Broadcast.promote_containertype(::Type{PooledDataArray}, ::Type{DataArray})       = DataArray
Base.Broadcast.promote_containertype(::Type{DataArray}, ct)                            = DataArray
Base.Broadcast.promote_containertype(::Type{PooledDataArray}, ct)                      = PooledDataArray
Base.Broadcast.promote_containertype(ct, ::Type{DataArray})                            = DataArray
Base.Broadcast.promote_containertype(ct, ::Type{PooledDataArray})                      = PooledDataArray
Base.Broadcast._containertype(::Type{T}) where T<:DataArray           = DataArray
Base.Broadcast._containertype(::Type{T}) where T<:PooledDataArray     = PooledDataArray
Base.Broadcast.broadcast_indices(::Type{T}, A) where T<:AbstractDataArray = indices(A)

@inline function Base.Broadcast.broadcast_c{S<:AbstractDataArray}(f, ::Type{S}, A, Bs...)
    T     = Base.Broadcast._broadcast_eltype(f, A, Bs...)
    shape = Base.Broadcast.broadcast_indices(A, Bs...)
    dest = S(T, Base.index_lengths(shape...))
    return broadcast!(f, dest, A, Bs...)
end
