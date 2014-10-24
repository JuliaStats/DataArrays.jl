if VERSION < v"0.4.0-dev+1274"
    import Base.evaluate
else
    evaluate(f, args...) = f(args...)
end

## mapreduce implementation that skips NA

function skipna_init(f, op, na::BitArray, data::Array, ifirst::Int, ilast::Int)
    # Get first non-NA element
    ifirst = Base.findnextnot(na, ifirst)
    @inbounds d1 = data[ifirst]

    # Get next non-NA element
    ifirst = Base.findnextnot(na, ifirst+1)
    @inbounds d2 = data[ifirst]

    # Reduce first two elements
    (evaluate(op, evaluate(f, d1), evaluate(f, d2)), ifirst)
end

function mapreduce_seq_impl_skipna(f, op, T, A::DataArray, ifirst::Int, ilast::Int)
    data = A.data
    na = A.na
    chunks = na.chunks
    
    v, i = skipna_init(f, op, na, data, ifirst, ilast)

    while i < ilast
        i += 1
        @inbounds na = Base.unsafe_bitgetindex(chunks, i)
        na && continue
        @inbounds d = data[i]
        v = evaluate(op, v, evaluate(f, d))
    end
    v
end

# Pairwise map-reduce
function mapreduce_pairwise_impl_skipna{T}(f, op, A::DataArray{T}, bytefirst::Int, bytelast::Int, n_notna::Int, blksize::Int)
    if n_notna <= blksize
        ifirst = 64*(bytefirst-1)+1
        ilast = min(64*bytelast, length(A))
        # Fall back to Base implementation if no NAs in block
        return ilast - ifirst + 1 == n_notna ? Base.mapreduce_seq_impl(f, op, A.data, ifirst, ilast) :
                                               mapreduce_seq_impl_skipna(f, op, T, A, ifirst, ilast)
    end

    # Find byte in the middle of range
    # The block size is restricted so that there will always be at
    # least two non-NA elements in the returned range
    chunks = A.na.chunks
    nmid = 0
    imid = bytefirst-1
    while nmid < (n_notna >> 1)
        imid += 1
        @inbounds nmid += count_zeros(chunks[imid])
    end

    v1 = mapreduce_pairwise_impl_skipna(f, op, A, bytefirst, imid, nmid, blksize)
    v2 = mapreduce_pairwise_impl_skipna(f, op, A, imid+1, bytelast, n_notna-nmid, blksize)
    evaluate(op, v1, v2)
end

mapreduce_impl_skipna{T}(f, op, A::DataArray{T}) =
    mapreduce_seq_impl_skipna(f, op, T, A, 1, length(A.data))
mapreduce_impl_skipna(f, op::Base.AddFun, A::DataArray) = 
    mapreduce_pairwise_impl_skipna(f, op, A, 1, length(A.na.chunks),
                                   length(A.na)-countnz(A.na),
                                   max(128, Base.sum_pairwise_blocksize(f)))

## general mapreduce interface

function _mapreduce_skipna{T}(f, op, A::DataArray{T})
    n = length(A)
    na = A.na

    nna = countnz(na)
    nna == n && return Base.mr_empty(f, op, T)
    nna == n-1 && return Base.r_promote(op, evaluate(f, A.data[Base.findnextnot(na, 1)]))
    nna == 0 && return Base.mapreduce_impl(f, op, A.data, 1, n)

    mapreduce_impl_skipna(f, op, A)
end

# This is only safe when we can guarantee that if a function is passed
# NA, it returns NA. Otherwise we will fall back to the implementation
# in Base, which is slow because it's type-unstable, but guarantees the
# correct semantics
typealias SafeMapFuns Union(Base.IdFun, Base.AbsFun, Base.Abs2Fun,
                            Base.ExpFun, Base.LogFun, Base.CentralizedAbs2Fun)
typealias SafeReduceFuns Union(Base.AddFun, Base.MulFun, Base.MaxFun, Base.MinFun)
function Base._mapreduce(f::SafeMapFuns, op::SafeReduceFuns, A::DataArray)
    any(A.na) && return NA
    Base._mapreduce(f, op, A.data)
end

function Base.mapreduce(f, op::Function, A::DataArray; skipna::Bool=false)
    is(op, +) ? (skipna ? _mapreduce_skipna(f, Base.AddFun(), A) : Base._mapreduce(f, Base.AddFun(), A)) :
    is(op, *) ? (skipna ? _mapreduce_skipna(f, Base.MulFun(), A) : Base._mapreduce(f, Base.MulFun(), A)) :
    is(op, &) ? (skipna ? _mapreduce_skipna(f, Base.AndFun(), A) : Base._mapreduce(f, Base.AndFun(), A)) :
    is(op, |) ? (skipna ? _mapreduce_skipna(f, Base.OrFun(), A) : Base._mapreduce(f, Base.OrFun(), A)) :
    skipna ? _mapreduce_skipna(f, op, A) : Base._mapreduce(f, op, A)
end

Base.mapreduce(f, op, A::DataArray; skipna::Bool=false) =
    skipna ? _mapreduce_skipna(f, op, A) : Base._mapreduce(f, op, A)

Base.reduce(op, A::DataArray; skipna::Bool=false) =
    mapreduce(Base.IdFun(), op, A; skipna=skipna)

## usual reductions

for (fn, op) in ((:(Base.sum), Base.AddFun()),
                 (:(Base.prod), Base.MulFun()),
                 (:(Base.minimum), Base.MinFun()),
                 (:(Base.maximum), Base.MaxFun()))
    @eval begin
        $fn(f::Union(Function,Base.Func{1}), a::DataArray; skipna::Bool=false) =
            mapreduce(f, $op, a; skipna=skipna)
        $fn(a::DataArray; skipna::Bool=false) =
            mapreduce(Base.IdFun(), $op, a; skipna=skipna)
    end
end

for (fn, f, op) in ((:(Base.sumabs), Base.AbsFun(), Base.AddFun()),
                    (:(Base.sumabs2), Base.Abs2Fun(), Base.AddFun()))
    @eval $fn(a::DataArray; skipna::Bool=false) = mapreduce($f, $op, a; skipna=skipna)
end

## mean

Base.mean(a::DataArray; skipna::Bool=false) =
    sum(a; skipna=skipna) / (length(a.na)-(skipna ? countnz(a.na) : 0))

## variance

function Base.varm{T}(A::DataArray{T}, m::Number; corrected::Bool=true, skipna::Bool=false)
    if skipna
        n = length(A)
        na = A.na

        nna = countnz(na)
        nna == n && return convert(Base.momenttype(T), NaN)
        nna == n-1 && return convert(Base.momenttype(T),
                                     abs2(A.data[Base.findnextnot(na, 1)] - m)/(1 - int(corrected)))
        
        /(nna == 0 ? Base.centralize_sumabs2(A.data, m, 1, n) :
                     mapreduce_impl_skipna(Base.CentralizedAbs2Fun(m), Base.AddFun(), A), 
          n - nna - int(corrected))
    else
        any(A.na) && return NA
        Base.varm(A.data, m; corrected=corrected)
    end
end
Base.varm{T}(A::DataArray{T}, m::NAtype; corrected::Bool=true, skipna::Bool=false) = NA

function Base.varzm{T}(A::DataArray{T}; corrected::Bool=true, skipna::Bool=false)
    n = length(A)
    nna = skipna ? countnz(A.na) : 0
    (n == 0 || n == nna) && return convert(Base.momenttype(T), NaN)
    return Base.sumabs2(A; skipna=skipna) / (n - nna - int(corrected))
end

function Base.var(A::DataArray; corrected::Bool=true, mean=nothing, skipna::Bool=false)
    mean == 0 ? Base.varzm(A; corrected=corrected, skipna=skipna) :
    mean == nothing ? varm(A, Base.mean(A; skipna=skipna); corrected=corrected, skipna=skipna) :
    isa(mean, Union(Number, NAtype)) ? varm(A, mean; corrected=corrected, skipna=skipna) :
    error("Invalid value of mean.")
end

Base.stdm(A::DataArray, m::Number; corrected::Bool=true, skipna::Bool=false) = 
    sqrt(varm(A, m; corrected=corrected, skipna=skipna))

Base.std(A::DataArray; corrected::Bool=true, mean=nothing, skipna::Bool=false) = 
    sqrt(var(A; corrected=corrected, mean=mean, skipna=skipna))

## weighted mean

function Base.mean{W,V}(a::DataArray, w::WeightVec{W,V}; skipna::Bool=false)
    if skipna
        v = a .* w.values
        sum(v; skipna=true) / sum(DataArray(w.values, v.na); skipna=true)
    else
        anyna(a) ? NA : mean(a.data, w)
    end
end

function Base.mean{W,V<:DataArray}(a::DataArray, w::WeightVec{W,V}; skipna::Bool=false)
    if skipna
        v = a .* w.values
        sum(v; skipna=true) / sum(DataArray(w.values.data, v.na); skipna=true)
    else
        anyna(a) || anyna(w.values) ? NA : wsum(a.data, w.values.data) / w.sum
    end
end
