## mapreduce implementation that skips missing

function skipmissing_init(f, op, na::BitArray, data::Array, ifirst::Int, ilast::Int)
    # Get first non-missing element
    ifirst = Base.findnextnot(na, ifirst)
    @inbounds d1 = data[ifirst]

    # Get next non-missing element
    ifirst = Base.findnextnot(na, ifirst+1)
    @inbounds d2 = data[ifirst]

    # Reduce first two elements
    (op(f(d1), f(d2)), ifirst)
end

function mapreduce_seq_impl_skipmissing(f, op, T, A::DataArray, ifirst::Int, ilast::Int)
    data = A.data
    na = A.na
    chunks = na.chunks

    v, i = skipmissing_init(f, op, na, data, ifirst, ilast)

    while i < ilast
        i += 1
        @inbounds na = Base.unsafe_bitgetindex(chunks, i)
        na && continue
        @inbounds d = data[i]
        v = op(v, f(d))
    end
    v
end

# Pairwise map-reduce
function mapreduce_pairwise_impl_skipmissing(f, op, A::DataArray{T}, bytefirst::Int, bytelast::Int, n_notna::Int, blksize::Int) where T
    if n_notna <= blksize
        ifirst = 64*(bytefirst-1)+1
        ilast = min(64*bytelast, length(A))
        # Fall back to Base implementation if no missings in block
        return ilast - ifirst + 1 == n_notna ? Base.mapreduce_seq_impl(f, op, A.data, ifirst, ilast) :
                                               mapreduce_seq_impl_skipmissing(f, op, T, A, ifirst, ilast)
    end

    # Find byte in the middle of range
    # The block size is restricted so that there will always be at
    # least two non-missing elements in the returned range
    chunks = A.na.chunks
    nmid = 0
    imid = bytefirst-1
    while nmid < (n_notna >> 1)
        imid += 1
        @inbounds nmid += count_zeros(chunks[imid])
    end

    v1 = mapreduce_pairwise_impl_skipmissing(f, op, A, bytefirst, imid, nmid, blksize)
    v2 = mapreduce_pairwise_impl_skipmissing(f, op, A, imid+1, bytelast, n_notna-nmid, blksize)
    op(v1, v2)
end

if isdefined(Base, :pairwise_blocksize)
    sum_pairwise_blocksize(f) = Base.pairwise_blocksize(f, +)
else
    const sum_pairwise_blocksize = Base.sum_pairwise_blocksize
end

mapreduce_impl_skipmissing(f, op, A::DataArray{T}) where {T} =
    mapreduce_seq_impl_skipmissing(f, op, T, A, 1, length(A.data))
mapreduce_impl_skipmissing(f, op::typeof(+), A::DataArray) =
    mapreduce_pairwise_impl_skipmissing(f, op, A, 1, length(A.na.chunks),
                                     length(A.na)-countnz(A.na),
                                     max(128, sum_pairwise_blocksize(f)))

## general mapreduce interface

function _mapreduce_skipmissing(f, op, A::DataArray{T}) where T
    n = length(A)
    na = A.na

    nna = countnz(na)
    nna == n && return Base.mr_empty(f, op, T)
    nna == n-1 && return Base.r_promote(op, f(A.data[Base.findnextnot(na, 1)]))
    nna == 0 && return Base.mapreduce_impl(f, op, A.data, 1, n)

    mapreduce_impl_skipmissing(f, op, A)
end

# This is only safe when we can guarantee that if a function is passed
# missing, it returns missing. Otherwise we will fall back to the implementation
# in Base, which is slow because it's type-unstable, but guarantees the
# correct semantics
const SafeMapFuns = Union{typeof(identity), typeof(abs), typeof(abs2),
                            typeof(exp), typeof(log), typeof(Base.centralizedabs2fun)}
const SafeReduceFuns = Union{typeof(+), typeof(*), typeof(max), typeof(min)}
function Base._mapreduce(f::SafeMapFuns, op::SafeReduceFuns, A::DataArray)
    any(A.na) && return missing
    Base._mapreduce(f, op, A.data)
end

function Base.mapreduce(f, op::Function, A::DataArray;
                        skipmissing::Bool=false, skipna::Bool=false)
    if skipna && !skipmissing
        Base.depwarn("skipna=$skipna is deprecated, use skipmissing=$skipna instead", :mapreduce)
        skipmissing = true
    end
    (op === +) ? (skipmissing ? _mapreduce_skipmissing(f, +, A) : Base._mapreduce(f, +, A)) :
    (op === *) ? (skipmissing ? _mapreduce_skipmissing(f, *, A) : Base._mapreduce(f, *, A)) :
    (op === &) ? (skipmissing ? _mapreduce_skipmissing(f, &, A) : Base._mapreduce(f, &, A)) :
    (op === |) ? (skipmissing ? _mapreduce_skipmissing(f, |, A) : Base._mapreduce(f, |, A)) :
    skipmissing ? _mapreduce_skipmissing(f, op, A) : Base._mapreduce(f, op, A)
end

# To silence deprecations, but could be more efficient
function Base.mapreduce(f, op::Union{typeof(|), typeof(&)}, A::DataArray;
                        skipmissing::Bool=false, skipna::Bool=false)
    if skipna && !skipmissing
        Base.depwarn("skipna=$skipna is deprecated, use skipmissing=$skipna instead", :mapreduce)
        skipmissing = true
    end
    skipmissing ? _mapreduce_skipmissing(f, op, A) : Base._mapreduce(f, op, A)
end

function Base.mapreduce(f, op, A::DataArray;
                        skipmissing::Bool=false, skipna::Bool=false)
    if skipna && !skipmissing
        Base.depwarn("skipna=$skipna is deprecated, use skipmissing=$skipna instead", :mapreduce)
        skipmissing = true
    end
    skipmissing ? _mapreduce_skipmissing(f, op, A) : Base._mapreduce(f, op, A)
end

Base.reduce(op, A::DataArray;
            skipmissing::Bool=false, skipna::Bool=false) =
    mapreduce(identity, op, A; skipmissing=skipmissing, skipna=skipna)

## usual reductions

for (fn, op) in ((:(Base.sum), +),
                 (:(Base.prod), *),
                 (:(Base.minimum), min),
                 (:(Base.maximum), max))
    @eval begin
        $fn(f::Union{Function,$(supertype(typeof(abs)))}, a::DataArray;
            skipmissing::Bool=false, skipna::Bool=false) =
            mapreduce(f, $op, a; skipmissing=skipmissing, skipna=skipna)
        $fn(a::DataArray; skipmissing::Bool=false, skipna::Bool=false) =
            mapreduce(identity, $op, a; skipmissing=skipmissing, skipna=skipna)
    end
end

for (fn, f, op) in ((:(Base.sumabs), abs, +),
                    (:(Base.sumabs2), abs2, +))
    @eval $fn(a::DataArray; skipmissing::Bool=false, skipna::Bool=false) =
        mapreduce($f, $op, a; skipmissing=skipmissing, skipna=skipna)
end

## mean

Base.mean(a::DataArray; skipmissing::Bool=false, skipna::Bool=false) =
    sum(a; skipmissing=skipmissing, skipna=skipna) / (length(a.na)-(skipna || skipmissing ? countnz(a.na) : 0))

## variance

function Base.varm(A::DataArray{T}, m::Number;
                   corrected::Bool=true, skipmissing::Bool=false, skipna::Bool=false) where T
    if skipna || skipmissing
        if skipna && !skipmissing
            Base.depwarn("skipna=$skipna is deprecated, use skipmissing=$skipna instead", :mapreduce)
        end

        n = length(A)
        na = A.na

        nna = countnz(na)
        nna == n && return convert(Base.momenttype(T), NaN)
        nna == n-1 && return convert(Base.momenttype(T),
                                     abs2(A.data[Base.findnextnot(na, 1)] - m)/(1 - corrected))

        /(nna == 0 ? Base.centralize_sumabs2(A.data, m, 1, n) :
                     mapreduce_impl_skipmissing(Base.centralizedabs2fun(m), +, A),
          n - nna - corrected)
    else
        any(A.na) && return missing
        Base.varm(A.data, m; corrected=corrected)
    end
end
Base.varm(A::DataArray{T}, m::Missing;
          corrected::Bool=true, skipmissing::Bool=false, skipna::Bool=false) where {T} = missing

function Base.var(A::DataArray;
                  corrected::Bool=true, mean=nothing, skipmissing::Bool=false, skipna::Bool=false)
    mean == 0 ? Base.varm(A, 0; corrected=corrected, skipmissing=skipmissing, skipna=skipna) :
    mean == nothing ? varm(A, Base.mean(A; skipmissing=skipmissing, skipna=skipna);
                           corrected=corrected, skipmissing=skipmissing, skipna=skipna) :
    isa(mean, Union{Number,Missing}) ?
        varm(A, mean; corrected=corrected, skipmissing=skipmissing, skipna=skipna) :
        throw(ErrorException("Invalid value of mean."))
end

Base.stdm(A::DataArray, m::Number;
          corrected::Bool=true, skipmissing::Bool=false, skipna::Bool=false) =
    sqrt(varm(A, m; corrected=corrected, skipmissing=skipmissing, skipna=skipna))

Base.std(A::DataArray;
         corrected::Bool=true, mean=nothing, skipmissing::Bool=false, skipna::Bool=false) =
    sqrt(var(A; corrected=corrected, mean=mean, skipmissing=skipmissing, skipna=skipna))

## weighted mean

function Base.mean(a::DataArray, w::Weights; skipmissing::Bool=false, skipna::Bool=false)
    if skipna || skipmissing
        v = a .* w.values
        sum(v; skipmissing=true) / sum(DataArray(w.values, v.na); skipmissing=true)
    else
        any(ismissing, a) ? missing : mean(a.data, w)
    end
end

function Base.mean(a::DataArray, w::Weights{W,V};
                   skipmissing::Bool=false, skipna::Bool=false) where {W,V<:DataArray}
    if skipna || skipmissing
        v = a .* w.values
        sum(v; skipmissing=true) / sum(DataArray(w.values.data, v.na); skipmissing=true)
    else
        any(ismissing, a) || any(ismissing, w.values) ? missing : wsum(a.data, w.values.data) / w.sum
    end
end
