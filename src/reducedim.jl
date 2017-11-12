## Utility function

using Base.check_reducedims

# Determine if there are any true values in a BitArray in a given
# range. We use this for reductions with skipmissing=false along the first
# dimension.
function _any(B::BitArray, istart::Int, iend::Int)
    chunks = B.chunks
    startidx, startbit = Base.get_chunks_id(istart)
    endidx, endbit = Base.get_chunks_id(iend)
    startidx == endidx && return chunks[startidx] >> startbit << (63-endbit+startbit) != 0

    chunks[startidx] >> startbit != 0 && return true
    for i = startidx+1:endidx-1
        chunks[i] != 0 && return true
    end
    chunks[endidx] << (63-endbit) != 0 && return true
    return false
end

# Counts the number of ones in a given range. We use this for counting
# the values for mean and var with skipmissing=false along the first
# dimension.
function _count(B::BitArray, istart::Int, iend::Int)
    chunks = B.chunks
    startidx, startbit = Base.get_chunks_id(istart)
    endidx, endbit = Base.get_chunks_id(iend)
    startidx == endidx && return count_ones(chunks[startidx] >> startbit << (63-endbit+startbit))

    n = 0
    n += count_ones(chunks[startidx] >> startbit)
    for i = startidx+1:endidx-1
        n += count_ones(chunks[i])
    end
    n += count_ones(chunks[endidx] << (63-endbit))
    return n
end

## missing-preserving
@generated function _mapreducedim!(f::SafeMapFuns, op::SafeReduceFuns,
                                   R::DataArray, A::DataArray{T,N} where {T}) where {N}
    quote
        data = A.data
        na = A.na

        lsiz = check_reducedims(R, data)
        isempty(data) && return R

        if lsiz > 16
            # use mapreduce_impl, which is probably better tuned to achieve higher performance
            nslices = div(length(A), lsiz)
            ibase = 0
            extr = daextract(R)
            for i = 1:nslices
                if _any(na, ibase+1, ibase+lsiz)
                    unsafe_setmissing!(R, extr, i)
                else
                    v = Base.mapreduce_impl(f, op, data, ibase+1, ibase+lsiz)
                    @inbounds unsafe_dasetindex!(R, extr, v, i)
                end
                ibase += lsiz
            end
        else
            @nextract $N sizeR d->size(R,d)
            na_chunks = A.na.chunks

            new_data = R.data

            # If reducing to a DataArray, skip strides with missings.
            # In my benchmarks, it is actually faster to compute a new missing
            # array and BitArray it than to operate on the BitArray
            # directly.
            new_na = fill(false, size(new_data))

            @nexprs 1 d->(state_0 = state_{$N} = 1)
            @nexprs $N d->(skip_d = sizeR_d == 1)
            k = 1
            @nloops($N, i, A,
                d->(state_{d-1} = state_d),
                d->(skip_d || (state_d = state_0)), begin
                    @inbounds vna = new_na[state_0] | Base.unsafe_bitgetindex(na_chunks, k)
                    if vna
                        @inbounds new_na[state_0] = true
                    else
                        @inbounds x = data[k]
                        v = f(x)
                        @inbounds v0 = new_data[state_0]
                        nv = op(v0, v)
                        @inbounds new_data[state_0] = nv
                    end

                    state_0 += 1
                    k += 1
                end)

            R.na = BitArray(new_na)
        end
        return R
    end
end

## missing-preserving to array
@generated function _mapreducedim!(f::SafeMapFuns, op::SafeReduceFuns,
                                   R::AbstractArray, A::DataArray{T,N} where {T}) where {N}
    quote
        data = A.data
        na   = A.na

        lsiz = check_reducedims(R, data)
        isempty(data) && return R

        if lsiz > 16
            # use mapreduce_impl, which is probably better tuned to achieve higher performance
            nslices = div(length(A), lsiz)
            ibase = 0
            extr = daextract(R)
            for i = 1:nslices
                if _any(na, ibase+1, ibase+lsiz)
                    throw(MissingException("array contains missing values but output array does not support them"))
                else
                    v = Base.mapreduce_impl(f, op, data, ibase+1, ibase+lsiz)
                    @inbounds unsafe_dasetindex!(R, extr, v, i)
                end
                ibase += lsiz
            end
        else
            @nextract $N sizeR d->size(R,d)

            # If reducing to a non-DataArray, throw an error at the start on missing
            any(ismissing, A) && throw(MissingException("array contains missing values: pass skipmissing=true to skip them"))            
            @nloops $N i data d->(j_d = sizeR_d==1 ? 1 : i_d) begin
                @inbounds x = (@nref $N data i)
                v = f(x)
                @inbounds v0 = (@nref $N R j)
                nv = op(v0, v)
                @inbounds (@nref $N R j) = nv
            end
        end
        return R
    end
end
_mapreducedim!(f, op, R, A) = Base._mapreducedim!(f, op, R, A)

## missing-skipping
_getdata(A) = A
_getdata(A::DataArray) = A.data

# mapreduce across a dimension. If specified, C contains the number of
# non-missing values reduced into each element of R.
@generated function _mapreducedim_skipmissing_impl!(f, op, R::AbstractArray,
                                                        C::Union{Array{Int}, Void},
                                                        A::DataArray{T,N} where {T}) where {N}
    quote

        data = A.data
        na = A.na
        na_chunks = na.chunks
        new_data = _getdata(R)

        C === nothing || size(R) == size(C) || throw(DimensionMismatch("R and C must have same size"))
        lsiz = check_reducedims(new_data, data)
        isempty(data) && return R

        if lsiz > 16
            # keep the accumulator as a local variable when reducing along the first dimension
            nslices = div(length(A), lsiz)
            ibase = 0
            for i = 1:nslices
                # TODO: use pairwise impl for sum
                @inbounds v = new_data[i]
                @inbounds C !== nothing && (C[i] = lsiz - _count(na, ibase+1, ibase+lsiz))
                for k = ibase+1:ibase+lsiz
                    @inbounds Base.unsafe_bitgetindex(na_chunks, k) && continue
                    @inbounds x = data[k]
                    v = convert(typeof(v), op(f(x), v))::typeof(v)
                end
                @inbounds new_data[i] = v
                ibase += lsiz
            end
        else
            # general implementation
            @nextract $N sizeR d->size(new_data,d)
            @nexprs 1 d->(state_0 = state_{$N} = 1)
            @nexprs $N d->(skip_d = sizeR_d == 1)
            k = 1
            C !== nothing && fill!(C, div(length(A), length(R)))
            @nloops($N, i, A,
                d->(state_{d-1} = state_d),
                d->(skip_d || (state_d = state_0)), begin
                    @inbounds xna = Base.unsafe_bitgetindex(na_chunks, k)
                    if xna
                        C !== nothing && @inbounds C[state_0] -= 1
                    else
                        @inbounds x = data[k]
                        v = f(x)
                        @inbounds v0 = new_data[state_0]
                        nv = op(v0, v)
                        @inbounds new_data[state_0] = nv
                    end

                    state_0 += 1
                    k += 1
            end)
        end
        return R
    end
end

_mapreducedim_skipmissing!(f, op, R::AbstractArray, A::DataArray) =
    _mapreducedim_skipmissing_impl!(f, op, R, nothing, A)

# for MinFun/MaxFun, min or max is missing if all values along a dimension are missing
function _mapreducedim_skipmissing!(f, op::Union{typeof(min), typeof(max)}, R::DataArray, A::DataArray)
    R.na = BitArray(all!(fill(true, size(R)), A.na))
    _mapreducedim_skipmissing_impl!(f, op, R, nothing, A)
end
function _mapreducedim_skipmissing!(f, op::Union{typeof(min), typeof(max)}, R::AbstractArray, A::DataArray)
    if any(all!(fill(true, size(R)), A.na))
        throw(MissingException("some dimensions of the array only contain missing values"))
    end
    _mapreducedim_skipmissing_impl!(f, op, R, nothing, A)
end

## general reducedim interface

for op in (+, *, &, |, min, max)
    @eval begin
        function Base.initarray!{T}(a::DataArray{T}, op::typeof($op), init::Bool)
            if init
                Base.initarray!(a.data, op, true)
                Base.fill!(a.na, false)
            end
            a
        end
    end
end

function Base.reducedim_initarray(A::DataArray, region, v0, ::Type{R}) where R
    rd = length.(Base.reduced_indices(A.data, region))
    DataArray(fill!(similar(A.data, R, rd), v0), falses(rd))
end
function Base.reducedim_initarray0(A::DataArray, region, v0, ::Type{R}) where R
    rd = length.(Base.reduced_indices0(A,region))
    DataArray(fill!(similar(A.data, R, rd), v0), falses(rd))
end

function Base.mapreducedim!(f::Function, op, R::AbstractArray, A::DataArray;
                            skipmissing::Bool=false, skipna::Bool=false)
    if skipna && !skipmissing
        Base.depwarn("skipna=$skipna is deprecated, use skipmissing=$skipna instead", :mapreduce)
        skipmissing = true
    end
    skipmissing ? _mapreducedim_skipmissing!(f, op, R, A) : _mapreducedim!(f, op, R, A)
end
function Base.mapreducedim!(f, op, R::AbstractArray, A::DataArray;
                            skipmissing::Bool=false, skipna::Bool=false)
    if skipn && !skipmissing
        Base.depwarn("skipna=$skipna is deprecated, use skipmissing=$skipna instead", :mapreduce)
        skipmissing = true
    end
    skipmissing ? _mapreducedim_skipna!(f, op, R, A) : _mapreducedim!(f, op, R, A)
end
Base.reducedim!(op, R::DataArray{RT}, A::AbstractArray;
                skipmissing::Bool=false, skipna::Bool=false) where {RT} =
    Base.mapreducedim!(identity, op, R, A, zero(RT); skipna=skipna)

Base.mapreducedim(f, op, A::DataArray, region, v0;
                  skipmissing::Bool=false, skipna::Bool=false) =
    Base.mapreducedim!(f, op, Base.reducedim_initarray(A, region, v0), A;
                       skipmissing=skipmissing, skipna=skipna)
Base.mapreducedim(f, op, A::DataArray{T}, region;
                  skipmissing::Bool=false, skipna::Bool=false) where {T} =
    Base.mapreducedim!(f, op, Base.reducedim_init(f, op, A, region), A;
                       skipmissing=skipmissing, skipna=skipna)

Base.reducedim(op, A::DataArray, region, v0; skipna::Bool=false) =
    Base.mapreducedim(identity, op, A, region, v0; skipmissing=skipmissing, skipna=skipna)
Base.reducedim(op, A::DataArray, region; skipna::Bool=false) =
    Base.mapreducedim(identity, op, A, region; skipmissing=skipmissing, skipna=skipna)

## usual reductions

for (basfn, Op) in [(:sum, +), (:prod, *),
                    (:maximum, max), (:minimum, min),
                    (:all, &), (:any, |)]
    fname = Expr(:., :Base, Base.Meta.quot(basfn))
    fname! = Expr(:., :Base, Base.Meta.quot(Symbol(string(basfn, '!'))))
    @eval begin
        $(fname!)(f::Union{Function,$(supertype(typeof(abs)))}, r::AbstractArray, A::DataArray;
                  init::Bool=true, skipmissing::Bool=false, skipna::Bool=false) =
            Base.mapreducedim!(f, $(Op), Base.initarray!(r, $(Op), init), A;
                               skipmissing=skipmissing, skipna=skipna)
        $(fname!)(r::AbstractArray, A::DataArray;
                  init::Bool=true, skipmissing::Bool=false, skipna::Bool=false) =
            $(fname!)(identity, r, A; init=init, skipmissing=skipmissing, skipna=skipna)

        $(fname)(f::Union{Function,$(supertype(typeof(abs)))}, A::DataArray, region;
                 skipmissing::Bool=false, skipna::Bool=false) =
            Base.mapreducedim(f, $(Op), A, region; skipmissing=skipmissing, skipna=skipna)
        $(fname)(A::DataArray, region; skipmissing::Bool=false, skipna::Bool=false) =
            $(fname)(identity, A, region; skipmissing=skipmissing, skipna=skipna)
    end
end

for (basfn, fbase, Fun) in [(:sumabs, :sum, abs),
                            (:sumabs2, :sum, abs2),
                            (:maxabs, :maximum, abs),
                            (:minabs, :minimum, abs)]
    fname = Expr(:., :Base, Base.Meta.quot(basfn))
    fname! = Expr(:., :Base, Base.Meta.quot(Symbol(string(basfn, '!'))))
    fbase! = Expr(:., :Base, Base.Meta.quot(Symbol(string(fbase, '!'))))
    @eval begin
        $(fname!)(r::AbstractArray, A::DataArray;
                  init::Bool=true, skipmissing::Bool=false, skipna::Bool=false) =
            $(fbase!)($(Fun), r, A; init=init, skipmissing=skipmissing, skipna=skipna)
        $(fname)(A::DataArray, region; skipmissing::Bool=false, skipna::Bool=false) =
            $(fbase)($(Fun), A, region; skipmissing=skipmissing, skipna=skipna)
    end
end

## mean

function Base.mean!(R::AbstractArray{T}, A::DataArray;
                    skipmissing::Bool=false, skipna::Bool=false, init::Bool=true) where {T}
    init && fill!(R, 0)
    if skipna || skipmissing
        C = Array{Int}(size(R))
        _mapreducedim_skipmissing_impl!(identity, +, R, C, A)
        broadcast!(/, R, R, C)
    else
        sum!(R, A; skipmissing=false)
        broadcast!(/, R, R, convert(T, length(A)/length(R)))
        R
    end
end

Base.mean(A::DataArray{T}, region; skipmissing::Bool=false, skipna::Bool=false) where {T} =
    mean!(Base.reducedim_initarray(A, region, zero(Base.momenttype(T))), A;
          skipmissing=skipmissing, skipna=skipna, init=false)

## var

struct MapReduceDim2ArgHelperFun{F,T}
    f::F
    val::T
end
(f::MapReduceDim2ArgHelperFun)(x) = f.f(x, f.val)

# A version of _mapreducedim! that accepts an array S of the same size
# as R, the elements of which are passed as a second argument to f.
@generated function _mapreducedim_2arg!(f, op, R::DataArray,
                                                    A::DataArray{T,N} where {T},
                                                    S::AbstractArray) where {N}
    quote
        data = A.data
        na = A.na
        Sextr = daextract(S)

        lsiz = check_reducedims(R, data)
        size(R) == size(S) || throw(DimensionMismatch("R and S must have same size"))
        isempty(data) && return R

        if lsiz > 16
            # use mapreduce_impl, which is probably better tuned to achieve higher performance
            nslices = div(length(A), lsiz)
            ibase = 0
            extr = daextract(R)
            for i = 1:nslices
                if unsafe_ismissing(S, Sextr, i) || _any(na, ibase+1, ibase+lsiz)
                    unsafe_setmissing!(R, extr, i)
                else
                    @inbounds s = unsafe_getindex_notmissing(S, Sextr, i)
                    v = Base.mapreduce_impl(MapReduceDim2ArgHelperFun(f, s), op, data, ibase+1, ibase+lsiz)
                    @inbounds unsafe_dasetindex!(R, extr, v, i)
                end
                ibase += lsiz
            end
        else
            @nextract $N sizeR d->size(R,d)
            na_chunks = A.na.chunks
            new_data = R.data
            new_na = isa(S, DataArray) ? Array(S.na) : fill(false, size(S))

            @nexprs 1 d->(state_0 = state_{$N} = 1)
            @nexprs $N d->(skip_d = sizeR_d == 1)
            k = 1
            @nloops($N, i, A,
                d->(state_{d-1} = state_d),
                d->(skip_d || (state_d = state_0)), begin
                    @inbounds vna = new_na[state_0] | Base.unsafe_bitgetindex(na_chunks, k)
                    if vna
                        @inbounds new_na[state_0] = true
                    else
                        @inbounds s = unsafe_getindex_notmissing(S, Sextr, state_0)
                        @inbounds x = data[k]
                        v = f(x, s)
                        @inbounds v0 = new_data[state_0]
                        nv = op(v0, v)
                        @inbounds new_data[state_0] = nv
                    end

                    state_0 += 1
                    k += 1
                end)

            R.na = BitArray(new_na)
        end
        return R
    end
end

# A version of _mapreducedim_skipmissing! that accepts an array S of the same size
# as R, the elements of which are passed as a second argument to f.
@generated function _mapreducedim_skipmissing_2arg!(f, op, R::AbstractArray,
                                                        C::Union{Array{Int}, Void},
                                                        A::DataArray{T,N} where {T}, S::AbstractArray) where {N}
    quote
        data = A.data
        na = A.na
        na_chunks = na.chunks
        new_data = _getdata(R)
        Sextr = daextract(S)

        lsiz = check_reducedims(new_data, data)
        C === nothing || size(R) == size(C) || throw(DimensionMismatch("R and C must have same size"))
        size(R) == size(S) || throw(DimensionMismatch("R and S must have same size"))
        isempty(data) && return R
        @nextract $N sizeR d->size(new_data,d)
        sizA1 = size(data, 1)

        # If there are any missings in S, assume these will produce missings in R
        if isa(S, DataArray)
            copy!(R.na, S.na)
        end

        if lsiz > 16
            # keep the accumulator as a local variable when reducing along the first dimension
            nslices = div(length(A), lsiz)
            ibase = 0
            for i = 1:nslices
                @inbounds v = new_data[i]
                !isa(C, Void) && (C[i] = lsiz - _count(na, ibase+1, ibase+lsiz))

                # If S[i] is missing, skip this iteration
                @inbounds sna = unsafe_ismissing(S, Sextr, i)
                if !sna
                    @inbounds s = unsafe_getindex_notmissing(S, Sextr, i)
                    # TODO: use pairwise impl for sum
                    for k = ibase+1:ibase+lsiz
                        @inbounds Base.unsafe_bitgetindex(na_chunks, k) && continue
                        @inbounds x = data[k]
                        v = convert(typeof(v), op(f(x, s), v))::typeof(v)
                    end

                    @inbounds new_data[i] = v
                end

                ibase += lsiz
            end
        else
            # general implementation
            @nexprs 1 d->(state_0 = state_{$N} = 1)
            @nexprs $N d->(skip_d = sizeR_d == 1)
            k = 1
            !isa(C, Void) && fill!(C, div(length(A), length(R)))
            @nloops($N, i, A,
                d->(state_{d-1} = state_d),
                d->(skip_d || (state_d = state_0)), begin
                    @inbounds xna = Base.unsafe_bitgetindex(na_chunks, k) | unsafe_ismissing(S, Sextr, state_0)
                    if xna
                        !isa(C, Void) && @inbounds C[state_0] -= 1
                    else
                        @inbounds s = unsafe_getindex_notmissing(S, Sextr, state_0)
                        @inbounds x = data[k]
                        v = f(x, s)
                        @inbounds v0 = new_data[state_0]
                        nv = op(v0, v)
                        @inbounds new_data[state_0] = nv
                    end

                    state_0 += 1
                    k += 1
            end)
        end
        return R
    end
end

struct Abs2MinusFun end
(::Abs2MinusFun)(x, m) = abs2(x - m)

function Base.varm!(R::AbstractArray, A::DataArray, m::AbstractArray; corrected::Bool=true,
                    skipmissing::Bool=false, skipna::Bool=false, init::Bool=true)
    if skipna && !skipmissing
            Base.depwarn("skipna=$skipna is deprecated, use skipmissing=$skipna instead", :mapreduce)
            skipmissing = true
    end
    if isempty(A)
        fill!(R, convert(eltype(R), NaN))
    else
        init && fill!(R, zero(eltype(R)))
        if skipmissing
            C = Array{Int}(size(R))

            # Compute R = abs2(A-m)
            _mapreducedim_skipmissing_2arg!(Abs2MinusFun(), +, R, C, A, m)

            # Divide by number of non-missing values
            if corrected
                for i = 1:length(C)
                    @inbounds C[i] = max(C[i] - 1, 0)
                end
            end
            broadcast!(/, R, R, C)
        else
            # Compute R = abs2(A-m)
            _mapreducedim_2arg!(Abs2MinusFun(), +, R, A, m)

            # Divide by number of values
            broadcast!(/, R, R, div(length(A), length(R)) - corrected)
        end
    end
end

Base.varm(A::DataArray{T}, m::AbstractArray, region; corrected::Bool=true,
          skipmissing::Bool=false, skipna::Bool=false) where {T} =
    Base.varm!(Base.reducedim_initarray(A, region, zero(Base.momenttype(T))), A, m;
               corrected=corrected, skipmissing=skipmissing, skipna=skipna, init=false)

function Base.var(A::DataArray{T}, region::Union{Integer, AbstractArray, Tuple};
                  corrected::Bool=true, mean=nothing,
                  skipmissing::Bool=false, skipna::Bool=false) where T
    if mean == 0
        Base.varm(A, Base.reducedim_initarray(A, region, zero(Base.momenttype(T))), region;
                  corrected=corrected, skipmissing=skipmissing, skipna=skipna)
    elseif mean == nothing
        if skipna || skipmissing
            # Can reduce mean into ordinary array
            m = zeros(Base.momenttype(T), length.(Base.reduced_indices(A, region)))
            Base.varm(A, Base.mean!(m, A; skipmissing=skipmissing, skipna=skipna), region;
                      corrected=corrected, skipmissing=skipmissing, skipna=skipna)
        else
            Base.varm(A, Base.mean(A, region; skipmissing=skipmissing, skipna=skipna), region;
                      corrected=corrected, skipmissing=skipmissing, skipna=skipna)
        end
    elseif isa(mean, AbstractArray)
        Base.varm(A, mean::AbstractArray, region;
                  corrected=corrected, skipmissing=skipmissing, skipna=skipna)
    else
        throw(ErrorException("invalid value of mean"))
    end
end
