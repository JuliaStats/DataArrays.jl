## Utility function

# This is a substantially faster implementation of the "all" reduction
# across dimensions for reducing a BitArray to an Array{Bool}. We use 
# this below for implementing MaxFun and MinFun with skipna=true.
@ngenerate N typeof(R) function Base._mapreducedim!{N}(f, op::Base.AndFun, R::Array{Bool}, A::BitArray{N})
    lsiz = Base.check_reducdims(R, A)
    isempty(A) && return R
    @nextract N sizeR d->size(R, d)
    @nexprs 1 d->(state_0 = state_{N} = 1)
    @nexprs N d->(skip_d = sizeR_d == 1)
    Achunks = A.chunks
    k = 1
    @nloops(N, i, A,
        d->(state_{d-1} = state_d),
        d->(skip_d || (state_d = state_0)),
        begin
            @inbounds R[state_0] &= evaluate(f, Base.unsafe_bitgetindex(Achunks, k))
            state_0 += 1
            k += 1
        end)
    R
end

# Determine if there are any true values in a BitArray in a given
# range. We use this for reductions with skipna=false along the first
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
# the values for mean and var with skipna=false along the first
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

## NA-preserving
@ngenerate N typeof(R) function _mapreducedim!{T,N}(f::SafeMapFuns, op::SafeReduceFuns,
                                                    R::DataArray, A::DataArray{T,N})
    data = A.data
    na = A.na

    lsiz = Base.check_reducdims(R, data)
    isempty(data) && return R

    if lsiz > 16
        # use mapreduce_impl, which is probably better tuned to achieve higher performance
        nslices = div(length(A), lsiz)
        ibase = 0
        extr = daextract(R)
        for i = 1:nslices
            if _any(na, ibase+1, ibase+lsiz)
                unsafe_setna!(R, extr, i)
            else
                v = Base.mapreduce_impl(f, op, data, ibase+1, ibase+lsiz)
                @inbounds unsafe_dasetindex!(R, extr, v, i)
            end
            ibase += lsiz
        end
    else
        @nextract N sizeR d->size(R,d)
        na_chunks = A.na.chunks

        new_data = R.data

        # If reducing to a DataArray, skip strides with NAs.
        # In my benchmarks, it is actually faster to compute a new NA
        # array and bitpack it than to operate on the BitArray
        # directly.
        new_na = fill(false, size(new_data))

        @nexprs 1 d->(state_0 = state_{N} = 1)
        @nexprs N d->(skip_d = sizeR_d == 1)
        k = 1
        @nloops(N, i, A,
            d->(state_{d-1} = state_d),
            d->(skip_d || (state_d = state_0)), begin
                @inbounds vna = new_na[state_0] | Base.unsafe_bitgetindex(na_chunks, k)
                if vna
                    @inbounds new_na[state_0] = true
                else
                    @inbounds x = data[k]
                    v = evaluate(f, x)
                    @inbounds v0 = new_data[state_0]
                    nv = evaluate(op, v0, v)
                    @inbounds new_data[state_0] = nv
                end

                state_0 += 1
                k += 1
            end)

        R.na = bitpack(new_na)
    end
    return R
end

## NA-preserving to array
@ngenerate N typeof(R) function _mapreducedim!{T,N}(f::SafeMapFuns, op::SafeReduceFuns,
                                                    R::AbstractArray, A::DataArray{T,N})
    data = A.data
    na = A.na

    lsiz = Base.check_reducdims(R, data)
    isempty(data) && return R

    if lsiz > 16
        # use mapreduce_impl, which is probably better tuned to achieve higher performance
        nslices = div(length(A), lsiz)
        ibase = 0
        extr = daextract(R)
        for i = 1:nslices
            if _any(na, ibase+1, ibase+lsiz)
                error("cannot reduce a DataArray containing NAs to an AbstractArray")
            else
                v = Base.mapreduce_impl(f, op, data, ibase+1, ibase+lsiz)
                @inbounds unsafe_dasetindex!(R, extr, v, i)
            end
            ibase += lsiz
        end
    else
        @nextract N sizeR d->size(R,d)

        # If reducing to a non-DataArray, throw an error at the start on NA
        any(isna(A)) && error("cannot reduce a DataArray containing NAs to an AbstractArray")
        @nloops N i data d->(j_d = sizeR_d==1 ? 1 : i_d) begin
            @inbounds x = (@nref N data i)
            v = evaluate(f, x)
            @inbounds v0 = (@nref N R j)
            nv = evaluate(op, v0, v)
            @inbounds (@nref N R j) = nv
        end
    end
    return R
end
_mapreducedim!(f, op, R, A) = Base._mapreducedim!(f, op, R, A)

## NA-skipping
_getdata(A) = A
_getdata(A::DataArray) = A.data

# mapreduce across a dimension. If specified, C contains the number of
# non-NA values reduced into each element of R.
@ngenerate N typeof(R) function _mapreducedim_skipna_impl!{T,N}(f, op, R::AbstractArray,
                                                                C::Union(Array{Int}, Nothing),
                                                                A::DataArray{T,N})
    data = A.data
    na = A.na
    na_chunks = na.chunks
    new_data = _getdata(R)

    isa(C, Nothing) || size(R) == size(C) || error("R and C must have same size")
    lsiz = Base.check_reducdims(new_data, data)
    isempty(data) && return R

    if lsiz > 16
        # keep the accumulator as a local variable when reducing along the first dimension
        nslices = div(length(A), lsiz)
        ibase = 0
        for i = 1:nslices
            # TODO: use pairwise impl for sum
            @inbounds v = new_data[i]
            @inbounds !isa(C, Nothing) && (C[i] = lsiz - _count(na, ibase+1, ibase+lsiz))
            for k = ibase+1:ibase+lsiz
                @inbounds Base.unsafe_bitgetindex(na_chunks, k) && continue
                @inbounds x = data[k]
                v = convert(typeof(v), evaluate(op, evaluate(f, x), v))::typeof(v)
            end
            @inbounds new_data[i] = v
            ibase += lsiz
        end
    else
        # general implementation
        @nextract N sizeR d->size(new_data,d)
        @nexprs 1 d->(state_0 = state_{N} = 1)
        @nexprs N d->(skip_d = sizeR_d == 1)
        k = 1
        !isa(C, Nothing) && fill!(C, div(length(A), length(R)))
        @nloops(N, i, A,
            d->(state_{d-1} = state_d),
            d->(skip_d || (state_d = state_0)), begin
                @inbounds xna = Base.unsafe_bitgetindex(na_chunks, k)
                if xna
                    !isa(C, Nothing) && @inbounds C[state_0] -= 1
                else
                    @inbounds x = data[k]
                    v = evaluate(f, x)
                    @inbounds v0 = new_data[state_0]
                    nv = evaluate(op, v0, v)
                    @inbounds new_data[state_0] = nv
                end

                state_0 += 1
                k += 1
        end)
    end
    return R    
end

_mapreducedim_skipna!(f, op, R::AbstractArray, A::DataArray) = 
    _mapreducedim_skipna_impl!(f, op, R, nothing, A)

# for MinFun/MaxFun, min or max is NA if all values along a dimension are NA
function _mapreducedim_skipna!(f, op::Union(Base.MinFun, Base.MaxFun), R::DataArray, A::DataArray)
    R.na = bitpack(all!(fill(true, size(R)), A.na))
    _mapreducedim_skipna_impl!(f, op, R, nothing, A)
end
function _mapreducedim_skipna!(f, op::Union(Base.MinFun, Base.MaxFun), R::AbstractArray, A::DataArray)
    if any(all!(fill(true, size(R)), A.na))
        error("all values along specified dimension are NA for one element of reduced dimension; cannot reduce to non-DataArray")
    end
    _mapreducedim_skipna_impl!(f, op, R, nothing, A)
end

## general reducedim interface

for op in (Base.AddFun, Base.MulFun, Base.AndFun, Base.OrFun, Base.MinFun, Base.MaxFun)
    @eval begin
        function Base.initarray!{T}(a::DataArray{T}, op::$op, init::Bool)
            if init
                Base.initarray!(a.data, op, true)
                Base.fill!(a.na, false)
            end
            a
        end
    end
end

function Base.reducedim_initarray{R}(A::DataArray, region, v0, ::Type{R})
    rd = Base.reduced_dims(A.data, region)
    DataArray(fill!(similar(A.data, R, rd), v0), falses(rd))
end
function Base.reducedim_initarray0{R}(A::DataArray, region, v0, ::Type{R})
    rd = Base.reduced_dims0(A,region)
    DataArray(fill!(similar(A.data, R, rd), v0), falses(rd))
end

function Base.mapreducedim!(f::Function, op, R::AbstractArray, A::DataArray; skipna::Bool=false)
    is(op, +) ? (skipna ? _mapreducedim_skipna!(f, Base.AddFun(), R, A) : _mapreducedim!(f, Base.AddFun(), R, A)) :
    is(op, *) ? (skipna ? _mapreducedim_skipna!(f, Base.MulFun(), R, A) : _mapreducedim!(f, Base.MulFun(), R, A)) :
    is(op, &) ? (skipna ? _mapreducedim_skipna!(f, Base.AndFun(), R, A) : _mapreducedim!(f, Base.AndFun(), R, A)) :
    is(op, |) ? (skipna ? _mapreducedim_skipna!(f, Base.OrFun(), R, A) : _mapreducedim!(f, Base.OrFun(), R, A)) :
    skipna ? _mapreducedim_skipna!(f, op, R, A) : _mapreducedim!(f, op, R, A)
end
Base.mapreducedim!(f, op, R::AbstractArray, A::DataArray; skipna::Bool=false) =
    skipna ? _mapreducedim_skipna!(f, op, R, A) : _mapreducedim!(f, op, R, A)
Base.reducedim!{RT}(op, R::DataArray{RT}, A::AbstractArray; skipna::Bool=false) =
    Base.mapreducedim!(Base.IdFun(), op, R, A, zero(RT); skipna=skipna)

Base.mapreducedim(f, op, A::DataArray, region, v0; skipna::Bool=false) =
    Base.mapreducedim!(f, op, Base.reducedim_initarray(A, region, v0), A; skipna=skipna)
Base.mapreducedim{T}(f, op, A::DataArray{T}, region; skipna::Bool=false) =
    Base.mapreducedim!(f, op, Base.reducedim_init(f, op, A, region), A; skipna=skipna)

Base.reducedim(op, A::DataArray, region, v0; skipna::Bool=false) =
    Base.mapreducedim(Base.IdFun(), op, A, region, v0; skipna=skipna)
Base.reducedim(op, A::DataArray, region; skipna::Bool=false) =
    Base.mapreducedim(Base.IdFun(), op, A, region; skipna=skipna)

## usual reductions

for (basfn, Op) in [(:sum, Base.AddFun), (:prod, Base.MulFun), 
                    (:maximum, Base.MaxFun), (:minimum, Base.MinFun), 
                    (:all, Base.AndFun), (:any, Base.OrFun)]
    fname = Expr(:., :Base, Base.Meta.quot(basfn))
    fname! = Expr(:., :Base, Base.Meta.quot(symbol(string(basfn, '!'))))
    @eval begin
        $(fname!)(f::Union(Function,Base.Func{1}), r::AbstractArray, A::DataArray;
                  init::Bool=true, skipna::Bool=false) = 
            Base.mapreducedim!(f, $(Op)(), Base.initarray!(r, $(Op)(), init), A; skipna=skipna)
        $(fname!)(r::AbstractArray, A::DataArray; init::Bool=true, skipna::Bool=false) =
            $(fname!)(Base.IdFun(), r, A; init=init, skipna=skipna)

        $(fname)(f::Union(Function,Base.Func{1}), A::DataArray, region; skipna::Bool=false) = 
            Base.mapreducedim(f, $(Op)(), A, region; skipna=skipna)
        $(fname)(A::DataArray, region; skipna::Bool=false) =
            $(fname)(Base.IdFun(), A, region; skipna=skipna)
    end
end

for (basfn, fbase, Fun) in [(:sumabs, :sum, Base.AbsFun), 
                            (:sumabs2, :sum, Base.Abs2Fun), 
                            (:maxabs, :maximum, Base.AbsFun), 
                            (:minabs, :minimum, Base.AbsFun)]
    fname = Expr(:., :Base, Base.Meta.quot(basfn))
    fname! = Expr(:., :Base, Base.Meta.quot(symbol(string(basfn, '!'))))
    fbase! = Expr(:., :Base, Base.Meta.quot(symbol(string(fbase, '!'))))
    @eval begin 
        $(fname!)(r::AbstractArray, A::DataArray; init::Bool=true, skipna::Bool=false) = 
            $(fbase!)($(Fun)(), r, A; init=init, skipna=skipna)
        $(fname)(A::DataArray, region; skipna::Bool=false) =
            $(fbase)($(Fun)(), A, region; skipna=skipna)
    end
end

## mean

function Base.mean!{T}(R::AbstractArray{T}, A::DataArray; skipna::Bool=false,
                       init::Bool=true)
    init && fill!(R, zero(eltype(R)))
    if skipna
        C = Array(Int, size(R))
        _mapreducedim_skipna_impl!(Base.IdFun(), Base.AddFun(), R, C, A)
        broadcast!(/, R, R, C)
    else
        sum!(R, A; skipna=false)
        broadcast!(/, R, R, convert(T, length(A)/length(R)))
        R
    end
end

Base.mean{T}(A::DataArray{T}, region; skipna::Bool=false) =
    mean!(Base.reducedim_initarray(A, region, zero(Base.momenttype(T))), A; skipna=skipna,
          init=false)

## var

immutable MapReduceDim2ArgHelperFun{F,T}
    f::F
    val::T
end
if VERSION < v"0.4.0-dev+1274"
    evaluate(f::MapReduceDim2ArgHelperFun, x) = evaluate(f.f, x, f.val)
else
    Base.call(f::MapReduceDim2ArgHelperFun, x) = f.f(x, f.val)
end

# A version of _mapreducedim! that accepts an array S of the same size
# as R, the elements of which are passed as a second argument to f.
@ngenerate N typeof(R) function _mapreducedim_2arg!{T,N}(f, op, R::DataArray,
                                                         A::DataArray{T,N},
                                                         S::AbstractArray)
    data = A.data
    na = A.na
    Sextr = daextract(S)

    lsiz = Base.check_reducdims(R, data)
    size(R) == size(S) || error("R and S must have same size")
    isempty(data) && return R

    if lsiz > 16
        # use mapreduce_impl, which is probably better tuned to achieve higher performance
        nslices = div(length(A), lsiz)
        ibase = 0
        extr = daextract(R)
        for i = 1:nslices
            if unsafe_isna(S, Sextr, i) || _any(na, ibase+1, ibase+lsiz)
                unsafe_setna!(R, extr, i)
            else
                @inbounds s = unsafe_getindex_notna(S, Sextr, i)
                v = Base.mapreduce_impl(MapReduceDim2ArgHelperFun(f, s), op, data, ibase+1, ibase+lsiz)
                @inbounds unsafe_dasetindex!(R, extr, v, i)
            end
            ibase += lsiz
        end
    else
        @nextract N sizeR d->size(R,d)
        na_chunks = A.na.chunks
        new_data = R.data
        new_na = isa(S, DataArray) ? bitunpack(S.na) : fill(false, size(S))

        @nexprs 1 d->(state_0 = state_{N} = 1)
        @nexprs N d->(skip_d = sizeR_d == 1)
        k = 1
        @nloops(N, i, A,
            d->(state_{d-1} = state_d),
            d->(skip_d || (state_d = state_0)), begin
                @inbounds vna = new_na[state_0] | Base.unsafe_bitgetindex(na_chunks, k)
                if vna
                    @inbounds new_na[state_0] = true
                else
                    @inbounds s = unsafe_getindex_notna(S, Sextr, state_0)
                    @inbounds x = data[k]
                    v = evaluate(f, x, s)
                    @inbounds v0 = new_data[state_0]
                    nv = evaluate(op, v0, v)
                    @inbounds new_data[state_0] = nv
                end

                state_0 += 1
                k += 1
            end)

        R.na = bitpack(new_na)
    end
    return R
end

# A version of _mapreducedim_skipna! that accepts an array S of the same size
# as R, the elements of which are passed as a second argument to f.
@ngenerate N typeof(R) function _mapreducedim_skipna_2arg!{T,N}(f, op, R::AbstractArray,
                                                                C::Union(Array{Int}, Nothing),
                                                                A::DataArray{T,N}, S::AbstractArray)
    data = A.data
    na = A.na
    na_chunks = na.chunks
    new_data = _getdata(R)
    Sextr = daextract(S)

    lsiz = Base.check_reducdims(new_data, data)
    isa(C, Nothing) || size(R) == size(C) || error("R and C must have same size")
    size(R) == size(S) || error("R and S must have same size")
    isempty(data) && return R
    @nextract N sizeR d->size(new_data,d)
    sizA1 = size(data, 1)

    # If there are any NAs in S, assume these will produce NAs in R
    if isa(S, DataArray)
        copy!(R.na, S.na)
    end

    if lsiz > 16
        # keep the accumulator as a local variable when reducing along the first dimension
        nslices = div(length(A), lsiz)
        ibase = 0
        for i = 1:nslices
            @inbounds v = new_data[i]
            !isa(C, Nothing) && (C[i] = lsiz - _count(na, ibase+1, ibase+lsiz))

            # If S[i] is NA, skip this iteration
            @inbounds sna = unsafe_isna(S, Sextr, i)
            if !sna
                @inbounds s = unsafe_getindex_notna(S, Sextr, i)
                # TODO: use pairwise impl for sum
                for k = ibase+1:ibase+lsiz
                    @inbounds Base.unsafe_bitgetindex(na_chunks, k) && continue
                    @inbounds x = data[k]
                    v = convert(typeof(v), evaluate(op, evaluate(f, x, s), v))::typeof(v)
                end

                @inbounds new_data[i] = v
            end

            ibase += lsiz
        end
    else
        # general implementation
        @nexprs 1 d->(state_0 = state_{N} = 1)
        @nexprs N d->(skip_d = sizeR_d == 1)
        k = 1
        !isa(C, Nothing) && fill!(C, div(length(A), length(R)))
        @nloops(N, i, A,
            d->(state_{d-1} = state_d),
            d->(skip_d || (state_d = state_0)), begin
                @inbounds xna = Base.unsafe_bitgetindex(na_chunks, k) | unsafe_isna(S, Sextr, state_0)
                if xna
                    !isa(C, Nothing) && @inbounds C[state_0] -= 1
                else
                    @inbounds s = unsafe_getindex_notna(S, Sextr, state_0)
                    @inbounds x = data[k]
                    v = evaluate(f, x, s)
                    @inbounds v0 = new_data[state_0]
                    nv = evaluate(op, v0, v)
                    @inbounds new_data[state_0] = nv
                end

                state_0 += 1
                k += 1
        end)
    end
    return R    
end

immutable Abs2MinusFun end
if VERSION < v"0.4.0-dev+1274"
    evaluate(f::Abs2MinusFun, x, m) = abs2(x - m)
else
    Base.call(f::Abs2MinusFun, x, m) = abs2(x - m)
end

function Base.varm!(R::AbstractArray, A::DataArray, m::AbstractArray; corrected::Bool=true,
                    skipna::Bool=false, init::Bool=true)
    if isempty(A)
        fill!(R, convert(eltype(R), NaN))
    else
        init && fill!(R, zero(eltype(R)))
        if skipna
            C = Array(Int, size(R))

            # Compute R = abs2(A-m)
            _mapreducedim_skipna_2arg!(Abs2MinusFun(), Base.AddFun(), R, C, A, m)

            # Divide by number of non-NA values
            if corrected
                for i = 1:length(C)
                    @inbounds C[i] = max(C[i] - 1, 0)
                end
            end
            broadcast!(/, R, R, C)
        else
            # Compute R = abs2(A-m)
            _mapreducedim_2arg!(Abs2MinusFun(), Base.AddFun(), R, A, m)

            # Divide by number of values
            broadcast!(/, R, R, div(length(A), length(R)) - int(corrected))
        end
    end
end

function Base.varzm!(R::AbstractArray, A::DataArray; corrected::Bool=true,
                     skipna::Bool=false, init::Bool=true)
    if isempty(A)
        fill!(R, convert(eltype(R), NaN))
    else
        init && fill!(R, zero(eltype(R)))
        if skipna
            C = Array(Int, size(R))
            _mapreducedim_skipna_impl!(Base.Abs2Fun(), Base.AddFun(), R, C, A)
            if corrected
                for i = 1:length(C)
                    @inbounds C[i] = max(C[i] - 1, 0)
                end
            end
            broadcast!(/, R, R, C)
        else
            Base.sumabs2!(R, A; init=true)
            broadcast!(/, R, R, div(length(A), length(R)) - int(corrected))
        end
    end
end

Base.varm{T}(A::DataArray{T}, m::AbstractArray, region; corrected::Bool=true,
             skipna::Bool=false) =
    Base.varm!(Base.reducedim_initarray(A, region, zero(Base.momenttype(T))), A, m;
               corrected=corrected, skipna=skipna, init=false)

Base.varzm{T}(A::DataArray{T}, region::Union(Integer, AbstractArray, Tuple);
              corrected::Bool=true, skipna::Bool=false) =
    Base.varzm!(Base.reducedim_initarray(A, region, zero(Base.momenttype(T))), A;
               corrected=corrected, skipna=skipna, init=false)

function Base.var{T}(A::DataArray{T}, region::Union(Integer, AbstractArray, Tuple);
                     corrected::Bool=true, mean=nothing, skipna::Bool=false)
    if mean == 0
        Base.varzm(A, region; corrected=corrected, skipna=skipna)
    elseif mean == nothing
        if skipna
            # Can reduce mean into ordinary array
            m = zeros(Base.momenttype(T), Base.reduced_dims(A, region))
            Base.varm(A, Base.mean!(m, A; skipna=skipna), region;
                 corrected=corrected, skipna=skipna)
        else
            Base.varm(A, Base.mean(A, region; skipna=skipna), region;
                 corrected=corrected, skipna=skipna)
        end
    elseif isa(mean, AbstractArray)
        Base.varm(A, mean::AbstractArray, region; corrected=corrected, skipna=skipna)
    else
        error("invalid value of mean")
    end
end
