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

# Determine if there are any true values in a BitArray in a given range
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

## NA-preserving
@ngenerate N typeof(R) function _mapreducedim!{T,N}(f::SafeMapFuns, op::SafeReduceFuns,
                                                    R::AbstractArray, A::DataArray{T,N})
    data = A.data
    na = A.na

    lsiz = Base.check_reducdims(R, data)
    isempty(data) && return R
    @nextract N sizeR d->size(R,d)

    if lsiz > 16
        # use mapreduce_impl, which is probably better tuned to achieve higher performance
        nslices = div(length(A), lsiz)
        ibase = 0
        extr = daextract(R)
        for i = 1:nslices
            if _any(na, ibase+1, ibase+lsiz)
                if isa(R, DataArray)
                    unsafe_setna!(R, extr, i)
                    continue
                else
                    error("cannot reduce a DataArray containing NAs to an AbstractArray")
                end
            end
            @inbounds unsafe_dasetindex!(R, extr, Base.mapreduce_impl(f, op, data, ibase+1, ibase+lsiz), i)
            ibase += lsiz
        end
    elseif isa(R, DataArray)
        na_chunks = A.na.chunks

        # If reducing to a DataArray, skip strides with NAs
        new_data = R.data

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
                @inbounds if new_na[state_0] | Base.unsafe_bitgetindex(na_chunks, k)
                    new_na[state_0] = true
                    @goto loopend
                end

                @inbounds x = data[k]
                v = evaluate(f, x)
                @inbounds v0 = new_data[state_0]
                nv = evaluate(op, v0, v)
                @inbounds new_data[state_0] = nv

                @label loopend
                state_0 += 1
                k += 1
            end)

        R.na = bitpack(new_na)
    else
        # if reducing to a non-DataArray, throw an error at the start on NA
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
@ngenerate N typeof(R) function _mapreducedim_skipna_impl!{T,N}(f, op, R::AbstractArray, A::DataArray{T,N})
    data = A.data
    na = A.na
    na_chunks = na.chunks
    new_data = _getdata(R)

    lsiz = Base.check_reducdims(new_data, data)
    isempty(data) && return new_data
    @nextract N sizeR d->size(new_data,d)
    sizA1 = size(data, 1)

    if lsiz > 16
        # keep the accumulator as a local variable when reducing along the first dimension
        nslices = div(length(A), lsiz)
        ibase = 0
        for i = 1:nslices
            # TODO: use pairwise impl for sum
            @inbounds v = new_data[i]
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
        @nexprs 1 d->(state_0 = state_{N} = 1)
        @nexprs N d->(skip_d = sizeR_d == 1)
        k = 1
        @nloops(N, i, A,
            d->(state_{d-1} = state_d),
            d->(skip_d || (state_d = state_0)), begin
                @inbounds Base.unsafe_bitgetindex(na_chunks, k) && @goto loopend

                @inbounds x = data[k]
                v = evaluate(f, x)
                @inbounds v0 = new_data[state_0]
                nv = evaluate(op, v0, v)
                @inbounds new_data[state_0] = nv

                @label loopend
                state_0 += 1
                k += 1
        end)
    end
    return R    
end

_mapreducedim_skipna!(f, op, R::AbstractArray, A::DataArray) = 
    _mapreducedim_skipna_impl!(f, op, R, A)

# for MinFun/MaxFun, min or max is NA if all values along a dimension are NA
function _mapreducedim_skipna!(f, op::Union(Base.MinFun, Base.MaxFun), R::AbstractArray, A::DataArray)
    R.na = bitpack(all!(fill(true, size(R.na)), A.na))
    _mapreducedim_skipna_impl!(f, op, R, A)
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
