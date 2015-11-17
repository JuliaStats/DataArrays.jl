# TODO: Remove some T's from output type signatures

#' @description
#'
#' A type that extends the normal Julia Array{T} type by allowing
#' the entries to be missing (i.e. NULL or NA) in addition to
#' taking on values of type `T`.
#'
#' @field data::Array{T, N} The elements of the DataArray are stored here,
#'        unless the given element is missing. In this case, the contents
#'        of the corresponding entry in `data` can be any arbitrary value.
#' @field na::BitArray{N} A 1-bit per entry Boolean mask specifying,
#'        for each entry of the DataArray, whether that entry is `NA`
#'        or not.
#'
#' @examples
#'
#' dv = DataArray([1, 2, 3], [false, false, true])
#'
#' dm = DataArray([1 2; 3 4], [false false; true false])
type DataArray{T, N} <: AbstractDataArray{T, N}
    data::Array{T, N}
    na::BitArray{N}

    # Ensure data values and missingness metadata match
    function DataArray(d::Array{T, N}, m::BitArray{N})
        if size(d) != size(m)
            msg = "Data and missingness arrays must be the same size"
            throw(ArgumentError(msg))
        end
        new(d, m)
    end
end

#' @description
#'
#' An DataVector is an DataArray of order 1.
typealias DataVector{T} DataArray{T, 1}

#' @description
#'
#' An DataMatrix is an DataArray of order 2.
typealias DataMatrix{T} DataArray{T, 2}

#' @description
#'
#' Create a DataArray from a set of data values and missingness
#' mask as an `Array{Bool}`.
#'
#' NB: This definition exists because Julia requires that we redefine
#' inner DataArray constructor as an outer constuctor.
#'
#' @param d::Array The non-NA values of the DataArray being constructed.
#' @param m::BitArray A Boolean mask indicating whether each element of
#'        the DataArray is missing or not.
#'
#' @returns out::DataArray A DataArray based on the input data and
#'          missingness mask.
#'
#' @examples
#'
#' da = DataArray([1, 2, 3], falses(3))
function DataArray{T, N}(d::Array{T, N},
                         m::BitArray{N} = falses(size(d))) # -> DataArray{T}
    return DataArray{T, N}(d, m)
end

#' @description
#'
#' Create a DataArray from a set of data values and missingness
#' mask as an `Array{Bool}`.
#'
#' @param d::Array The non-NA values of the DataArray being constructed.
#' @param m::Array{Bool} A Boolean mask indicating whether each element of
#'        the DataArray is missing or not.
#'
#' @returns out::DataArray A DataArray based on the input data and
#'          missingness mask.
#'
#' @examples
#'
#' da = DataArray([1, 2, 3], [false, false, true])
function DataArray(d::Array, m::Array{Bool}) # -> DataArray{T}
    return DataArray(d, bitpack(m))
end

#' @description
#'
#' Create a DataArray of a given type and dimensionality. All
#' entries will be set to `NA` by default.
#'
#' @param T::Type The type of the output DataArray.
#' @param dims::Integer... The size of the output DataArray.
#'
#' @returns out::DataArray A DataArray of the desired type and size.
#'
#' @examples
#'
#' da = DataArray(Int, 2, 2)
function DataArray(T::Type, dims::Integer...) # -> DataArray{T}
    return DataArray(Array(T, dims...), trues(dims...))
end

#' @description
#'
#' Create a DataArray of a given type and dimensionality. All
#' entries will be set to `NA` by default.
#'
#' @param T::Type The type of the output DataArray.
#' @param dims::NTuple{N, Int} The size of the output DataArray.
#'
#' @returns out::DataArray A DataArray of the desired type and size.
#'
#' @examples
#'
#' da = DataArray(Int, (2, 2))
function DataArray{N}(T::Type, dims::NTuple{N, Int}) # -> DataArray{T}
    return DataArray(Array(T, dims...), trues(dims...))
end

#' @description
#'
#' Create a copy of a DataArray.
#'
#' @param da::DataArray The DataArray that will be copied.
#'
#' @returns out::DataArray A copy of `da`.
#'
#' @examples
#'
#' dv = @data [false, false, true, false]
#' dv_new = copy(dv)
Base.copy(d::DataArray) = Base.copy!(similar(d), d) # -> DataArray{T}

function Base.copy!(dest::DataArray, src::DataArray) # -> DataArray{T}
    if isbits(eltype(src)) && isbits(eltype(dest))
        copy!(dest.data, src.data)
    else
        # Elements of src_data are not necessarily initialized, so
        # only copy initialized elements
        dest_data = dest.data
        src_data = src.data
        length(dest_data) >= length(src_data) || throw(BoundsError())
        src_chunks = src.na.chunks
        for i = 1:length(src_data)
            @inbounds if !Base.unsafe_bitgetindex(src_chunks, i)
                dest_data[i] = src_data[i]
            end
        end
    end
    copy!(dest.na, src.na)
    dest
end

function Base.copy!(dest::DataArray, doffs::Integer, src::DataArray) # -> DataArray{T}
    copy!(dest, doffs, src, 1, length(src))
end

# redundant on Julia 0.4
function Base.copy!(dest::DataArray, doffs::Integer, src::DataArray, soffs::Integer) # -> DataArray{T}
    soffs <= length(src) || throw(BoundsError())
    copy!(dest, doffs, src, soffs, length(src)-soffs+1)
end

function Base.copy!(dest::DataArray, doffs::Integer, src::DataArray, soffs::Integer, n::Integer) # -> DataArray{T}
    if n == 0
        return dest
    elseif n < 0
        throw(BoundsError())
    end
    if isbits(eltype(src))
        copy!(dest.data, doffs, src.data, soffs, n)
    else
        # Elements of src_data are not necessarily initialized, so
        # only copy initialized elements
        dest_data = dest.data
        src_data = src.data
        if doffs < 1 || length(dest_data) - doffs + 1 < n ||
           soffs < 1 || length(src_data) - soffs + 1 < n
            throw(BoundsError())
        end
        src_chunks = src.na.chunks
        for i = 0:(n-1)
            di, si = doffs + i, soffs + i

            @inbounds if !Base.unsafe_bitgetindex(src_chunks, si)
                dest_data[di] = src_data[si]
            end
        end
    end
    copy!(dest.na, doffs, src.na, soffs, n)
    dest
end

Base.fill!(A::DataArray, ::NAtype) = (fill!(A.na, true); A)
Base.fill!(A::DataArray, v) = (fill!(A.data, v); fill!(A.na, false); A)

#' @description
#'
#' Create a deep copy of a DataArray.
#'
#' @param da::DataArray The DataArray that will be deep copied.
#'
#' @returns out::DataArray A deep-copy of `da`.
#'
#' @examples
#'
#' dv = @data [false, false, true, false]
#' dv_new = deepcopy(dv)
function Base.deepcopy(d::DataArray) # -> DataArray{T}
    return DataArray(deepcopy(d.data), deepcopy(d.na))
end

function Base.resize!{T}(da::DataArray{T,1}, n::Int)
    resize!(da.data, n)
    oldn = length(da.na)
    resize!(da.na, n)
    da.na[oldn+1:n] = true
    da
end

#' @description
#'
#' Create a new DataArray{T} that is similar to an existing DataArray.
#'
#' @param da::DataArray DataArray based on which a new DataArray
#'        will be created.
#' @param T::Type The element type of the output DataArray.
#' @param dims::Dims The dimensionality of the output DataArray.
#'
#' @returns out::DataArray{T} A new DataArray with the desired type and
#'          dimensionality.
#'
#' @examples
#'
#' dv = @data [false, false, true, false]
#' dv_new = similar(dv, Float64, 2, 2, 2)
function Base.similar(da::DataArray, T::Type, dims::Dims) #-> DataArray{T}
    return DataArray(Array(T, dims), trues(dims))
end

#' @description
#'
#' Find the sizes along each dimension of the DataArray.
#'
#' @param da::DataArray{Bool} DataArray whose size is desired.
#'
#' @returns a::Array{Int} Array containing the indices of all `true` values
#'          in `da`.
#'
#' @examples
#'
#' dv = @data [false, false, true, false]
#' inds = find(dv)
Base.size(d::DataArray) = size(d.data) # -> (Int...)

#' @description
#'
#' Determine the number of dimensions (i.e. order) of a DataArray.
#'
#' @param da::DataArray{Bool} DataArray whose dimensionality is desired.
#'
#' @returns d::Int The number of dimensions of `da`.
#'
#' @examples
#'
#' dm = @data [false false; true false]
#' inds = ndims(dm)
Base.ndims(da::DataArray) = ndims(da.data) # -> Int

#' @description
#'
#' Determine the length (e.g. number of elements) of a DataArray.
#'
#' @param da::DataArray The DataArray whose length is desired.
#'
#' @returns n::Int The length of `da`.
#'
#' @examples
#'
#' dv = @data [false, false, true, false]
#' i = length(dv)
Base.length(d::DataArray) = length(d.data) # -> Int

#' @description
#'
#' Find the index of the last element of `da`.
#'
#' @param da::DataArray The DataArray whose final element's index is desired.
#'
#' @returns i::Int The index of the final element of `da`.
#'
#' @examples
#'
#' dv = @data [false, false, true, false]
#' i = endof(dv)
Base.endof(da::DataArray) = endof(da.data) # -> Int

#' @description
#'
#' Find the indices of all `true` values in a `DataArray{Bool}`.
#'
#' @param da::DataArray{Bool} DataArray whose `true` values will be found.
#'
#' @returns a::Array{Int} Array containing the indices of all `true` values
#'          in `da`.
#'
#' @examples
#'
#' dv = @data [false, false, true, false]
#' inds = find(dv)
function Base.find(da::DataArray{Bool}) # -> Array{Int}
    data = da.data
    ntrue = 0
    @inbounds @bitenumerate da.na i na begin
        ntrue += !na && data[i]
    end
    res = Array(Int, ntrue)
    count = 1
    @inbounds @bitenumerate da.na i na begin
        if !na && data[i]
            res[count] = i
            count += 1
        end
    end
    return res
end

#' @description
#'
#' Convert a DataArray{T} to an Array{S}. Throws an `NAException`
#' if the input contains `NA` values that prohibit conversion.
#'
#' @param da::DataArray{T} The DataArray that will be converted.
#'
#' @returns a::Array{S} The (possibly type-converted) elements of
#'         `da` if none were `NA`.
#'
#' @examples
#'
#' da = @data [1 2; 3 NA]
#' a = convert(Array{Float64}, da)
#'
#' da = @data [1 2; 3 4]
#' a = convert(Array{Float64}, da)
function Base.convert{S, T, N}(::Type{Array{S, N}},
                               x::DataArray{T, N}) # -> Array{S, N}
    if anyna(x)
        err = "Cannot convert DataArray with NA's to desired type"
        throw(NAException(err))
    else
        return convert(Array{S, N}, x.data)
    end
end

function Base.convert{S, T, N}(::Type{Array{S}}, da::DataArray{T, N})
    return convert(Array{S, N}, da)
end

function Base.convert{T}(::Type{Vector}, dv::DataVector{T})
    return convert(Array{T, 1}, dv)
end

function Base.convert{T}(::Type{Matrix}, dm::DataMatrix{T})
    return convert(Array{T, 2}, dm)
end

function Base.convert{T, N}(::Type{Array}, da::DataArray{T, N})
    return convert(Array{T, N}, da)
end

function Base.convert{S, T, N}(
    ::Type{Array{S, N}},
    da::DataArray{T, N},
    replacement::Any
) # -> Array{S, N}
    replacementS = convert(S, replacement)
    res = Array(S, size(da))
    for i in 1:length(da)
        if da.na[i]
            res[i] = replacementS
        else
            res[i] = da.data[i]
        end
    end
    return res
end

function Base.convert{T}(::Type{Vector}, dv::DataVector{T}, replacement::Any)
    return convert(Array{T, 1}, dv, replacement)
end

function Base.convert{T}(::Type{Matrix}, dm::DataMatrix{T}, replacement::Any)
    return convert(Array{T, 2}, dm, replacement)
end

function Base.convert{T, N}(::Type{Array}, da::DataArray{T, N}, replacement::Any)
    return convert(Array{T, N}, da, replacement)
end

#' @description
#'
#' Turn a DataVector into a Vector. Drop any NA's.
#'
#' NB: Because NA's are dropped instead of replaced, this function only
#'     works on DataVector's and will not work on DataArray's of higher
#'     order.
#'
#' @param dv::DataVector{T} DataArray that will be converted to an Array.
#'
#' @returns v::Array{T} Array containing only the non-NA values of `dv`.
#'
#' @examples
#'
#' dv = @data [1, 2, NA, 4]
#' v = dropna(dv)
dropna(dv::DataVector) = copy(dv.data[!dv.na]) # -> Vector

#' @description
#'
#' Determine if the entries of an DataArray are `NA`.
#'
#' @param a::DataArray{T, N} The DataArray whose missingness will
#'        be assessed.
#'
#' @returns na::BitArray{N} Elementwise Boolean whether entry is missing.
#'
#' @examples
#'
#' da = @data([1, 2, 3])
#' isna(da)
isna(da::DataArray) = copy(da.na) # -> BitArray

#' @description
#'
#' Safe and type-stable way to determine if element `i` of an
#' DataArray is `NA`.
#'
#' @param a::DataArray The DataArray whose missingness will
#'        be assessed.
#' @param inds::Any The indices of the elements to be checked for `NA`.
#'
#' @returns na::Any Are the indexed elements `NA` or not?
#'
#' @examples
#'
#' a = @data([1, 2, 3])
#' isna(a, 1)
#' isna(a, 1:2)
isna(da::DataArray, I::Any) = getindex(da.na, I)

@nsplat N function isna(da::DataArray, I::NTuple{N,Any}...)
    getindex(da.na, I...)
end

#' @description
#'
#' Determine if any of the entries of an DataArray are `NA`.
#'
#' @param da::DataArray{T, N} The DataArray whose elements will
#'        be assessed.
#'
#' @returns out::Bool Are any of the elements of `da` an `NA` value?
#'
#' @examples
#'
#' da = @data([1, 2, 3])
#' anyna(da)
anyna(da::DataArray) = any(da.na) # -> Bool

#' @description
#'
#' Determine if all of the entries of an DataArray are `NA`.
#'
#' @param da::DataArray{T, N} The DataArray whose elements will
#'        be assessed.
#'
#' @returns out::Bool Are all of the elements of `da` an `NA` value?
#'
#' @examples
#'
#' da = @data([1, 2, 3])
#' allna(da)
allna(da::DataArray) = all(da.na) # -> Bool

#' @description
#'
#' Determine if the entries of an DataArray are `NaN`.
#'
#' @param da::DataArray{T, N} The DataArray whose elements will
#'        be assessed.
#'
#' @returns na::DataArray{Bool} Elementwise Boolean whether entry is `NaN`.
#'
#' @examples
#'
#' da = @data([1, 2, 3])
#' isnan(da)
function Base.isnan(da::DataArray) # -> DataArray{Bool}
    return DataArray(isnan(da.data), copy(da.na))
end

#' @description
#'
#' Determine if the entries of an DataArray are finite, which means
#' neither `+/-NaN` nor `+/-Inf`.
#'
#' @param da::DataArray{T, N} The DataArray whose elements will
#'        be assessed.
#'
#' @returns na::DataArray{Bool} Elementwise Boolean whether entry is finite.
#'
#' @examples
#'
#' da = @data([1, 2, 3])
#' isfinite(da)
function Base.isfinite(da::DataArray) # -> DataArray{Bool}
    n = length(da)
    res = Array(Bool, size(da))
    for i in 1:n
        if !da.na[i]
            res[i] = isfinite(da.data[i])
        end
    end
    return DataArray(res, copy(da.na))
end

# Promotion rules

# promote_rule{T, T}(::Type{AbstractDataArray{T}},
#                    ::Type{T}) = promote_rule(T, T)
# promote_rule{S, T}(::Type{AbstractDataArray{S}},
#                    ::Type{T}) = promote_rule(S, T)
# promote_rule{T}(::Type{AbstractDataArray{T}}, ::Type{T}) = T

#' @description
#'
#' Convert an Array{T} to a DataArray{S}.
#'
#' @param a::Array{T} The Array that will be converted.
#'
#' @returns da::DataArray{S} The converted DataArray with potential
#'          type-conversion of the elements of `a`.
#'
#' @examples
#'
#' a = [1 2; 3 4]
#' da = convert(DataArray{Float64}, a)
function Base.convert{S, T, N}(::Type{DataArray{S, N}},
                               a::AbstractArray{T, N}) # -> DataArray{S, N}
    return DataArray(convert(Array{S, N}, a), falses(size(a)))
end
Base.convert{S,T,N}(::Type{DataArray{S}}, x::AbstractArray{T,N}) =
    convert(DataArray{S,N}, x)
Base.convert{T, N}(::Type{DataArray}, x::AbstractArray{T, N}) =
    convert(DataArray{T,N}, x)

#' @description
#'
#' Convert a DataArray{T} to a DataArray{S}.
#'
#' @param da::DataArray{T} The DataArray that will be converted.
#'
#' @returns out::DataArray{S} The converted DataArray.
#'
#' @examples
#'
#' dv = @data [1, 2, NA, 4]
#' dv_alt = convert(DataVector{Float64}, dv)
function Base.convert{S, T, N}(::Type{DataArray{S, N}},
                               x::DataArray{T, N}) # -> DataArray{S, N}
    return DataArray(convert(Array{S}, x.data), x.na)
end

#' @description
#'
#' Convert an AbstractArray to a DataArray.
#'
#' @param a::AbstractArray{T} The AbstractArray that will be converted to
#'        a DataArray.
#'
#' @returns da::DataArray{T} `a` after conversion to a DataArray.
#'
#' @examples
#'
#' dv = data([1, 2, 3, 4])
#' dm = data([1 2; 3 4])
#' dv = data(1:10)
#' dv = data(falses(3))

data(a::AbstractArray) = convert(DataArray, a)

#' @description
#'
#' Convert a DataArray to an Array of int, float or bool type.
#'
#' @param da::DataArray{T} The DataArray that will be converted.
#'
#' @returns a::Array{Union(Int, Float64, Bool)} An Array containing the
#'          type-converted values of `da`.
#'
#' @examples
#'
#' dv = @data [1, 2, NA, 4]
#' v = int(dv)
#' v = float(dv)
#' v = bool(dv)
#
# TODO: Make sure these handle copying correctly
# TODO: Remove these? They have odd behavior, because they convert to Array's.
# TODO: Rethink multi-item documentation approach
for f in (:(Base.int), :(Base.float), :(Base.bool))
    @eval begin
        function ($f)(da::DataArray) # -> DataArray
            if anyna(da)
                err = "Cannot convert DataArray with NA's to desired type"
                throw(NAException(err))
            else
                ($f)(da.data)
            end
        end
    end
end

#' @description
#'
#' Compute the hash of an AbstractDataArray.
#'
#' @param da::DataArray{T} DataArray whose hash is desired.
#'
#' @returns h::UInt An unsigned integer hash value for `da`.
#'
#' @examples
#'
#' dv = @data [1, 2, NA, 4]
#' k = hash(dv)
#
# TODO: Make sure this agrees with is_equals()
function Base.hash(a::DataArray) # -> UInt
    # hash NA pattern
    h = hash(a.na)
    # hash non-NA elements
    i = findfirst(a.na, false)
    while i > 0
      h = hash(a.data[i], h)
      i = findnext(a.na, false, i+1)
    end
    return @compat UInt(h)
end

#' @internal
#' @description
#'
#' Find the unique values in a DataArray, noting if `NA` occurs in the
#' `DataArray`.
#'
#' @param da::DataArray{T} `DataArray` whose unique values are desired.
#'
#' @returns v::Vector{T} `Vector` containing the unique values from `da`.
#' @returns firstna::Int The index of the first `NA` value, or 0 if
#'          none is present.
#'
#' @examples
#'
#' dv = @data [1, 2, NA, 4]
#' distinct_values, firstna = finduniques(dv)
function finduniques{T}(da::DataArray{T}) # -> Vector{T}, Int
    out = Array(T,0)
    seen = Set{T}()
    n = length(da)
    firstna = 0
    for i in 1:n
        if isna(da, i)
            if firstna == 0
                firstna = length(out) + 1
            else
                continue
            end
        elseif !in(da.data[i], seen)
            push!(seen, da.data[i])
            push!(out, da.data[i])
        end
    end
    return out, firstna
end

#' @description
#'
#' Return a DataVector containing the unique values of a `DataArray`,
#' in the order they appear in the data, including `NA` if any missing entries
#' are encountered.
#'
#' @param da::DataArray{T} `DataArray` whose unique values are desired.
#'
#' @returns dv::DataVector{T} `DataVector` containing the unique values
#'          from `da`, in the order they appear, including `NA` if there are
#'          any missing entries in `da`.
#'
#' @examples
#'
#' dv = @data [1, -2, 1, NA, 4]
#' distinct_values = unique(dv)
function Base.unique{T}(da::DataArray{T}) # -> DataVector{T}
    unique_values, firstna = finduniques(da)
    n = length(unique_values)
    if firstna > 0
        res = DataArray(Array(T, n + 1))
        i = 1
        for val in unique_values
            if i == firstna
                res.na[i] = true
                i += 1
            end
            res.data[i] = val
            i += 1
        end

        if firstna == n + 1
            res.na[n + 1] = true
        end

        return res
    else
        return DataArray(unique_values)
    end
end

#' @description
#'
#' Return a DataVector containing the unique values of a `DataArray`,
#' excluding `NA`.
#'
#'
#' @param da::DataArray{T} `DataArray` whose unique values are desired.
#'
#' @returns dv::DataVector{T} `DataVector` containing the unique values
#'          from `da`, excluding `NA`.
#'
#' @examples
#'
#' dv = @data [1, -2, 1, NA, 4]
#' distinct_values = levels(dv)
function levels(da::DataArray) # -> DataVector{T}
    unique_values, firstna = finduniques(da)
    return DataArray(unique_values)
end

#' @description
#'
#' Return a Vector containing the unique values of an `Array`. This
#' function is identical to `unique` and is only defined for consistency
#' with `DataArray`s and `PooledDataArray`s.
#'
#'
#' @param a::Array{T} `Array` whose unique values are desired.
#'
#' @returns v::Vector{T} `Vector` containing the unique values from `da`.
#'
#' @examples
#'
#' v = [1, -2, 1, 4]
#' distinct_values = levels(v)
function levels(a::AbstractArray) # -> Vector{T}
    return unique(a)
end
