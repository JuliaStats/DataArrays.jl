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
function Base.copy(d::DataArray) # -> DataArray{T}
    return DataArray(copy(d.data), copy(d.na))
end

function Base.copy!(dest::DataArray, src::DataArray) # -> DataArray{T}
    copy!(dest.data, src.data)
    copy!(dest.na, src.na)
    dest
end

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

#' @description
#'
#' Append the elements of items to the end of d.
#'
#' @param da::DataArray{T,1} The DataArray to append elements to.
#' @param items::DataArray{T,1} Elements to append to da.
#'
#' @returns: out::DataArray{T,1} The modified data array.
#'
#' @examples
#'
#' x = @data [1, 2, 3]
#' y = @data [4, 5, 6]
#' append!(x, y)
function Base.append!{T}(da::DataArray{T,1}, items::DataArray{T,1})
    append!(da.data, items.data)
    append!(da.na, items.na)
    da
end

#' @description
#'
#' Append the elements of items to the end of d.
#'
#' @param da::DataArray{T,1} The DataArray to append elements to.
#' @param items::AbstractArray{T,1} Elements to append to da.
#'
#' @returns: out::DataArray{T,1} The modified data array.
#'
#' @examples
#'
#' x = @data [1, 2, 3]
#' y = [4, 5, 6]
#' append!(x, y)
function Base.append!{T}(da::DataArray{T,1}, items::AbstractArray{T,1})
    append!(da.data, items)
    oldn = length(da.na)
    resize!(da.na, oldn + length(items))
    da.na[oldn+1:end] = false
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
#' Turn a DataArray into an Array. Raises an error if NA's are encountered.
#' 
#' @param da::DataArray{T} DataArray that will be converted to an Array.
#'
#' @returns a::Array{T} Array containing values of `da`.
#'
#' @examples
#'
#' dv = @data [1, 2, 3, 4]
#' v = array(dv)
#'
#' dm = @data [1 2; 3 4]
#' m = array(dm)
function array{T}(da::DataArray{T}) # -> Array{T}
    res = Array(T, size(da))
    for i in 1:length(da)
        if da.na[i]
            throw(NAException())
        else
            res[i] = da.data[i]
        end
    end
    return res
end

#' @description
#'
#' Turn a DataArray into an Array. Replace any NA's with the value
#' of second argument, `replacement`.
#' 
#' @param da::DataArray{T} DataArray that will be converted to an Array.
#' @param replacement::T Value that will replace NA's in `da`.
#'
#' @returns a::Array{T} Array containing values of `da` plus replacements.
#'
#' @examples
#'
#' dv = @data [1, 2, NA, 4]
#' v = array(dv, 3)
#'
#' dm = @data [1 2; NA 4]
#' m = array(dm, 3)
function array{T}(da::DataArray{T}, replacement::T) # -> Array{T}
    res = Array(T, size(da))
    for i in 1:length(da)
        if da.na[i]
            res[i] = replacement
        else
            res[i] = da.data[i]
        end
    end
    return res
end

#' @description
#'
#' Turn a DataArray into an Array. Replace any NA's with the value
#' of second argument, `replacement`.
#' 
#' @param da::DataArray{T} DataArray that will be converted to an Array.
#' @param replacement::Any Value that will replace NA's in `da`.
#'        Converted to the `eltype`, `T`, of `da`.
#'
#' @returns a::Array{T} Array containing values of `da` plus replacements.
#'
#' @examples
#'
#' dv = @data [1, 2, NA, 4]
#' v = array(dv, 3)
#'
#' dm = @data [1 2; NA 4]
#' m = array(dm, 3)
function array{T}(da::DataArray{T}, replacement::Any) # -> Array{T}
    return array(da, convert(T, replacement))
end

#' @description
#'
#' NO-OP: Turn a Vector into a Vector. See dropna(dv::DataVector) for
#'        rationale.
#'
#' @param v::Vector{T} Vector that will be converted to a Vector.
#'
#' @returns v::Vector{T} Vector containing all of the values of `v`.
#'
#' @examples
#'
#' v = [1, 2, 3, 4]
#' v = dropna(v)
dropna(v::AbstractVector) = copy(v) # -> AbstractVector

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

# Iterators
# TODO: Use values()
#       Use DataValueIterator type?

type EachFailNA{T}
    da::AbstractDataArray{T}
end
each_failNA{T}(da::AbstractDataArray{T}) = EachFailNA(da)
Base.start(itr::EachFailNA) = 1
function Base.done(itr::EachFailNA, ind::Integer)
    return ind > length(itr.da)
end
function Base.next(itr::EachFailNA, ind::Integer)
    if isna(itr.da[ind])
        throw(NAException())
    else
        (itr.da[ind], ind + 1)
    end
end

type EachDropNA{T}
    da::AbstractDataArray{T}
end
each_dropna{T}(da::AbstractDataArray{T}) = EachDropNA(da)
Base.start(itr::EachDropNA) = 1
function Base.done(itr::EachDropNA, ind::Integer)
    return ind > length(itr.da)
end
function Base.next(itr::EachDropNA, ind::Integer)
    while ind <= length(itr.da) && isna(itr.da[ind])
        ind += 1
    end
    (itr.da[ind], ind + 1)
end

type EachReplaceNA{S, T}
    da::AbstractDataArray{S}
    replacement_val::T
end
function each_replaceNA(da::AbstractDataArray, val::Any)
    EachReplaceNA(da, convert(eltype(da), val))
end
function each_replaceNA(val::Any)
    x -> each_replaceNA(x, val)
end
Base.start(itr::EachReplaceNA) = 1
function Base.done(itr::EachReplaceNA, ind::Integer)
    return ind > length(itr.da)
end
function Base.next(itr::EachReplaceNA, ind::Integer)
    if isna(itr.da[ind])
        (itr.replacement_val, ind + 1)
    else
        (itr.da[ind], ind + 1)
    end
end

# Indexing

typealias SingleIndex Real
# TODO: Remove BitVector here
typealias MultiIndex Union(Vector, BitVector, Ranges, Range1)
typealias BooleanIndex Union(BitVector, Vector{Bool})

# TODO: Solve ambiguity warnings here without
#       ridiculous accumulation of methods

#' @description
#'
#' Get a set of elements of a DataArray.
#'
#' @param v::Vector A Vector whose elements will be retrieved.
#' @param inds::AbstractDataVector{Bool} An AbstractDataVector
#'        containing a Boolean mask specifying, for each element,
#'        whether that element should be retrieved or not. `NA`
#'        values in the mask are treated as `false`.
#'
#' @returns out::Vector The values of `v` at the requested indices.
#'
#' @examples
#'
#' v = [1, 2, 3]
#' inds = @data([false, true])
#' v[inds]
function Base.getindex(v::Vector,
                       inds::AbstractDataVector{Bool})
    return v[find(inds)]
end

#' @description
#'
#' Get a set of elements of a DataArray.
#'
#' @param v::Vector A Vector whose elements will be retrieved.
#' @param inds::AbstractDataArray{Bool} A DataArray containing
#'        a Boolean mask specifying, for each element, whether
#'        that element should be retrieved or not. `NA` values
#'        in the mask are treated as `false`.
#'
#' @returns out::Vector The values of `v` at the requested indices.
#'
#' @examples
#'
#' v = [1, 2, 3]
#' inds = @data([false, true])
#' v[inds]
function Base.getindex(v::Vector,
                       inds::AbstractDataArray{Bool})
    return v[find(inds)]
end

#' @description
#'
#' Get a set of elements of a DataArray.
#'
#' @param a::Array An Array whose elements will be retrieved.
#' @param inds::AbstractDataArray{Bool} A DataArray containing
#'        a Boolean mask specifying, for each element, whether
#'        that element should be retrieved or not. `NA` values
#'        in the mask are treated as `false`.
#'
#' @returns out::Vector{S} The values of `a` at the requested indices.
#'
#' @examples
#'
#' a = [1, 2, 3]
#' inds = @data([false, true])
#' a[inds]
function Base.getindex(a::Array,
                       inds::AbstractDataArray{Bool})
    return a[find(inds)]
end

#' @description
#'
#' Get a set of elements of a DataArray.
#'
#' @param a::Vector{S} A Vector whose elements will be retrieved.
#' @param inds::AbstractDataArray{T} A DataArray containing the indices
#'        of the requested elements.
#'
#' @returns out::Vector{S} The values of `a` at the requested indices.
#'
#' @examples
#'
#' a = [1, 2, 3]
#' inds = @data([1, 2])
#' da[1]
#'
#' da = @data([NA, 2, 3])
#' da[1]
function Base.getindex{S, T}(a::Vector{S},
                             inds::AbstractDataArray{T})
    return a[dropna(inds)]
end

#' @description
#'
#' Get a set of elements of a DataArray.
#'
#' @param a::Array{T, N} An Array whose elements will be retrieved.
#' @param inds::AbstractDataArray{T} A DataArray containing the indices
#'        of the requested elements.
#'
#' @returns out::Array{T} The values of `a` at the requested indices.
#'
#' @examples
#'
#' a = [1, 2, 3]
#' inds = @data([1, 2])
#' da[1]
#'
#' da = @data([NA, 2, 3])
#' da[1]
function Base.getindex{S, T}(a::Array{S},
                             inds::AbstractDataArray{T})
    return a[dropna(inds)]
end

#' @description
#'
#' Get a set of elements of a DataArray.
#'
#' @param da::DataArray{T, N} A DataArray whose element will be retrieved.
#' @param ind::Real The index of the element to be retrieved.
#'
#' @returns out::Union(T, NAtype) The value of `da` at the requested
#'          index.
#'
#' @examples
#'
#' da = @data([1, 2, 3])
#' da[1]
#'
#' da = @data([NA, 2, 3])
#' da[1]
function Base.getindex(da::DataArray, ind::SingleIndex)
	if da.na[ind]
		return NA
	else
		return da.data[ind]
	end
end

# d[MultiItemIndex]
# TODO: Return SubDataArray
#' @description
#'
#' Get a set of elements of a DataArray.
#'
#' @param da::DataArray{T, N} A DataArray whose element will be retrieved.
#' @param inds::AbstractDataVector{Bool} A Boolean mask specifying for
#'        each element whether that element will be retrieved or not.
#'
#' @returns out::DataArray{T} A DataArray containing the elements of
#'          `da` indexed.
#'
#' @examples
#'
#' da = @data([1, 2, 3])
#' inds = @data([true, false])
#' da[inds]
function Base.getindex(d::DataArray,
                       inds::AbstractDataVector{Bool})
    inds = find(inds)
    return d[inds]
end

#' @description
#'
#' Get a set of elements of a DataArray.
#'
#' @param da::DataArray{T, N} A DataArray whose element will be retrieved.
#' @param inds::AbstractDataVector The indices of the elements that
#'        will be retrieved.
#'
#' @returns out::DataArray{T} A DataArray containing the elements of
#'          `da` indexed.
#'
#' @examples
#'
#' da = @data([1, 2, 3])
#' inds = @data([1, 2])
#' da[inds]
function Base.getindex(d::DataArray,
                       inds::AbstractDataVector)
    inds = dropna(inds)
    return d[inds]
end

# There are two definitions in order to remove ambiguity warnings
# TODO: Return SubDataArray
# TODO: Make inds::AbstractVector
#' @description
#'
#' Get a set of elements of a DataArray.
#'
#' @param da::DataArray{T, N} A DataArray whose element will be retrieved.
#' @param inds::BooleanIndex A Boolean mask specifying whether each
#'        element of `da` should be retrieved.
#'
#' @returns out::DataArray{T} A DataArray containing the elements of
#'          `da` indexed.
#'
#' @examples
#'
#' da = @data([1, 2, 3])
#' da[[true, false]]
function Base.getindex{T <: Number, N}(d::DataArray{T,N},
                                       inds::BooleanIndex)
    DataArray(d.data[inds], d.na[inds])
end

#' @description
#'
#' Get a set of elements of a DataArray.
#'
#' @param da::DataArray{T, N} A DataArray whose element will be retrieved.
#' @param inds::BooleanIndex A Boolean mask specifying whether each
#'        element of `da` should be retrieved.
#'
#' @returns out::DataArray{T} A DataArray containing the elements of
#'          `da` indexed.
#'
#' @examples
#'
#' da = @data([1, 2, 3])
#' da[[true, false]]
function Base.getindex(d::DataArray, inds::BooleanIndex)
    res = similar(d, sum(inds))
    j = 1
    for i in 1:length(inds)
        if inds[i]
            if !d.na[i]
                res[j] = d.data[i]
            end
            j += 1
        end
    end
    return res
end

#' @description
#'
#' Get a set of elements of a DataArray.
#'
#' @param da::DataArray{T, N} A DataArray whose element will be retrieved.
#' @param inds::MultiIndex The indices of the elements of `da` to be
#'        retrieved.
#'
#' @returns out::DataArray{T} A DataArray containing the elements of
#'          `da` indexed.
#'
#' @examples
#'
#' da = @data([1, 2, 3])
#' da[[1, 2]]
function Base.getindex{T <: Number, N}(d::DataArray{T, N},
                                       inds::MultiIndex)
    return DataArray(d.data[inds], d.na[inds])
end

#' @description
#'
#' Get a set of elements of a DataArray.
#'
#' @param da::DataArray{T, N} A DataArray whose element will be retrieved.
#' @param inds::MultiIndex The indices of the elements of `da` to be
#'        retrieved.
#'
#' @returns out::DataArray{T} A DataArray containing the elements of
#'          `da` indexed.
#'
#' @examples
#'
#' da = @data([1, 2, 3])
#' da[[1, 2]]
function Base.getindex(d::DataArray, inds::MultiIndex)
    res = similar(d, length(inds))
    for i in 1:length(inds)
        ix = inds[i]
        if !d.na[ix]
            res[i] = d.data[ix]
        else
            res[i] = NA # We could also change this in similar
        end
    end
    return res
end

# TODO: Return SubDataArray
# TODO: Make inds::AbstractVector
## # The following assumes that T<:Number won't have #undefs
## # There are two definitions in order to remove ambiguity warnings
#' @description
#'
#' Get a set of elements of a DataArray.
#'
#' @param da::DataArray{T, N} A DataArray whose element will be retrieved.
#' @param inds::BooleanIndex A Boolean mask specifying whether each
#'        element of `da` should be retrieved.
#'
#' @returns out::DataArray{T} A DataArray containing the elements of
#'          `da` indexed.
#'
#' @examples
#'
#' da = @data([1, 2, 3])
#' da[[true, false]]
function Base.getindex{T <: Number, N}(d::DataArray{T, N},
                                       inds::BooleanIndex)
    DataArray(d.data[inds], d.na[inds])
end

#' @description
#'
#' Get a set of elements of a DataArray.
#'
#' @param da::DataArray{T, N} A DataArray whose element will be retrieved.
#' @param inds::MultiIndex The indices of the elements of `da` to be
#'        retrieved.
#'
#' @returns out::DataArray{T} A DataArray containing the elements of
#'          `da` indexed.
#'
#' @examples
#'
#' da = @data([1, 2, 3])
#' da[[1, 2]]
function Base.getindex{T <: Number, N}(d::DataArray{T, N},
                                       inds::MultiIndex)
    DataArray(d.data[inds], d.na[inds])
end

#' @description
#'
#' Set one element of a DataArray to `NA`.
#'
#' @param da::DataArray{T, N} A DataArray whose element will be modified.
#' @param val::NAtype The `NA` value being assigned to an element of `da`.
#' @param ind::Real A real value specifying the index of the element
#'        of `da` being modified.
#'
#' @returns da::DataArray{T, N} The modified DataArray.
#'
#' @examples
#'
#' da = @data([1, 2, 3])
#' da[1] = NA
function Base.setindex!(da::DataArray,
                        val::NAtype,
                        i::SingleIndex) # -> NAtype
	da.na[i] = true
    return da
end

#' @description
#'
#' Set one element of a DataArray to a new value, `val`.
#'
#' @param da::DataArray{T, N} A DataArray whose element will be modified.
#' @param val::Any The value being assigned to an element of `da`.
#' @param ind::Real A real value specifying the index of the element
#'        of `da` being modified.
#'
#' @returns da::DataArray{T, N} The modified DataArray.
#'
#' @examples
#'
#' da = @data([1, 2, 3])
#' da[1] = 4
function Base.setindex!(da::DataArray,
                        val::Any,
                        ind::SingleIndex) # -> Any
	da.data[ind] = val
	da.na[ind] = false
    return da
end

#' @description
#'
#' Set a specified set of elements of a DataArray to `NA`.
#'
#' NB: The indices in `inds` must be exhaustive.
#'
#' @param da::DataArray{T, N} A DataArray whose entries will be modified.
#' @param val::NAtype The NA value being assigned to elements of `da`.
#' @param inds::AbstractVector{Bool} A Boolean vector specifying for every
#'        element of `da` whether it will be modified.
#'
#' @returns da::DataArray{T, N} The modified DataArray.
#'
#' @examples
#'
#' da = @data([1, 2, 3])
#' da[[true, false, true]] = NA
function Base.setindex!(da::DataArray,
                        val::NAtype,
                        inds::AbstractVector{Bool}) # -> NAtype
    da.na[find(inds)] = true
    return da
end

#' @description
#'
#' Set a specified set of elements of a DataArray to `NA`.
#'
#' @param da::DataArray{T, N} A DataArray whose entries will be modified.
#' @param val::NAtype The NA value being assigned to elements of `da`.
#' @param inds::AbstractVector A vector of indices of `da` to
#'        be modified.
#'
#' @returns da::DataArray{T, N} The modified DataArray.
#'
#' @examples
#'
#' da = @data([1, 2, 3])
#' da[1:3] = NA
function Base.setindex!(da::DataArray,
                        val::NAtype,
                        inds::AbstractVector) # -> NAtype
    da.na[inds] = true
    return da
end

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
isna(da::DataArray, inds::Any...) = getindex(da.na, inds...)

#' @description
#'
#' Determine if the entries of an DataArray are `NaN`.
#'
#' @param a::DataArray{T, N} The DataArray whose elements will
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
#' @param a::DataArray{T, N} The DataArray whose elements will
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

#' @description
#'
#' Convert a DataArray{T} to an Array{T}. Throws an `NAException`
#' if the input contains `NA` values that prohibit conversion.
#'
#' @param da::DataArray{T} The DataArray that will be converted.
#'
#' @returns a::Array{T} The elements of `da` if none were `NA`.
#'
#' @examples
#'
#' da = @data [1 2; 3 NA]
#' a = convert(Array, da)
#'
#' da = @data [1 2; 3 4]
#' a = convert(Array, da)
# TODO: Consider making a copy here
function Base.convert{T, N}(::Type{Array},
                            x::DataArray{T, N}) # -> Array{T}
    if anyna(x)
        err = "Cannot convert DataArray with NA's to base type"
        throw(NAException(err))
    else
        return x.data
    end
end

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

#' @description
#'
#' Convert an Array{T} to a DataArray{T}.
#'
#' @param a::Array{T} The Array that will be converted.
#'
#' @returns da::DataArray{T} The converted DataArray.
#'
#' @examples
#'
#' a = [1 2; 3 4]
#' da = convert(DataArray, a)
function Base.convert{T, N}(::Type{DataArray},
                            a::AbstractArray{T, N}) # -> DataArray{T, N}
    return DataArray(convert(Array{T, N}, a), falses(size(a)))
end

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
#' NO-OP: See convert(DataArray{S}, DataArray{T}) for rationale.
#' TODO: Make operative by doing copy?
#'
#' @param da::DataArray{T} The DataArray that will be converted.
#'
#' @returns out::DataArray{T} The converted DataArray.
#'
#' @examples
#'
#' dv = @data [1, 2, NA, 4]
#' dv_alt = convert(DataVector, dv)
function Base.convert{T, N}(::Type{DataArray},
                            x::DataArray{T, N}) # -> DataArray{T, N}
    return DataArray(x.data, x.na)
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
#' @returns h::Uint An unsigned integer hash value for `da`.
#'
#' @examples
#'
#' dv = @data [1, 2, NA, 4]
#' k = hash(dv)
#
# TODO: Make sure this agrees with is_equals()
function Base.hash(a::AbstractDataArray) # -> Uint
    h = hash(size(a)) + 1
    for i in 1:length(a)
        h = bitmix(h, int(hash(a[i])))
    end
    return uint(h)
end

#' @internal
#' @description
#'
#' Find the unique values in a DataArray, noting if `NA` occurs in the
#' DataArray.
#'
#' @param da::DataArray{T} DataArray whose unique values are desired.
#'
#' @returns v::Vector{T} Vector containing the unique values from `da`.
#' @returns hasna::Bool Did `da` contain any `NA` entries?
#'
#' @examples
#'
#' dv = @data [1, 2, NA, 4]
#' distinct_values, hasna = finduniques(dv)
function finduniques{T}(da::DataArray{T}) # -> Vector{T}, Bool
    unique_values = Dict{T, Bool}()
    n = length(da)
    hasna = false
    for i in 1:n
        if da.na[i]
            hasna = true
        else
            unique_values[da.data[i]] = true
        end
    end
    return unique_values, hasna
end

#' @description
#'
#' Return a DataVector containing the unique values of a DataArray,
#' including NA if it is encountered.
#'
#' @param da::DataArray{T} DataArray whose unique values are desired.
#'
#' @returns dv::DataVector{T} DataVector containing the unique values
#'          from `da`, including NA if there are any missing entries
#'          in `da`.
#'
#' @examples
#'
#' dv = @data [1, 2, NA, 4]
#' distinct_values = unique(dv)
function Base.unique{T}(da::DataArray{T}) # -> DataVector{T}
    unique_values, hasna = finduniques(da)
    n = length(unique_values)
    if hasna
        res = DataArray(Array(T, n + 1))
        i = 0
        for val in keys(unique_values)
            i += 1
            res.data[i] = val
        end
        res.na[n + 1] = true
        return res
    else
        return DataArray(collect(keys(unique_values)))
    end
end

#' @description
#'
#' Return a DataVector containing the unique values of a DataArray,
#' excluding any NA's.
#'
#' @param da::DataArray{T} DataArray whose unique values are desired.
#'
#' @returns dv::DataVector{T} DataVector containing the unique values
#'          from `da`, excluding any NA's.
#'
#' @examples
#'
#' dv = @data [1, 2, NA, 4]
#' distinct_values = levels(dv)
function levels(da::DataArray) # -> DataVector{T}
    unique_values, hasna = finduniques(da)
    return DataArray(collect(keys(unique_values)))
end
