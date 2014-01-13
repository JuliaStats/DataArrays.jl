abstract AbstractDataArray{T, N} <: AbstractArray{T, N}

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

typealias AbstractDataVector{T} AbstractDataArray{T, 1}
typealias AbstractDataMatrix{T} AbstractDataArray{T, 2}
typealias DataVector{T} DataArray{T, 1}
typealias DataMatrix{T} DataArray{T, 2}

# Need to redefine inner constructor as outer constuctor
function DataArray{T, N}(d::Array{T, N},
                         m::BitArray{N} = falses(size(d)))
    return DataArray{T, N}(d, m)
end

# Convert Array{Bool} NA values to a BitArray
DataArray(d::Array, m::Array{Bool}) = DataArray(d, bitpack(m))

# Convert a BitArray into a DataArray
function DataArray(d::BitArray, m::BitArray = falses(size(d)))
    return DataArray(bitunpack(d), m)
end

# Convert a Ranges object into a DataVector
function DataArray(d::Ranges, m::BitArray = falses(length(d)))
    DataArray(convert(Vector, d), m)
end

# Construct an all-NA DataArray of a specific type
function DataArray(t::Type, dims::Integer...)
    return DataArray(Array(t, dims...), trues(dims...))
end
function DataArray{N}(t::Type, dims::NTuple{N,Int})
    return DataArray(Array(t, dims...), trues(dims...))
end

# Copying
Base.copy(d::DataArray) = DataArray(copy(d.data), copy(d.na))
Base.deepcopy(d::DataArray) = DataArray(deepcopy(d.data), deepcopy(d.na))
function Base.copy!(dest::DataArray, src::Any)
    for i in 1:length(src)
        dest[i] = src[i]
    end
    return dest
end

# Similar array allocation
function Base.similar(d::DataArray, T::Type, dims::Dims)
    DataArray(Array(T, dims), trues(dims))
end

# Size information
Base.size(d::DataArray) = size(d.data)
Base.ndims(d::DataArray) = ndims(d.data)
Base.length(d::DataArray) = length(d.data)
Base.endof(d::DataArray) = endof(d.data)
Base.eltype{T, N}(d::DataArray{T, N}) = T

# Find
function Base.find(da::AbstractDataArray{Bool})
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
function array{T}(da::DataArray{T})
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
function array{T}(da::DataArray{T}, replacement::T)
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
function array{T}(da::DataArray{T}, replacement::Any)
    return array(da, convert(T, replacement))
end

#' @description
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
dropna(v::AbstractVector) = copy(v)

#' @description
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
dropna(dv::DataVector) = copy(dv.data[!dv.na])

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
#typealias MultiIndex Union(Vector, BitVector, Ranges, Range1)
typealias MultiIndex Union(Vector, Ranges, Range1)
typealias BooleanIndex Union(BitVector, Vector{Bool})

# TODO: Solve ambiguity warnings here without
#       ridiculous accumulation of methods

function Base.getindex{S, T}(x::Vector{S}, inds::AbstractDataArray{T})
    return x[array(inds)]
end

function Base.getindex{S, T}(x::Array{S}, inds::AbstractDataArray{T})
    return x[array(inds)]
end

function Base.getindex(d::DataArray, i::SingleIndex)
	if d.na[i]
		return NA
	else
		return d.data[i]
	end
end

# TODO: Return SubDataArray
function Base.getindex(d::DataArray, inds::AbstractDataVector)
    return d[array(inds)]
end

# There are two definitions in order to remove ambiguity warnings
# TODO: Return SubDataArray
function Base.getindex{T <: Number, N}(d::DataArray{T, N}, inds::BooleanIndex)
    return DataArray(d.data[inds], d.na[inds])
end

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

function Base.getindex{T <: Number, N}(d::DataArray{T, N}, inds::MultiIndex)
    return DataArray(d.data[inds], d.na[inds])
end

function Base.getindex(d::DataArray, inds::MultiIndex)
    res = similar(d, length(inds))
    for i in 1:length(inds)
        ix = inds[i]
        if !d.na[ix]
            res[i] = d.data[ix]
        end
    end
    return res
end

# TODO: Return SubDataArray
# TODO: Make inds::AbstractVector
## # The following assumes that T<:Number won't have #undefs
## # There are two definitions in order to remove ambiguity warnings
function Base.getindex{T <: Number, N}(d::DataArray{T, N}, inds::BooleanIndex)
    return DataArray(d.data[inds], d.na[inds])
end

function Base.getindex{T <: Number, N}(d::DataArray{T, N}, inds::MultiIndex)
    DataArray(d.data[inds], d.na[inds])
end

# setindex!()

# d[SingleItemIndex] = NA
function Base.setindex!(da::DataArray, val::NAtype, i::SingleIndex)
	da.na[i] = true
    return NA
end

# d[SingleItemIndex] = Single Item
function Base.setindex!(da::DataArray, val::Any, i::SingleIndex)
	da.data[i], da.na[i] = val, false
    return val
end

# d[MultiIndex] = NA
# TODO: Remove these definitions if ever possible w/o raising warnings
function Base.setindex!(da::DataArray{NAtype},
                        val::NAtype,
                        inds::AbstractVector{Bool})
    throw(ArgumentError("DataArray{NAtype} is incoherent"))
end

function Base.setindex!(da::DataArray{NAtype},
                        val::NAtype,
                        inds::AbstractVector)
    throw(ArgumentError("DataArray{NAtype} is incoherent"))
end

function Base.setindex!(da::DataArray,
                        val::NAtype,
                        inds::AbstractDataVector{Bool})
    da.na[find(array(inds))] = true
    return NA
end

function Base.setindex!(da::DataArray,
                        val::NAtype,
                        inds::AbstractVector{Bool})
    da.na[find(inds)] = true
    return NA
end

function Base.setindex!(da::DataArray,
                        val::NAtype,
                        inds::AbstractVector)
    da.na[inds] = true
    return NA
end

# d[MultiIndex] = Multiple Values
function Base.setindex!(da::AbstractDataArray,
                        vals::AbstractVector,
                        inds::AbstractDataVector{Bool})
    setindex!(da, vals, find(array(inds)))
end

function Base.setindex!(da::AbstractDataArray,
                        vals::AbstractVector,
                        inds::AbstractVector{Bool})
    setindex!(da, vals, find(inds))
end

function Base.setindex!(da::AbstractDataArray,
                        vals::AbstractVector,
                        inds::AbstractVector)
    for (val, ind) in zip(vals, inds)
        da[ind] = val
    end
    return vals
end

# x[MultiIndex] = Single Item
function Base.setindex!{T}(da::AbstractDataArray{T},
                           val::Union(Number, String, T),
                           inds::AbstractDataVector{Bool})
    setindex!(da, val, find(array(inds)))
end

function Base.setindex!{T}(da::AbstractDataArray{T},
                           val::Union(Number, String, T),
                           inds::AbstractVector{Bool})
    setindex!(da, val, find(inds))
end

function Base.setindex!{T}(da::AbstractDataArray{T},
                           val::Union(Number, String, T),
                           inds::AbstractVector)
    val = convert(T, val)
    for ind in inds
        da[ind] = val
    end
    return val
end

function Base.setindex!(da::AbstractDataArray,
                        val::Any,
                        inds::AbstractDataVector{Bool})
    setindex!(da, val, find(array(inds)))
end

function Base.setindex!(da::AbstractDataArray,
                        val::Any,
                        inds::AbstractVector{Bool})
    setindex!(da, val, find(inds))
end

function Base.setindex!{T}(da::AbstractDataArray{T},
                           val::Any,
                           inds::AbstractVector)
    val = convert(T, val)
    for ind in inds
        da[ind] = val
    end
    return val
end

# Predicates

isna(a::AbstractArray) = falses(size(a))

isna(da::DataArray) = copy(da.na)

Base.isnan(da::DataArray) = DataArray(isnan(da.data), copy(da.na))

Base.isfinite(da::DataArray) = DataArray(isfinite(da.data), copy(da.na))

anyna(a::AbstractArray) = false

anyna(d::AbstractDataArray) = any(isna, d)

allna(a::AbstractArray) = false

allna(d::AbstractDataArray) = all(isna, d)

# Generic iteration over AbstractDataArray's

Base.start(x::AbstractDataArray) = 1

Base.next(x::AbstractDataArray, state::Integer) = (x[state], state + 1)

Base.done(x::AbstractDataArray, state::Integer) = state > length(x)

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
function Base.convert{S, T, N}(::Type{Array{S, N}}, x::DataArray{T, N})
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
function Base.convert{T, N}(::Type{Array}, x::DataArray{T, N})
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
function Base.convert{S, T, N}(::Type{DataArray{S, N}}, x::Array{T, N})
    return DataArray(convert(Array{S}, x), falses(size(x)))
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
function Base.convert{T, N}(::Type{DataArray}, a::Array{T, N})
    return DataArray(a, falses(size(a)))
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
function Base.convert{S, T, N}(::Type{DataArray{S, N}}, x::DataArray{T, N})
    return DataArray(convert(Array{S}, x.data), x.na)
end

#' @description
#'
#' NO-OP: See convert(DataArray{S}, DataArray{T}) for rationale.
#'
#' @param da::DataArray{T} The DataArray that will be converted.
#'
#' @returns out::DataArray{T} The converted DataArray.
#'
#' @examples
#'
#' dv = @data [1, 2, NA, 4]
#' dv_alt = convert(DataVector, dv)
function Base.convert{T, N}(::Type{DataArray}, x::DataArray{T, N})
    return DataArray(x.data, x.na)
end

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
# TODO: Rethink multi-item documentation approach
for f in (:(Base.int), :(Base.float), :(Base.bool))
    @eval begin
        function ($f)(da::DataArray)
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
function Base.hash(a::AbstractDataArray)
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
