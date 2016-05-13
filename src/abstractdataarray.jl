#' @description
#'
#' An AbstractDataArray is an Array whose entries can take on
#' values of type `T` or the value `NA`.
abstract AbstractDataArray{T, N} <: AbstractArray{T, N}

#' @description
#'
#' An AbstractDataVector is an AbstractDataArray of order 1.
typealias AbstractDataVector{T} AbstractDataArray{T, 1}

#' @description
#'
#' An AbstractDataMatrix is an AbstractDataArray of order 2.
typealias AbstractDataMatrix{T} AbstractDataArray{T, 2}

#' @description
#' Determine the type of the elements of an AbstractDataArray.
#'
#' @param ada::AbstractDataArray{T} The AbstractDataArray whose
#'        element type is desired.
#'
#' @returns T::DataType The type of the non-NA elements of `ada`.
#'
#' @examples
#'
#' dv = @data [false, false, true, false]
#' T = eltype(dv)
Base.eltype{T, N}(d::AbstractDataArray{T, N}) = T

# Generic iteration over AbstractDataArray's

# TODO: Document
Base.start(x::AbstractDataArray) = 1

# TODO: Document
Base.next(x::AbstractDataArray, state::Integer) = (x[state], state + 1)

# TODO: Document
Base.done(x::AbstractDataArray, state::Integer) = state > length(x)

#' @description
#'
#' Determine if the values of an AbstractArray are `NA`.
#'
#' @param a::AbstractArray{T, N} The AbstractArray whose missingness will
#'        be assessed.
#'
#' @returns na::BitArray{N} Elementwise Boolean whether entry is missing.
#'
#' @examples
#'
#' a = [1, 2, 3]
#' isna(a)
isna{T}(a::AbstractArray{T}) =
    NAtype <: T ? BitArray(map(x->isa(x, NAtype), a)) : falses(size(a)) # -> BitArray

#' @description
#'
#' Safe and type-stable way to determine if element `i` of an
#' AbstractArray is `NA`.
#'
#' @param a::AbstractArray The AbstractArray whose missingness will
#'        be assessed.
#' @param i::Integer The index of the element to be checked for `NA`.
#'
#' @returns na::Bool Is the element `NA` or not?
#'
#' @examples
#'
#' a = [1, 2, 3]
#' isna(a, 1)
isna{T}(a::AbstractArray{T}, i::Real) = NAtype <: T ? isa(a[i], NAtype) : false # -> Bool

#' @description
#'
#' Determine if any of the entries of an AbstractArray are `NA`.
#'
#' @param a::AbstractArray{T, N} The AbstractArray whose elements will
#'        be assessed.
#'
#' @returns out::Bool Are any of the elements of `a` an `NA` value?
#'
#' @examples
#'
#' a = [1, 2, 3]
#' anyna(a)
anyna(a::AbstractArray) = false # -> Bool

#' @description
#'
#' Determine if all of the entries of an AbstractArray are `NA`.
#'
#' @param a::AbstractArray{T, N} The AbstractArray whose elements will
#'        be assessed.
#'
#' @returns out::Bool Are all of the elements of `a` an `NA` value?
#'
#' @examples
#'
#' a = [1, 2, 3]
#' allna(a)
allna(a::AbstractArray) = false # -> Bool

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

# Iterators
# TODO: Use values()
#       Use DataValueIterator type?

type EachFailNA{T}
    da::AbstractDataArray{T}
end
each_failna{T}(da::AbstractDataArray{T}) = EachFailNA(da)
Base.length(itr::EachFailNA) = length(itr.da)
Base.start(itr::EachFailNA) = 1
Base.done(itr::EachFailNA, ind::Integer) = ind > length(itr)
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
function _next_nonna_ind{T}(da::AbstractDataArray{T}, ind::Int)
    ind += 1
    while ind <= length(da) && isna(da, ind)
        ind += 1
    end
    ind
end
Base.length(itr::EachDropNA) = length(itr.da) - sum(itr.da.na)
Base.start(itr::EachDropNA) = _next_nonna_ind(itr.da, 0)
Base.done(itr::EachDropNA, ind::Int) = ind > length(itr.da)
function Base.next(itr::EachDropNA, ind::Int)
    (itr.da[ind], _next_nonna_ind(itr.da, ind))
end

type EachReplaceNA{S, T}
    da::AbstractDataArray{S}
    replacement::T
end
function each_replacena(da::AbstractDataArray, replacement::Any)
    EachReplaceNA(da, convert(eltype(da), replacement))
end
function each_replacena(replacement::Any)
    x -> each_replacena(x, replacement)
end
Base.length(itr::EachReplaceNA) = length(itr.da)
Base.start(itr::EachReplaceNA) = 1
Base.done(itr::EachReplaceNA, ind::Integer) = ind > length(itr)
function Base.next(itr::EachReplaceNA, ind::Integer)
    item = isna(itr.da, ind) ? itr.replacement : itr.da[ind]
    (item, ind + 1)
end