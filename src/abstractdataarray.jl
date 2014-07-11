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
#' Determine if any of the entries of an DataArray are `NA`.
#'
#' @param da::DataArray{T, N} The DataArray whose elements will
#'        be assessed.
#'
#' @returns out::Bool Are any of the elements of `a` an `NA` value?
#'
#' @examples
#'
#' da = @data([1, 2, 3])
#' anyna(da)
anyna(d::AbstractDataArray) = any(isna, d) # -> Bool

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
#' Determine if all of the entries of an DataArray are `NA`.
#'
#' @param da::DataArray{T, N} The DataArray whose elements will
#'        be assessed.
#'
#' @returns out::Bool Are all of the elements of `a` an `NA` value?
#'
#' @examples
#'
#' da = @data([1, 2, 3])
#' allna(da)
allna(d::AbstractDataArray) = all(isna, d) # -> Bool

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
isna(a::AbstractArray) = falses(size(a)) # -> BitArray

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
isna(a::AbstractArray, i::Real) = false # -> Bool

