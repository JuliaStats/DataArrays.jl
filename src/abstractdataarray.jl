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

#' @description
#' Copy the elements of `src` into the AbstractDataArray, `dest`.
#' 
#' @param dest::AbstractDataArray The AbstractDataArray that will
#'        be written into.
#' @param src::Any The iterable object whose contents will be copied
#'        into `dest`.
#'
#' @returns dest::AbstractDataArray The modified version of `dest` is
#'          returned for convenience.
#'
#' @examples
#'
#' dv = @data [false, false, true, false]
#' v = [true, true, true, false]
#' copy!(dv, v)
function Base.copy!(dest::AbstractDataArray, src::Any)
    for i in 1:length(src)
        dest[i] = src[i]
    end
    return dest
end
