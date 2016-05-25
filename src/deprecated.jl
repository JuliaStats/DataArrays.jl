# Note: These methods need a more helpfull error message than a `NoMethodError`,
#       when the deprecation is removed
import Base.@deprecate
import Base.Operators: /
@deprecate (/)(x::(@compat Union{NAtype,Number}),A::AbstractDataArray)    x ./ A

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
#' v = convert(Vector, dv)
#'
#' dm = @data [1 2; 3 4]
#' m = convert(Matrix, dm)
function array{T}(da::DataArray{T}) # -> Array{T}
    Base.depwarn(
        """
        array(da::DataArray{T}) is deprecated.
        Use convert(Array, da).
        """,
        :array
    )
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
#' v = convert(Vector, dv, 3)
#'
#' dm = @data [1 2; NA 4]
#' m = convert(Matrix, dm, 3)
function array{T}(da::DataArray{T}, replacement::Any) # -> Array{T}
    Base.depwarn(
        """
        array(da::DataArray{T}, replacement::Any) is deprecated.
        Use convert(Array, da, replacement) instead.
        """,
        :array
    )
    res = Array(T, size(da))
    replacementT = convert(T, replacement)
    for i in 1:length(da)
        if da.na[i]
            res[i] = replacementT
        else
            res[i] = da.data[i]
        end
    end
    return res
end

# Turn a PooledDataArray into an Array. Fail on NA
function array{T, R}(da::PooledDataArray{T, R})
    Base.depwarn(
        """
        array(pda::PooledDataArray{T, R}) is deprecated.
        Use convert(Array, pda) instead.
        """,
        :array
    )
    n = length(da)
    res = Array(T, size(da))
    for i in 1:n
        if da.refs[i] == zero(R)
            throw(NAException())
        else
            res[i] = da.pool[da.refs[i]]
        end
    end
    return res
end

function array{T, R}(da::PooledDataArray{T, R}, replacement::T)
    Base.depwarn(
        """
        array(pda::PooledDataArray{T, R}, replacement::T) is deprecated.
        Use convert(Array, pda, replacement) instead.
        """,
        :array
    )
    n = length(da)
    res = Array(T, size(da))
    for i in 1:n
        if da.refs[i] == zero(R)
            res[i] = replacement
        else
            res[i] = da.pool[da.refs[i]]
        end
    end
    return res
end

@deprecate head(dv::AbstractDataVector) dv[1:min(6, end)]
@deprecate tail(dv::AbstractDataVector) dv[max(end-6, 1):end]

function rep{T <: Integer}(x::AbstractVector, lengths::AbstractVector{T})
    Base.depwarn(
        """
        rep{T <: Integer}(x::AbstractVector, lengths::AbstractVector{T}) is deprecated.
        """,
        :rep
    )
    if length(x) != length(lengths)
        throw(DimensionMismatch("vector lengths must match"))
    end
    res = similar(x, sum(lengths))
    i = 1
    for idx in 1:length(x)
        tmp = x[idx]
        for kdx in 1:lengths[idx]
            res[i] = tmp
            i += 1
        end
    end
    return res
end

@deprecate rep(x::AbstractVector, times::Integer, each::Integer = 1) Compat.repeat(x; inner=each, outer=times)
@deprecate rep(x::AbstractVector; times::Integer = 1, each::Integer = 1) Compat.repeat(x; inner=each, outer=times)
@deprecate rep(x::Any, times::Integer) Compat.repeat(x; inner=times)
