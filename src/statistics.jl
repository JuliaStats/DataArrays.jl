# This is multiplicative analog of diff
function reldiff{T}(v::Vector{T})
    n = length(v)
    res = Array(T, n - 1)
    for i in 2:n
        res[i - 1] = v[i] / v[i - 1]
    end
    return res
end

# Diff scaled by previous value
function percent_change{T}(v::Vector{T})
    n = length(v)
    res = Array(T, n - 1)
    for i in 2:n
        res[i - 1] = (v[i] - v[i - 1]) / v[i - 1]
    end
    return res
end

autocor{T}(dv::DataVector{T}, lag::Int) = cor(dv[1:(end - lag)], dv[(1 + lag):end])
autocor{T}(dv::DataVector{T}) = autocor(dv, 1)

# Generate levels - see the R documentation for gl
function gl(n::Integer, k::Integer, l::Integer)
    nk = n * k
    d, r = divrem(l, nk)
    r == 0 || throw(ArgumentError("length out must be a multiple of n * k"))
    aa = Array(Int, l)
    for j = 0:(d - 1), i = 1:n
        aa[j * nk + (i - 1) * k + (1:k)] = i
    end
    compact(PooledDataArray(aa))
end

gl(n::Integer, k::Integer) = gl(n, k, n*k)

#' @description
#'
#' A cross-tabulation type. Currently, this is just a one-way table.
#'
#' @field vals::Array{T} The values of the original data
#' @field counts::Vector{Int} The counts corresponding to the values
type Xtab{T}
    vals::Array{T}
    counts::Vector{Int}
end

#' @description
#'
#' Create a new cross-tabulation from the array `x`
#' This is currently just for one-way tables.
#'
#' @param x::AbstractDataArray The AbstractDataArray with the data
#'
#' @returns out::Xtab The `Xtab` corresponding to `x`
function xtab{T}(x::AbstractArray{T})
    d = xtabs(x)
    kk = sort(collect(keys(d)))
    cc = Array(Int, length(kk))
    for i in 1:length(kk)
        cc[i] = d[kk[i]]
    end
    return Xtab(kk, cc)
end

#' @description
#'
#' A cross-tabulation function that returns the results as a Dict
#' This is currently just for one-way tables.
#'
#' @param x::AbstractDataArray The AbstractDataArray with the data
#'
#' @returns out::Dict The values and their counts
function xtabs{T}(x::AbstractArray{T})
    d = Dict{T, Int}()
    for el in x
        d[el] = get(d, el, 0) + 1
    end
    return d
end
