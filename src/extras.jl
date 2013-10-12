function Stats.table{T}(d::AbstractDataArray{T})
    counts = Dict{Union(T, NAtype), Int}()
    for i = 1:length(d)
        if haskey(counts, d[i])
            counts[d[i]] += 1
        else
            counts[d[i]] = 1
        end
    end
    return counts
end

function cut{S, T}(x::AbstractVector{S}, breaks::Vector{T})
    if !issorted(breaks)
        sort!(breaks)
    end
    min_x, max_x = min(x), max(x)
    if breaks[1] > min_x
        unshift!(breaks, min_x)
    end
    if breaks[end] < max_x
        push!(breaks, max_x)
    end
    refs = fill(zero(DEFAULT_POOLED_REF_TYPE), length(x))
    for i in 1:length(x)
        if x[i] == min_x
            refs[i] = 1
        else
            refs[i] = searchsortedfirst(breaks, x[i]) - 1
        end
    end
    n = length(breaks)
    from = map(x -> sprint(showcompact, x), breaks[1:(n - 1)])
    to = map(x -> sprint(showcompact, x), breaks[2:n])
    pool = Array(ASCIIString, n - 1)
    if breaks[1] == min_x
        pool[1] = string("[", from[1], ",", to[1], "]")
    else
        pool[1] = string("(", from[1], ",", to[1], "]")
    end
    for i in 2:(n - 1)
        pool[i] = string("(", from[i], ",", to[i], "]")
    end
    PooledDataArray(RefArray(refs), pool)
end

cut(x::AbstractVector, ngroups::Integer) = cut(x, quantile(x, [1 : ngroups - 1] / ngroups))

function rep{T <: Integer}(x::AbstractVector, lengths::AbstractVector{T})
    if length(x) != length(lengths)
        error("vector lengths must match")
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

function rep(x::AbstractVector, times::Integer = 1, each::Integer = 1)
    res = similar(x, each * times * length(x))
    i = 1
    for jdx in 1:times
        for idx in 1:length(x)
            tmp = x[idx]
            for kdx in 1:each
                res[i] = tmp
                i += 1
            end
        end
    end
    return res
end

function rep(x::AbstractVector; times::Integer = 1, each::Integer = 1)
    rep(x, times, each)
end

rep(x::Any, times::Integer) = fill(x, times)
