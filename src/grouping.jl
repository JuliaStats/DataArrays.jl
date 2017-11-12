function groupsort_indexer(x::AbstractVector, ngroups::Integer, missinglast::Bool=false)
    # translated from Wes McKinney's groupsort_indexer in pandas (file: src/groupby.pyx).

    # count group sizes, location 0 for missing
    n = length(x)
    # counts = x.pool
    counts = fill(0, ngroups + 1)
    for i = 1:n
        counts[x[i] + 1] += 1
    end

    # mark the start of each contiguous group of like-indexed data
    where = fill(1, ngroups + 1)
    if missinglast
        for i = 3:ngroups+1
            where[i] = where[i - 1] + counts[i - 1]
        end
        where[1] = where[end] + counts[end]
    else
        for i = 2:ngroups+1
            where[i] = where[i - 1] + counts[i - 1]
        end
    end

    # this is our indexer
    result = fill(0, n)
    for i = 1:n
        label = x[i] + 1
        result[where[label]] = i
        where[label] += 1
    end
    result, where, counts
end

groupsort_indexer(pv::PooledDataVector, missinglast::Bool=false) = groupsort_indexer(pv.refs, length(pv.pool), missinglast)
