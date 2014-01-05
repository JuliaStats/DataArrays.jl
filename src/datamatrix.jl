# dm[SingleItemIndex, SingleItemIndex)
function Base.getindex(d::DataMatrix,
                       i::SingleIndex,
                       j::SingleIndex)
    if d.na[i, j]
        return NA
    else
        return d.data[i, j]
    end
end

# dm[SingleItemIndex, MultiItemIndex]
function Base.getindex(x::DataMatrix,
                       i::SingleIndex,
                       col_inds::AbstractDataVector{Bool})
    getindex(x, i, find(col_inds))
end
function Base.getindex(x::DataMatrix,
                       i::SingleIndex,
                       col_inds::AbstractDataVector)
    getindex(x, i, dropna(col_inds))
end
# TODO: Make inds::AbstractVector
function Base.getindex(x::DataMatrix,
                       i::SingleIndex,
                       col_inds::MultiIndex)
    DataArray(x.data[i, col_inds], x.na[i, col_inds])
end

# dm[MultiItemIndex, SingleItemIndex]
function Base.getindex(x::DataMatrix,
                       row_inds::AbstractDataVector{Bool},
                       j::SingleIndex)
    getindex(x, find(row_inds), j)
end
function Base.getindex(x::DataMatrix,
                       row_inds::AbstractDataVector,
                       j::SingleIndex)
    getindex(x, dropna(row_inds), j)
end
# TODO: Make inds::AbstractVector
function Base.getindex(x::DataMatrix,
                       row_inds::MultiIndex,
                       j::SingleIndex)
    DataArray(x.data[row_inds, j], x.na[row_inds, j])
end

# dm[MultiItemIndex, MultiItemIndex]
function Base.getindex(x::DataMatrix,
                       row_inds::AbstractDataVector{Bool},
                       col_inds::AbstractDataVector{Bool})
    return getindex(x,
                    find(row_inds),
                    find(col_inds))
end
function Base.getindex(x::DataMatrix,
                       row_inds::AbstractDataVector{Bool},
                       col_inds::AbstractDataVector)
    return getindex(x,
                    find(row_inds),
                    dropna(col_inds))
end
# TODO: Make inds::AbstractVector
function Base.getindex(x::DataMatrix,
                       row_inds::AbstractDataVector{Bool},
                       col_inds::MultiIndex)
    return getindex(x,
                    find(row_inds),
                    col_inds)
end
function Base.getindex(x::DataMatrix,
                       row_inds::AbstractDataVector,
                       col_inds::AbstractDataVector{Bool})
    return getindex(x,
                    dropna(row_inds),
                    find(col_inds))
end
function Base.getindex(x::DataMatrix,
                       row_inds::AbstractDataVector,
                       col_inds::AbstractDataVector)
    return getindex(x,
                    dropna(row_inds),
                    dropna(col_inds))
end

# TODO: Make inds::AbstractVector
function Base.getindex(x::DataMatrix,
                       row_inds::AbstractDataVector,
                       col_inds::MultiIndex)
    return getindex(x, dropna(row_inds), col_inds)
end

# TODO: Make inds::AbstractVector
function Base.getindex(x::DataMatrix,
                       row_inds::MultiIndex,
                       col_inds::AbstractDataVector{Bool})
    return getindex(x,
                    row_inds,
                    find(col_inds))
end

# TODO: Make inds::AbstractVector
function Base.getindex(x::DataMatrix,
                       row_inds::MultiIndex,
                       col_inds::AbstractDataVector)
    return getindex(x, row_inds, dropna(col_inds))
end

# TODO: Make inds::AbstractVector
function Base.getindex(x::DataMatrix,
                       row_inds::MultiIndex,
                       col_inds::MultiIndex)
    return DataArray(x.data[row_inds, col_inds],
                     x.na[row_inds, col_inds])
end

# dm[SingleItemIndex, SingleItemIndex] = NA
function Base.setindex!(dm::DataMatrix,
                        val::NAtype,
                        i::SingleIndex,
                        j::SingleIndex)
    dm.na[i, j] = true
    return NA
end

# dm[SingleItemIndex, SingleItemIndex] = Single Item
function Base.setindex!(dm::DataMatrix,
                        val::Any,
                        i::SingleIndex,
                        j::SingleIndex)
    dm.data[i, j] = val
    dm.na[i, j] = false
    return val
end

# dm[MultiItemIndex, SingleItemIndex] = NA
function Base.setindex!(dm::DataMatrix,
                        val::NAtype,
                        row_inds::MultiIndex,
                        j::SingleIndex)
    dm.na[row_inds, j] = true
    return NA
end

# dm[MultiItemIndex, SingleItemIndex] = Multiple Items
function Base.setindex!{S, T}(dm::DataMatrix{S},
                              vals::Vector{T},
                              row_inds::MultiIndex,
                              j::SingleIndex)
    dm.data[row_inds, j] = vals
    dm.na[row_inds, j] = false
    return vals
end

# dm[MultiItemIndex, SingleItemIndex] = Single Item
function Base.setindex!(dm::DataMatrix,
                        val::Any,
                        row_inds::MultiIndex,
                        j::SingleIndex)
    dm.data[row_inds, j] = val
    dm.na[row_inds, j] = false
    return val
end

# dm[SingleItemIndex, MultiItemIndex] = NA
function Base.setindex!(dm::DataMatrix,
                        val::NAtype,
                        i::SingleIndex,
                        col_inds::MultiIndex)
    dm.na[i, col_inds] = true
    return NA
end

# dm[SingleItemIndex, MultiItemIndex] = Multiple Items
function Base.setindex!{S, T}(dm::DataMatrix{S},
                              vals::Vector{T},
                              i::SingleIndex,
                              col_inds::MultiIndex)
    dm.data[i, col_inds] = vals
    dm.na[i, col_inds] = false
    return vals
end

# dm[SingleItemIndex, MultiItemIndex] = Single Item
function Base.setindex!(dm::DataMatrix,
                        val::Any,
                        i::SingleIndex,
                        col_inds::MultiIndex)
    dm.data[i, col_inds] = val
    dm.na[i, col_inds] = false
    return val
end

# dm[MultiItemIndex, MultiItemIndex] = NA
function Base.setindex!(dm::DataMatrix,
                        val::NAtype,
                        row_inds::MultiIndex,
                        col_inds::MultiIndex)
    dm.na[row_inds, col_inds] = true
    return NA
end

# dm[MultiIndex, MultiIndex] = Multiple Items
function Base.setindex!{S, T}(dm::DataMatrix{S},
                              vals::Vector{T},
                              row_inds::MultiIndex,
                              col_inds::MultiIndex)
    dm.data[row_inds, col_inds] = vals
    dm.na[row_inds, col_inds] = false
    return vals
end

# dm[MultiItemIndex, MultiItemIndex] = Single Item
function Base.setindex!(dm::DataMatrix,
                        val::Any,
                        row_inds::MultiIndex,
                        col_inds::MultiIndex)
    dm.data[row_inds, col_inds] = val
    dm.na[row_inds, col_inds] = false
    return val
end

# Extract the matrix diagonal
Base.diag{T}(dm::DataMatrix{T}) = DataArray(diag(dm.data), diag(dm.na))
