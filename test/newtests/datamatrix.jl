module TestDataMatrixs
    using Base.Test
    using DataArrays

    # Base.getindex(d::DataMatrix, i::SingleIndex, j::SingleIndex)
    dm = @data([1 2; missing 4])
    dm[1, 2]

    # Base.getindex(x::DataMatrix, i::SingleIndex, col_inds::AbstractDataVector{Bool})
    dm = @data([1 2; missing 4])
    dm[1, @data([true, false])]

    # Base.getindex(x::DataMatrix, i::SingleIndex, col_inds::AbstractDataVector)
    dm = @data([1 2; missing 4])
    dm[1, @data([1, 2])]

    # Base.getindex(x::DataMatrix, i::SingleIndex, col_inds::MultiIndex)
    dm = @data([1 2; missing 4])
    dm[1, [1, 2]]

    # Base.getindex(x::DataMatrix, row_inds::AbstractDataVector{Bool}, j::SingleIndex)
    dm = @data([1 2; missing 4])
    dm[@data([true, false]), 1]

    # Base.getindex(x::DataMatrix, row_inds::AbstractDataVector, j::SingleIndex)
    dm = @data([1 2; missing 4])
    dm[@data([1, 2]), 1]

    # Base.getindex(x::DataMatrix, row_inds::MultiIndex, j::SingleIndex)
    dm = @data([1 2; missing 4])
    dm[[1, 2], 1]

    # Base.getindex(x::DataMatrix, row_inds::AbstractDataVector{Bool}, col_inds::AbstractDataVector{Bool})
    dm = @data([1 2; missing 4])
    dm[@data([true, false]), @data([true, false])]

    # Base.getindex(x::DataMatrix, row_inds::AbstractDataVector{Bool}, col_inds::AbstractDataVector)
    dm = @data([1 2; missing 4])
    dm[@data([true, false]), @data([1, 2])]

    # Base.getindex(x::DataMatrix, row_inds::AbstractDataVector{Bool}, col_inds::MultiIndex)
    dm = @data([1 2; missing 4])
    dm[@data([true, false]), [1, 2]]

    # Base.getindex(x::DataMatrix, row_inds::AbstractDataVector, col_inds::AbstractDataVector{Bool})
    dm = @data([1 2; missing 4])
    dm[@data([1, 2]), @data([true, false])]

    # Base.getindex(x::DataMatrix, row_inds::AbstractDataVector, col_inds::AbstractDataVector)
    dm = @data([1 2; missing 4])
    dm[@data([1, 2]), @data([1, 2])]

    # Base.getindex(x::DataMatrix, row_inds::AbstractDataVector, col_inds::MultiIndex)
    dm = @data([1 2; missing 4])
    dm[@data([1, 2]), [1, 2]]

    # Base.getindex(x::DataMatrix, row_inds::MultiIndex, col_inds::AbstractDataVector{Bool})
    dm = @data([1 2; missing 4])
    dm[[1, 2], @data([true, false])]

    # Base.getindex(x::DataMatrix, row_inds::MultiIndex, col_inds::AbstractDataVector)
    dm = @data([1 2; missing 4])
    dm[[1, 2], @data([1, 2])]

    # Base.getindex(x::DataMatrix, row_inds::MultiIndex, col_inds::MultiIndex)
    dm = @data([1 2; missing 4])
    dm[[1, 2], [1, 2]]

    # Base.setindex!(dm::DataMatrix, val::Missing, i::SingleIndex, j::SingleIndex)
    dm = @data([1 2; missing 4])
    dm[1, 1] = missing

    # Base.setindex!(dm::DataMatrix, val::Any, i::SingleIndex, j::SingleIndex)
    dm = @data([1 2; missing 4])
    dm[1, 1] = 3

    # Base.setindex!(dm::DataMatrix, val::Missing, row_inds::MultiIndex, j::SingleIndex)
    dm = @data([1 2; missing 4])
    dm[[1, 2], 1] = 3

    # Base.setindex!{S, T}(dm::DataMatrix{S}, vals::Vector{T}, row_inds::MultiIndex, j::SingleIndex)
    dm = @data([1 2; missing 4])
    dm[[1, 2], 1] = [3, 4]

    # Base.setindex!(dm::DataMatrix, val::Any, row_inds::MultiIndex, j::SingleIndex)
    dm = @data([1 2; missing 4])
    dm[[1, 2], 1] = 3

    # Base.setindex!(dm::DataMatrix, val::Missing, i::SingleIndex, col_inds::MultiIndex)
    dm = @data([1 2; missing 4])
    dm[[1, 2], 1] = missing

    # Base.setindex!{S, T}(dm::DataMatrix{S}, vals::Vector{T}, i::SingleIndex, col_inds::MultiIndex)
    dm = @data([1 2; missing 4])
    dm[1, [1, 2]] = [3, 4]

    # Base.setindex!(dm::DataMatrix, val::Any, i::SingleIndex, col_inds::MultiIndex)
    dm = @data([1 2; missing 4])
    dm[1, [1, 2]] = 3

    # Base.setindex!(dm::DataMatrix, val::Missing, row_inds::MultiIndex, col_inds::MultiIndex)
    dm = @data([1 2; missing 4])
    dm[[1, 2], [1, 2]] = missing

    # Base.setindex!{S, T}(dm::DataMatrix{S}, vals::Vector{T}, row_inds::MultiIndex, col_inds::MultiIndex)
    dm = @data([1 2; missing 4])
    dm[[1, 2], [1, 2]] = [1, 2, 3, 4]

    # Base.setindex!(dm::DataMatrix, val::Any, row_inds::MultiIndex, col_inds::MultiIndex)
    dm = @data([1 2; missing 4])
    dm[[1, 2], [1, 2]] = 5

    # Base.diag{T}(dm::DataMatrix{T})
    dm = @data([1 2; missing 4])
    diag(dm)
end
