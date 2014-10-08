module TestStats
    using Base.Test
    using DataArrays

    autocor(DataArray([1, 2, 3, 4, 5]))

    @assert isequal(xtabs([1, 2, 2, 2, 3]), Dict([(2, 3), (3, 1), (1, 1)]))
end
