module TestStats
    using Base.Test
    using DataArrays

    autocor(DataArray([1, 2, 3, 4, 5]))

    xtab_data = [1, 2, 2, 2, 3]
    @assert isequal(xtabs(xtab_data), Dict([(2, 3), (3, 1), (1, 1)]))

    xt = xtab(xtab_data)
    # Testing that the keys are correct and sorted
    @assert xt.vals == [1, 2, 3]
    # Testing that the counts are correct
    @assert xt.counts == [1, 3, 1]
end
