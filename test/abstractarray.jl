module TestAbstractArray
    using Base.Test
    using DataArrays

    unsorted_dv = @data [2, 1, NA]

    # TODO: Make this work
    # tiedrank(dv)

    @assert first(unsorted_dv) == 2
    @assert isna(last(unsorted_dv))

    # isna with AbstractArray
    a = [1, 2, 3]
    @test isna(a) == falses(3)
    a = Any[1, 2, NA, 3]
    @test isna(a) == [false, false, true, false]
    for i = 1:length(a)
	    @test isna(a, i) == isna(a)[i]
	end
end
