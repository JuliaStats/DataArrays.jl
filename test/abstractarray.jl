@testset "AbstractArray" begin
    unsorted_dv = @data [2, 1, null]

    # TODO: Make this work
    # tiedrank(dv)

    @test first(unsorted_dv) == 2
    @test isnull(last(unsorted_dv))

    # isnull with AbstractArray
    a = [1, 2, 3]
    @test isnull.(a) == falses(3)
    a = Any[1, 2, null, 3]
    @test isnull.(a) == [false, false, true, false]
    for i = 1:length(a)
        @test isnull(a, i) == isnull.(a)[i]
    end
end
