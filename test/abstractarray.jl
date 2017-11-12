@testset "AbstractArray" begin
    unsorted_dv = @data [2, 1, missing]

    # TODO: Make this work
    # tiedrank(dv)

    @test first(unsorted_dv) == 2
    @test ismissing(last(unsorted_dv))

    # ismissing with AbstractArray
    a = [1, 2, 3]
    @test ismissing.(a) == falses(3)
    a = Any[1, 2, missing, 3]
    @test ismissing.(a) == [false, false, true, false]
    for i = 1:length(a)
        @test ismissing(a, i) == ismissing.(a)[i]
    end
end
