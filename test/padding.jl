@testset "Padding" begin
    dv = @data ones(3)
    @test isequal(dv, padNA(dv, 0, 0))
    @test length(padNA(dv, 2, 0)) == length(dv) + 2
    @test length(padNA(dv, 0, 2)) == length(dv) + 2
    @test length(padNA(dv, 2, 2)) == length(dv) + 4
end
