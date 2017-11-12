@testset "Padding" begin
    dv = @data ones(3)
    @test isequal(dv, padmissing(dv, 0, 0))
    @test length(padmissing(dv, 2, 0)) == length(dv) + 2
    @test length(padmissing(dv, 0, 2)) == length(dv) + 2
    @test length(padmissing(dv, 2, 2)) == length(dv) + 4
end
