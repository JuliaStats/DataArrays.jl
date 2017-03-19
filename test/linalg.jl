@testset "LinAlg" begin
    d = @data eye(3, 3)
    d[1, 1] = NA

    @test_nowarn svd(d)
end
