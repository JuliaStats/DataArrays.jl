@testset "Booleans" begin
    @test null | true === true
    @test isnull(null | false)
    @test isnull(null | null)
    @test true | null === true
    @test isnull(false | null)

    @test isnull(null & true)
    @test null & false === false
    @test isnull(null & null)
    @test isnull(true & null)
    @test false & null === false

    @test isnull(null ⊻ true)
    @test isnull(null ⊻ false)
    @test isnull(null ⊻ null)
    @test isnull(true ⊻ null)
    @test isnull(false ⊻ null)

    @test any((@data [1, 2, null]) .== 1) === true
    @test any((@data [null, 1, 2]) .== 1) === true
    @test any((@data [1, 2, null]) .== 3) === false
    @test any((@data [1, 2, 3] ).== 4) === false

    @test all((@data [1, 1, null]) .== 1) === false
    @test all((@data [null, 1, 1]) .== 1) === false
    @test all((@data [1, 1, 1]) .== 1) === true
    @test all((@data [1, 2, 1]) .== 1) === false
end
