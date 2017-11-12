@testset "Booleans" begin
    @test missing | true === true
    @test ismissing(missing | false)
    @test ismissing(missing | missing)
    @test true | missing === true
    @test ismissing(false | missing)

    @test ismissing(missing & true)
    @test missing & false === false
    @test ismissing(missing & missing)
    @test ismissing(true & missing)
    @test false & missing === false

    @test ismissing(missing ⊻ true)
    @test ismissing(missing ⊻ false)
    @test ismissing(missing ⊻ missing)
    @test ismissing(true ⊻ missing)
    @test ismissing(false ⊻ missing)

    @test any((@data [1, 2, missing]) .== 1) === true
    @test any((@data [missing, 1, 2]) .== 1) === true
    @test ismissing(any((@data [1, 2, missing]) .== 3))
    @test any((@data [1, 2, 3] ).== 4) === false

    @test ismissing(all((@data [1, 1, missing]) .== 1))
    @test ismissing(all((@data [missing, 1, 1]) .== 1))
    @test all((@data [1, 1, 1]) .== 1) === true
    @test all((@data [1, 2, 1]) .== 1) === false
end
