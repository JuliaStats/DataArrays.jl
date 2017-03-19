@testset "Booleans" begin
    @test NA | true == true
    @test isna(NA | false)
    @test isna(NA | NA)
    @test true | NA == true
    @test isna(false | NA)

    @test isna(NA & true)
    @test NA & false == false
    @test isna(NA & NA)
    @test isna(true & NA)
    @test false & NA == false

    @test any((@data [1, 2, NA]) .== 1) == true
    @test any((@data [NA, 1, 2]) .== 1) == true
    @test isna(any((@data [1, 2, NA]) .== 3))
    @test any((@data [1, 2, 3] ).== 4) == false

    @test isna(all((@data [1, 1, NA]) .== 1))
    @test isna(all((@data [NA, 1, 1]) .== 1))
    @test all((@data [1, 1, 1]) .== 1) == true
    @test all((@data [1, 2, 1]) .== 1) == false
end
