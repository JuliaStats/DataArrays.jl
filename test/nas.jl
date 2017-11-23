@testset "missings" begin
    @testset "any(ismissing, x)" begin
        # any(ismissing, a::AbstractArray)
        @test any(ismissing, Any[missing, 1])
        @test !any(ismissing, [1, 2])
        @test !any(ismissing, repeat([1, 2], outer = [1, 2]))
        @test !any(ismissing, repeat([1, 2], outer = [1, 2, 2]))

        # any(ismissing, da::DataArray)
        @test !any(ismissing, DataArray([1, 2], falses(2)))
        @test !any(ismissing, DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
        da = DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2))
        @test !any(ismissing, da)
        da[2] = missing
        @test any(ismissing, da)

        # any(ismissing, pda::PooledDataArray)
        @test !any(ismissing, PooledDataArray([1, 2], falses(2)))
        @test !any(ismissing, PooledDataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
        pda = PooledDataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2))
        @test !any(ismissing, pda)
        pda[2] = missing
        @test any(ismissing, pda)
    end

    @testset "all(ismissing, x)" begin
        # all(ismissing, a::AbstractArray)
        @test all(ismissing, Any[missing, missing])
        @test !all(ismissing, Any[missing, 1])
        @test !all(ismissing, [1, 2])
        @test !all(ismissing, repeat([1, 2], outer = [1, 2]))
        @test !all(ismissing, repeat([1, 2], outer = [1, 2, 2]))

        # all(ismissing, da::DataArray)
        @test !all(ismissing, DataArray([1, 2], falses(2)))
        @test !all(ismissing, DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
        da = DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2))
        da[1] = missing
        @test !all(ismissing, da)
        da[:] = missing
        @test all(ismissing, da)

        # all(ismissing, da::PooledDataArray)
        @test !all(ismissing, PooledDataArray([1, 2], falses(2)))
        @test !all(ismissing, PooledDataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
        pda = PooledDataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2))
        pda[1] = missing
        @test !all(ismissing, pda)
        pda[:] = missing
        @test all(ismissing, pda)
    end

    dv = DataArray(collect(1:6), fill(false, 6))
    a = dv[.!ismissing.(dv)]
    @test collect(Missings.fail(dv)) == a
    @test collect(skipmissing(dv)) == a
    @test collect(Missings.replace(dv, 4)) == a

    dv[[1, 2, end]] = missing

    @testset "promotion" for (T1, T2) in ((Int, Float64),
                                          (Dates.Minute, Dates.Second))
        @eval begin
            @test promote_type($T1, Union{$T2, Missing})              == Union{$T2, Missing}
            @test promote_type(Union{$T1, Missing}, $T2)              == Union{$T2, Missing}
            @test promote_type(Union{$T1, Missing}, Union{$T2, Missing}) == Union{$T2, Missing}
        end
    end

    a = dv[.!ismissing.(dv)]
    @test_throws MissingException for v in Missings.fail(dv); end
    @test collect(skipmissing(dv)) == a
    @test collect(Missings.replace(dv, 4)) == [4, 4, a..., 4]

end
