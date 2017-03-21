@testset "NAs" begin
    @testset "any(isna, x)" begin
        # any(isna, a::AbstractArray)
        @test any(isna, Any[NA, 1])
        @test !any(isna, [1, 2])
        @test !any(isna, repeat([1, 2], outer = [1, 2]))
        @test !any(isna, repeat([1, 2], outer = [1, 2, 2]))

        # any(isna, da::DataArray)
        @test !any(isna, DataArray([1, 2], falses(2)))
        @test !any(isna, DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
        da = DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2))
        @test !any(isna, da)
        da[2] = NA
        @test any(isna, da)

        # any(isna, pda::PooledDataArray)
        @test !any(isna, PooledDataArray([1, 2], falses(2)))
        @test !any(isna, PooledDataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
        pda = PooledDataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2))
        @test !any(isna, pda)
        pda[2] = NA
        @test any(isna, pda)
    end

    @testset "all(isna, x)" begin
        # all(isna, a::AbstractArray)
        @test all(isna, Any[NA, NA])
        @test !all(isna, Any[NA, 1])
        @test !all(isna, [1, 2])
        @test !all(isna, repeat([1, 2], outer = [1, 2]))
        @test !all(isna, repeat([1, 2], outer = [1, 2, 2]))

        # all(isna, da::DataArray)
        @test !all(isna, DataArray([1, 2], falses(2)))
        @test !all(isna, DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
        da = DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2))
        da[1] = NA
        @test !all(isna, da)
        da[:] = NA
        @test all(isna, da)

        # all(isna, da::PooledDataArray)
        @test !all(isna, PooledDataArray([1, 2], falses(2)))
        @test !all(isna, PooledDataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
        pda = PooledDataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2))
        pda[1] = NA
        @test !all(isna, pda)
        pda[:] = NA
        @test all(isna, pda)
    end

    dv = DataArray(collect(1:6), fill(false, 6))
    a = dropna(dv)
    @test collect(each_failna(dv)) == a
    @test collect(each_dropna(dv)) == a
    @test collect(each_replacena(dv, 4)) == a

    dv[[1, 2, end]] = NA

    a = dropna(dv)
    @test_throws NAException for v in each_failna(dv); end
    @test collect(each_dropna(dv)) == a
    @test collect(each_replacena(dv, 4)) == [4, 4, a..., 4]
end
