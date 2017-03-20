@testset "NAs" begin
    @testset "anyna(x)" begin
        # anyna(a::AbstractArray)
        @test anyna(Any[NA, 1])
        @test !anyna([1, 2])
        @test !anyna(repeat([1, 2], outer = [1, 2]))
        @test !anyna(repeat([1, 2], outer = [1, 2, 2]))

        # anyna(da::DataArray)
        @test !anyna(DataArray([1, 2], falses(2)))
        @test !anyna(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
        da = DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2))
        @test !anyna(da)
        da[2] = NA
        @test anyna(da)

        # anyna(pda::PooledDataArray)
        @test !anyna(PooledDataArray([1, 2], falses(2)))
        @test !anyna(PooledDataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
        pda = PooledDataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2))
        @test !anyna(pda)
        pda[2] = NA
        @test anyna(pda)
    end

    @testset "allna(x)" begin
        # allna(a::AbstractArray)
        @test allna(Any[NA, NA])
        @test !allna(Any[NA, 1])
        @test !allna([1, 2])
        @test !allna(repeat([1, 2], outer = [1, 2]))
        @test !allna(repeat([1, 2], outer = [1, 2, 2]))

        # allna(da::DataArray)
        @test !allna(DataArray([1, 2], falses(2)))
        @test !allna(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
        da = DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2))
        da[1] = NA
        @test !allna(da)
        da[:] = NA
        @test allna(da)

        # allna(da::PooledDataArray)
        @test !allna(PooledDataArray([1, 2], falses(2)))
        @test !allna(PooledDataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
        pda = PooledDataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2))
        pda[1] = NA
        @test !allna(pda)
        pda[:] = NA
        @test allna(pda)
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
