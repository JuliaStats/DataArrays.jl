@testset "nulls" begin
    @testset "any(isnull, x)" begin
        # any(isnull, a::AbstractArray)
        @test any(isnull, Any[null, 1])
        @test !any(isnull, [1, 2])
        @test !any(isnull, repeat([1, 2], outer = [1, 2]))
        @test !any(isnull, repeat([1, 2], outer = [1, 2, 2]))

        # any(isnull, da::DataArray)
        @test !any(isnull, DataArray([1, 2], falses(2)))
        @test !any(isnull, DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
        da = DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2))
        @test !any(isnull, da)
        da[2] = null
        @test any(isnull, da)

        # any(isnull, pda::PooledDataArray)
        @test !any(isnull, PooledDataArray([1, 2], falses(2)))
        @test !any(isnull, PooledDataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
        pda = PooledDataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2))
        @test !any(isnull, pda)
        pda[2] = null
        @test any(isnull, pda)
    end

    @testset "all(isnull, x)" begin
        # all(isnull, a::AbstractArray)
        @test all(isnull, Any[null, null])
        @test !all(isnull, Any[null, 1])
        @test !all(isnull, [1, 2])
        @test !all(isnull, repeat([1, 2], outer = [1, 2]))
        @test !all(isnull, repeat([1, 2], outer = [1, 2, 2]))

        # all(isnull, da::DataArray)
        @test !all(isnull, DataArray([1, 2], falses(2)))
        @test !all(isnull, DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
        da = DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2))
        da[1] = null
        @test !all(isnull, da)
        da[:] = null
        @test all(isnull, da)

        # all(isnull, da::PooledDataArray)
        @test !all(isnull, PooledDataArray([1, 2], falses(2)))
        @test !all(isnull, PooledDataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
        pda = PooledDataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2))
        pda[1] = null
        @test !all(isnull, pda)
        pda[:] = null
        @test all(isnull, pda)
    end

    dv = DataArray(collect(1:6), fill(false, 6))
    a = dropnull(dv)
    @test collect(Nulls.fail(dv)) == a
    @test collect(Nulls.skip(dv)) == a
    @test collect(Nulls.replace(dv, 4)) == a

    dv[[1, 2, end]] = null

    @testset "promotion" for (T1, T2) in ((Int, Float64),
                                          (Dates.Minute, Dates.Second))
        @eval begin
            @test promote_type($T1, Union{$T2, Null})              == Union{$T2, Null}
            @test promote_type(Union{$T1, Null}, $T2)              == Union{$T2, Null}
            @test promote_type(Union{$T1, Null}, Union{$T2, Null}) == Union{$T2, Null}
        end
    end

    a = dropnull(dv)
    @test_throws NullException for v in Nulls.fail(dv); end
    @test collect(Nulls.skip(dv)) == a
    @test collect(Nulls.replace(dv, 4)) == [4, 4, a..., 4]

end
