module TestNAs
    using Base.Test
    using DataArrays


    # anyna(a::AbstractArray)
    anyna([1, 2])
    anyna(repeat([1, 2], outer = [1, 2]))
    @test !anyna(repeat([1, 2], outer = [1, 2, 2]))

    # anyna(da::DataArray)
    anyna(DataArray([1, 2], falses(2)))
    anyna(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
    da = DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2))
    @test !anyna(da)
    da[2] = NA
    @test anyna(da)

    # anyna(pda::PooledDataArray)
    anyna(PooledDataArray([1, 2], falses(2)))
    anyna(PooledDataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
    pda = PooledDataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2))
    @test !anyna(pda)
    pda[2] = NA
    @test anyna(pda)

    # allna(a::AbstractArray)
    allna([1, 2])
    allna(repeat([1, 2], outer = [1, 2]))
    @test !allna(repeat([1, 2], outer = [1, 2, 2]))

    # allna(da::DataArray)
    allna(DataArray([1, 2], falses(2)))
    allna(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
    da = DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2))
    da[1] = NA
    @test !allna(da)
    da[:] = NA
    @test allna(da)

    # allna(da::PooledDataArray)
    allna(PooledDataArray([1, 2], falses(2)))
    allna(PooledDataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
    pda = PooledDataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2))
    pda[1] = NA
    @test !allna(pda)
    pda[:] = NA
    @test allna(pda)

    dv = DataArray([1, 2, 3], BitArray([false, false, false]))

    dv = DataArray(collect(1:6), fill(false, 6))

    a = dropna(dv)
    for v in each_failna(dv); end
    @test collect(each_failna(dv)) == a
    @test collect(each_dropna(dv)) == a
    @test collect(each_replacena(dv, 4)) == a

    dv[[1, 2, end]] = NA

    a = dropna(dv)
    @test_throws NAException for v in each_failna(dv); end
    @test collect(each_dropna(dv)) == a
    @test collect(each_replacena(dv, 4)) == [4, 4, a..., 4]
end
