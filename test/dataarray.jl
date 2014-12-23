module TestDataArray
    using Base.Test
    using DataArrays

    v = [1, 2, 3, 4]
    dv = DataArray(v, falses(size(v)))

    m = [1 2; 3 4]
    dm = DataArray(m, falses(size(m)))

    t = Array(Int, 2, 2, 2)
    t[1:2, 1:2, 1:2] = 1
    dt = DataArray(t, falses(size(t)))

    dv = DataArray(v)
    dv = DataArray(v, [false, false, false, false])

    dv = DataArray(Int, 2)
    dm = DataArray(Int, 2, 2)
    dt = DataArray(Int, 2, 2, 2)

    similar(dv)
    similar(dm)
    similar(dt)

    similar(dv, 2)
    similar(dm, 2, 2)
    similar(dt, 2, 2, 2)


    x = DataArray([9, 9, 8])
    y = DataArray([1, 9, 3, 2, 2])
    @assert append!(x, y) == [9, 9, 8, 1, 9, 3, 2, 2]

    x = DataArray([9, 9, 8])
    y = [1, 9, 3, 2, 2]
    @assert append!(x, y) == [9, 9, 8, 1, 9, 3, 2, 2]

    x = @data [1, 2, NA]
    y = @data [3, NA, 5]
    @test isequal(copy(x), x)
    @test isequal(copy!(y, x), x)

    x = @data [1, NA, -2, 1, NA, 4]
    @assert isequal(unique(x), @data [1, NA, -2, 4])
    @assert isequal(unique(reverse(x)), @data [4, NA, 1, -2])
    @assert isequal(unique(dropna(x)), @data [1, -2, 4])
    @assert isequal(unique(reverse(dropna(x))), @data [4, 1, -2])
    @assert isequal(levels(x), @data [1, -2, 4])
    @assert isequal(levels(reverse(x)), @data [4, 1, -2])

    # check case where only NA occurs in final position
    @assert isequal(unique(@data [1, 2, 1, NA]), @data [1, 2, NA])

    # Test copy!
    function nonbits(dv)
        ret = similar(dv, Integer)
        for i = 1:length(dv)
            if !isna(dv, i)
                ret[i] = dv[i]
            end
        end
        ret
    end
    set1 = Any[@data([1, NA, 3]),
               @data([NA, 5]), @data([1, 2, 3, 4, 5]), data(Int[]),
               @data([NA, 5, 3]), @data([1, 5, 3])]
    set2 = map(nonbits, set1)
    set3 = map(pdata, set1)

    for (dest, src, bigsrc, emptysrc, res1, res2) in Any[set1, set2, set3]
        # Base.copy! was inconsistent until recently in 0.4-dev
        da_or_04 = VERSION > v"0.4-" || !isa(dest, PooledDataArray)

        @test isequal(copy!(copy(dest), src), res1)
        @test isequal(copy!(copy(dest), 1, src), res1)

        da_or_04 && @test isequal(copy!(copy(dest), 2, src, 2), res2)
        @test isequal(copy!(copy(dest), 2, src, 2, 1), res2)

        @test isequal(copy!(copy(dest), 99, src, 99, 0), dest)

        @test isequal(copy!(copy(dest), 1, emptysrc), dest)
        da_or_04 && @test_throws BoundsError copy!(dest, 1, emptysrc, 1)

        for idx in [0, 4]
            @test_throws BoundsError copy!(dest, idx, src)
            @test_throws BoundsError copy!(dest, idx, src, 1)
            @test_throws BoundsError copy!(dest, idx, src, 1, 1)
            @test_throws BoundsError copy!(dest, 1, src, idx)
            @test_throws BoundsError copy!(dest, 1, src, idx, 1)
        end

       da_or_04 && @test_throws BoundsError copy!(dest, 1, src, 1, -1)

        @test_throws BoundsError copy!(dest, bigsrc)

        @test_throws BoundsError copy!(dest, 3, src)
        @test_throws BoundsError copy!(dest, 3, src, 1)
        @test_throws BoundsError copy!(dest, 3, src, 1, 2)
        @test_throws BoundsError copy!(dest, 1, src, 2, 2)
    end

end
