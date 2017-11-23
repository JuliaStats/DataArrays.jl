@testset "DataArray" begin
    v = [1, 2, 3, 4]
    dv = DataArray(v, falses(size(v)))

    m = [1 2; 3 4]
    dm = DataArray(m, falses(size(m)))

    t = Array{Int}(2, 2, 2)
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

    @test isequal(DataArray([missing, missing], [true, true]), DataArray([missing, missing], [false, false]))
    @test isequal(DataArray(Any[1, missing], [false, true]), DataArray(Any[1, missing], [false, false]))
    @test isequal(DataArray(Any[1, 2], [false, true]), DataArray(Any[1, missing], [false, false]))

    x = DataArray([9, 9, 8])
    y = DataArray([1, 9, 3, 2, 2])
    @test append!(x, y) == [9, 9, 8, 1, 9, 3, 2, 2]

    x = DataArray([9, 9, 8])
    y = [1, 9, 3, 2, 2]
    @test append!(x, y) == [9, 9, 8, 1, 9, 3, 2, 2]

    x = @data [1, 2, missing]
    y = @data [3, missing, 5]
    @test isequal(copy(x), x)
    @test isequal(copy!(y, x), x)

    x = @data [1, missing, -2, 1, missing, 4]
    @test isequal(unique(x), @data [1, missing, -2, 4])
    @test isequal(unique(reverse(x)), @data [4, missing, 1, -2])
    @test isequal(unique(skipmissing(x)), @data [1, -2, 4])
    @test isequal(unique(reverse(collect(skipmissing(x)))), @data [4, 1, -2])
    @test isequal(levels(x), @data [1, -2, 4])
    @test isequal(levels(reverse(x)), @data [4, 1, -2])

    # check case where only missing occurs in final position
    @test isequal(unique(@data [1, 2, 1, missing]), @data [1, 2, missing])

    # Test copy!
    function nonbits(dv)
        ret = similar(dv, Integer)
        for i = 1:length(dv)
            # if !ismissing(dv, i)
                ret[i] = dv[i]
            # end
        end
        ret
    end
    set1 = Any[@data([1, missing, 3]),
               @data([missing, 5]), @data([1, 2, 3, 4, 5]), data(Int[]),
               @data([missing, 5, 3]), @data([1, 5, 3])]
    set2 = map(nonbits, set1)
    set3 = map(pdata, set1)

    for (dest, src, bigsrc, emptysrc, res1, res2) in Any[set1, set2, set3]
        @test isequal(copy!(copy(dest), src), res1)
        @test isequal(copy!(copy(dest), 1, src), res1)

        @test isequal(copy!(copy(dest), 2, src, 2), res2)
        @test isequal(copy!(copy(dest), 2, src, 2, 1), res2)

        @test isequal(copy!(copy(dest), 99, src, 99, 0), dest)

        @test isequal(copy!(copy(dest), 1, emptysrc), dest)
        @test_throws BoundsError copy!(dest, 1, emptysrc, 1)

        for idx in [0, 4]
            @test_throws BoundsError copy!(dest, idx, src)
            @test_throws BoundsError copy!(dest, idx, src, 1)
            @test_throws BoundsError copy!(dest, idx, src, 1, 1)
            @test_throws BoundsError copy!(dest, 1, src, idx)
            @test_throws BoundsError copy!(dest, 1, src, idx, 1)
        end

        @test_throws ArgumentError copy!(dest, 1, src, 1, -1)

        @test_throws BoundsError copy!(dest, bigsrc)

        @test_throws BoundsError copy!(dest, 3, src)
        @test_throws BoundsError copy!(dest, 3, src, 1)
        @test_throws BoundsError copy!(dest, 3, src, 1, 2)
        @test_throws BoundsError copy!(dest, 1, src, 2, 2)
    end

    # Inferrability of map (#276)
    @test eltype(map(x -> x > 1, @data [1, 2])) == Union{Bool,Missing}

    @testset "Issue #278" begin
        x = @data ones(4)
        @test parent(view(x, :)).data === x.data
    end
end
