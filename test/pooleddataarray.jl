@testset "PDA" begin
    p = @pdata [9, 9, 8, missing, 1, 1]
    pcopy = copy(p)
    @test levels(p) == [1, 8, 9]
    @test levels(setlevels(p, ["a", "b", "c"])) == ["a", "b", "c"]
    @test collect(skipmissing(setlevels(p, (@data ["a", "b", missing])))) == ["b", "a", "a"]
    @test collect(skipmissing(setlevels(p, (@data ["a", "b", "a"])))) == ["a", "a", "b", "a", "a"]
    @test levels(setlevels(p, (@data ["a", "b", "a"]))) == ["a", "b"]
    @test levels(setlevels(p, Dict([(1, 111)]))) == [111, 8, 9]
    @test levels(setlevels(p, Dict([(1, 111), (8, missing)]))) == [111, 9]
    @test levels(PooledDataArray(p, [9, 8, 1])) == [9, 8, 1]
    @test levels(PooledDataArray(p, [9, 8])) == [9, 8]
    @test collect(skipmissing(PooledDataArray(p, [9, 8]))) == [9, 9, 8]
    @test levels(PooledDataArray(p, levels(p)[[3,2,1]])) == [9,8,1]
    v = collect(1:6)
    @test isequal(p, reorder(p))
    # @test levels(reorder(p, v)) == [9,8,1]
    @test isequal(p, pcopy)

    @test levels(setlevels!(copy(p), [10,80,90])) == [10, 80, 90]
    @test levels(setlevels!(copy(p), [1,8,1])) == [1, 8]
    @test levels(setlevels!(copy(p), (@data [1, 8, missing]))) == [1, 8]
    @test levels(setlevels!(copy(p), [1,8,9, 10])) == [1, 8, 9, 10]
    @test levels(setlevels!(copy(p), Dict([(1, 111)]))) == [111, 8, 9]
    @test levels(setlevels!(copy(p), Dict([(1, 111), (8, missing)]))) == [111, 9]
    # issue #201
    @test levels(setlevels!(@pdata([1.0, 2.0]), [3,4])) == [3.0, 4.0]

    y = @pdata [1, missing, -2, 1, missing, 4, missing]
    @test isequal(unique(y), @pdata [1, missing, -2, 4])
    @test isequal(unique(reverse(y)), @data [missing, 4, 1, -2])
    @test isequal(unique(skipmissing(y)), @data [1, -2, 4])
    @test isequal(unique(reverse(collect(skipmissing(y)))), @data [4, 1, -2])

    z = @pdata ["frank", missing, "gertrude", "frank", missing, "herbert", missing]
    @test isequal(unique(z), @pdata ["frank", missing, "gertrude", "herbert"])
    @test isequal(unique(reverse(z)), @pdata [missing, "herbert", "frank", "gertrude"])
    @test isequal(unique(skipmissing(z)), @pdata ["frank", "gertrude", "herbert"])
    @test isequal(unique(reverse(collect(skipmissing(z)))), @pdata ["herbert", "frank", "gertrude"])

    # check case where only missing occurs in final position
    @test isequal(unique(@pdata [1, 2, 1, missing]), @pdata [1, 2, missing])

    pp = PooledDataArray(Any[])
    @test length(pp) == 0
    @test length(levels(pp)) == 0

    # test construction with unordered types
    pim = @pdata [1 + im, 2 + im, 3 + im, 2 + im, 1 + im]
    @test levels(pim) == [1 + im, 2 + im, 3 + im]

    # Test explicitly setting refs type
    testarray = [1, 1, 2, 2, 0, 0, 3, 3]
    testdata = @data [1, 1, 2, 2, 0, 0, 3, 3]
    for t in Any[testarray, testdata]
        for R in [UInt8, UInt16, UInt32, UInt64]
            @test eltype(PooledDataArray(t, R).refs) == R
            @test eltype(PooledDataArray(t, [1,2,3], R).refs) == R
            @test eltype(PooledDataArray(t, [1,2,3], t .== 0, R).refs) == R
        end
    end

    pcopy = copy(p)
    @test levels(append!(pcopy, @pdata [4, missing, 6, 5])) == [1, 8, 9, 4, 5, 6]

    x = PooledDataArray([9, 9, 8])
    y = PooledDataArray([1, 9, 3, 2, 2])
    @test append!(x, y) == [9, 9, 8, 1, 9, 3, 2, 2]

    x = PooledDataArray([9, 9, 8])
    y = [1, 9, 3, 2, 2]
    @test append!(x, y) == [9, 9, 8, 1, 9, 3, 2, 2]

    # convert methods
    for from in (@pdata(ones(5, 5)), @data(ones(5, 5)), ones(5, 5))
        for (to, totype) in ((PooledDataArray{Float32,UInt16,2}, PooledDataArray{Float32,UInt16,2}),
                             (PooledDataArray{Float32,UInt32,2}, PooledDataArray{Float32,UInt32,2}),
                             (PooledDataArray{Float64,UInt16,2}, PooledDataArray{Float64,UInt16,2}),
                             (PooledDataArray{Float64,UInt32,2}, PooledDataArray{Float64,UInt32,2}),
                             (PooledDataArray{Float32,UInt16}, PooledDataArray{Float32,UInt16,2}),
                             (PooledDataArray{Float32,UInt32}, PooledDataArray{Float32,UInt32,2}),
                             (PooledDataArray{Float64,UInt16}, PooledDataArray{Float64,UInt16,2}),
                             (PooledDataArray{Float64,UInt32}, PooledDataArray{Float64,UInt32,2}),
                             (PooledDataArray{Float32}, PooledDataArray{Float32,UInt32,2}),
                             (PooledDataArray{Float64}, PooledDataArray{Float64,UInt32,2}),
                             (PooledDataArray, PooledDataArray{Float64,UInt32,2}))
            rettype = typeof(convert(to, from))
            if rettype != totype
                error("convert($to, ::$(typeof(from))) returned $rettype (expected $totype)")
            end
        end
    end

    da = convert(DataArray, @pdata(ones(5, 5)))
    @test isequal(da, @data(ones(5, 5)))
    @test typeof(da) == DataArray{Float64,2}
    da = convert(DataArray{Float32}, @pdata(ones(5, 5)))
    @test isequal(da, @data(ones(Float32, 5, 5)))
    @test typeof(da) == DataArray{Float32,2}
    da = convert(DataArray{Float32,2}, @pdata(ones(5, 5)))
    @test isequal(da, @data(ones(Float32, 5, 5)))
    @test typeof(da) == DataArray{Float32,2}

    # permute
    pda = @pdata([missing, "A", "B", "C", "A", "B"])
    @test isequal(Base.permute!!(copy(pda), [2, 5, 3, 6, 4, 1]), @pdata(["A", "A", "B", "B", "C", missing]))
    @test isequal(Base.ipermute!!(copy(pda), [6, 1, 3, 5, 2, 4]), @pdata(["A", "A", "B", "B", "C", missing]))

    a1 = 1:200
    a2 = 100:300
    pa1 = PooledDataArray(a1)
    pa2 = PooledDataArray(a2)
    ca1 = compact(pa1)
    ca2 = compact(pa2)
    r = vcat(ca1, ca2)
    @test r == vcat(a1, a2)
    @test isa(r, PooledDataArray{Int,DataArrays.DEFAULT_POOLED_REF_TYPE})
    @test isa(vcat(ca1, pa2), PooledDataArray{Int,DataArrays.DEFAULT_POOLED_REF_TYPE})

    a1 = Array{Int}(2,3,4,5)
    a2 = Array{Int}(3,3,4,5)
    a1[1:end] = length(a1):-1:1
    a2[1:end] = (1:length(a2)) + 10
    ca1 = compact(PooledDataArray(a1))
    ca2 = compact(PooledDataArray(a2))
    r = vcat(ca1, ca2)
    @test r == vcat(a1, a2)
    @test isa(r, PooledDataArray{Int,DataArrays.DEFAULT_POOLED_REF_TYPE,4})

    # Issue #265:
    #   Ensure that any levels that have are not expressed in a
    #   PooledDataArray are handled correctly and not left uninitialised when
    #   using unique()
    x = ["A", "B", "A"];
    masks = [[1], [2], [3], [1, 3]]
    for mask in masks
        y = PooledDataArray(x)
        y[mask] = missing
        @test isequal(sort(unique(y)), sort(DataArray(unique(y))))
    end
    z = PooledDataArray([1, 2], [1, 2, 3])
    @test sort(unique(z)) == DataArray([1, 2])


    # Issue #273
    #   Ensure that PooledDataArray works for arrays with non leaf type elements.
    #   Those two examples used to throw exceptions.
    x = AbstractString["a"]
    m = Bool[false]
    r = UInt32
    @test isa(PooledDataArray(x, m, r), DataArrays.PooledDataArray{AbstractString, UInt32, 1})
    x = Integer[1]
    @test isa(PooledDataArray(x, m, r), DataArrays.PooledDataArray{Integer, UInt32, 1})
end
