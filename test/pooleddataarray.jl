module TestPDA
    using Base.Test
    using DataArrays
    using Compat

    p = @pdata [9, 9, 8, NA, 1, 1]
    pcopy = copy(p)
    @assert levels(p) == [1, 8, 9]
    @assert levels(setlevels(p, ["a", "b", "c"])) == ["a", "b", "c"]
    @assert dropna(setlevels(p, (@data ["a", "b", NA]))) == ["b", "a", "a"]
    @assert dropna(setlevels(p, (@data ["a", "b", "a"]))) == ["a", "a", "b", "a", "a"]
    @assert levels(setlevels(p, (@data ["a", "b", "a"]))) == ["a", "b"]
    @assert levels(setlevels(p, Dict([(1, 111)]))) == [111, 8, 9]
    @assert levels(setlevels(p, Dict([(1, 111), (8, NA)]))) == [111, 9]
    @assert levels(PooledDataArray(p, [9, 8, 1])) == [9, 8, 1]
    @assert levels(PooledDataArray(p, [9, 8])) == [9, 8]
    @assert dropna(PooledDataArray(p, [9, 8])) == [9, 9, 8]
    @assert levels(PooledDataArray(p, levels(p)[[3,2,1]])) == [9,8,1]
    v = collect(1:6)
    @assert isequal(p, reorder(p))
    # @assert levels(reorder(p, v)) == [9,8,1]
    @assert isequal(p, pcopy)

    @assert levels(setlevels!(copy(p), [10,80,90])) == [10, 80, 90]
    @assert levels(setlevels!(copy(p), [1,8,1])) == [1, 8]
    @assert levels(setlevels!(copy(p), (@data [1, 8, NA]))) == [1, 8]
    @assert levels(setlevels!(copy(p), [1,8,9, 10])) == [1, 8, 9, 10]
    @assert levels(setlevels!(copy(p), Dict([(1, 111)]))) == [111, 8, 9]
    @assert levels(setlevels!(copy(p), Dict([(1, 111), (8, NA)]))) == [111, 9]
    # issue #201
    @assert levels(setlevels!(@pdata([1.0, 2.0]), [3,4])) == [3.0, 4.0]

    y = @pdata [1, NA, -2, 1, NA, 4, NA]
    @assert isequal(unique(y), @pdata [1, NA, -2, 4])
    @assert isequal(unique(reverse(y)), @data [NA, 4, 1, -2])
    @assert isequal(unique(dropna(y)), @data [1, -2, 4])
    @assert isequal(unique(reverse(dropna(y))), @data [4, 1, -2])

    z = @pdata ["frank", NA, "gertrude", "frank", NA, "herbert", NA]
    @assert isequal(unique(z), @pdata ["frank", NA, "gertrude", "herbert"])
    @assert isequal(unique(reverse(z)), @pdata [NA, "herbert", "frank", "gertrude"])
    @assert isequal(unique(dropna(z)), @pdata ["frank", "gertrude", "herbert"])
    @assert isequal(unique(reverse(dropna(z))), @pdata ["herbert", "frank", "gertrude"])

    # check case where only NA occurs in final position
    @assert isequal(unique(@pdata [1, 2, 1, NA]), @pdata [1, 2, NA])

    pp = PooledDataArray(Any[])
    @assert length(pp) == 0
    @assert length(levels(pp)) == 0

    # test construction with unordered types
    pim = @pdata [1 + im, 2 + im, 3 + im, 2 + im, 1 + im]
    @assert levels(pim) == [1 + im, 2 + im, 3 + im]

    # Test explicitly setting refs type
    testarray = [1, 1, 2, 2, 0, 0, 3, 3]
    testdata = @data [1, 1, 2, 2, 0, 0, 3, 3]
    for t in Any[testarray, testdata]
        for R in [UInt8, UInt16, UInt32, UInt64]
            @assert eltype(PooledDataArray(t, R).refs) == R
            @assert eltype(PooledDataArray(t, [1,2,3], R).refs) == R
            @assert eltype(PooledDataArray(t, [1,2,3], t .== 0, R).refs) == R
        end
    end

    pcopy = copy(p)
    @assert levels(append!(pcopy, @pdata [4, NA, 6, 5])) == [1, 8, 9, 4, 5, 6]

    x = PooledDataArray([9, 9, 8])
    y = PooledDataArray([1, 9, 3, 2, 2])
    @assert append!(x, y) == [9, 9, 8, 1, 9, 3, 2, 2]

    x = PooledDataArray([9, 9, 8])
    y = [1, 9, 3, 2, 2]
    @assert append!(x, y) == [9, 9, 8, 1, 9, 3, 2, 2]

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
    @assert isequal(da, @data(ones(5, 5)))
    @assert typeof(da) == DataArray{Float64,2}
    da = convert(DataArray{Float32}, @pdata(ones(5, 5)))
    @assert isequal(da, @data(ones(Float32, 5, 5)))
    @assert typeof(da) == DataArray{Float32,2}
    da = convert(DataArray{Float32,2}, @pdata(ones(5, 5)))
    @assert isequal(da, @data(ones(Float32, 5, 5)))
    @assert typeof(da) == DataArray{Float32,2}

    # permute
    pda = @pdata([NA, "A", "B", "C", "A", "B"])
    @test isequal(Base.permute!!(copy(pda), [2, 5, 3, 6, 4, 1]), @pdata(["A", "A", "B", "B", "C", NA]))
    @test isequal(Base.ipermute!!(copy(pda), [6, 1, 3, 5, 2, 4]), @pdata(["A", "A", "B", "B", "C", NA]))

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
end
