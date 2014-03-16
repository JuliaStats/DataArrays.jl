module TestPDA
    using Base.Test
    using DataArrays

    p = @pdata [9, 9, 8, NA, 1, 1]
    pcopy = copy(p)
    @assert levels(p) == [1, 8, 9]
    @assert levels(setlevels(p, ["a", "b", "c"])) == ["a", "b", "c"]
    @assert dropna(setlevels(p, (@data ["a", "b", NA]))) == ["b", "a", "a"]
    @assert dropna(setlevels(p, (@data ["a", "b", "a"]))) == ["a", "a", "b", "a", "a"]
    @assert levels(setlevels(p, (@data ["a", "b", "a"]))) == ["a", "b"]
    @assert levels(setlevels(p, [1 => 111])) == [111, 8, 9]
    @assert levels(setlevels(p, [1 => 111, 8 => NA])) == [111, 9]
    @assert levels(PooledDataArray(p, [9, 8, 1])) == [9, 8, 1]
    @assert levels(PooledDataArray(p, [9, 8])) == [9, 8]
    @assert dropna(PooledDataArray(p, [9, 8])) == [9, 9, 8]
    @assert levels(PooledDataArray(p, levels(p)[[3,2,1]])) == [9,8,1]
    v = [1:6]
    @assert isequal(p, reorder(p))
    # @assert levels(reorder(p, v)) == [9,8,1]
    @assert isequal(p, pcopy)

    @assert levels(setlevels!(copy(p), [10,80,90])) == [10, 80, 90]
    @assert levels(setlevels!(copy(p), [1,8,1])) == [1, 8]
    @assert levels(setlevels!(copy(p), (@data [1, 8, NA]))) == [1, 8]
    @assert levels(setlevels!(copy(p), [1,8,9, 10])) == [1, 8, 9, 10]
    @assert levels(setlevels!(copy(p), [1 => 111])) == [111, 8, 9]
    @assert levels(setlevels!(copy(p), [1 => 111, 8 => NA])) == [111, 9]

    pp = PooledDataArray(Any[])
    @assert length(pp) == 0
    @assert length(levels(pp)) == 0

    # Test explicitly setting refs type
    testarray = [1, 1, 2, 2, 0, 0, 3, 3]
    testdata = @data [1, 1, 2, 2, 0, 0, 3, 3]
    for t in Any[testarray, testdata]
        for R in [Uint8, Uint16, Uint32, Uint64]
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
        for (to, totype) in ((PooledDataArray{Float32,Uint16,2}, PooledDataArray{Float32,Uint16,2}),
                             (PooledDataArray{Float32,Uint32,2}, PooledDataArray{Float32,Uint32,2}),
                             (PooledDataArray{Float64,Uint16,2}, PooledDataArray{Float64,Uint16,2}),
                             (PooledDataArray{Float64,Uint32,2}, PooledDataArray{Float64,Uint32,2}),
                             (PooledDataArray{Float32,Uint16}, PooledDataArray{Float32,Uint16,2}),
                             (PooledDataArray{Float32,Uint32}, PooledDataArray{Float32,Uint32,2}),
                             (PooledDataArray{Float64,Uint16}, PooledDataArray{Float64,Uint16,2}),
                             (PooledDataArray{Float64,Uint32}, PooledDataArray{Float64,Uint32,2}),
                             (PooledDataArray{Float32}, PooledDataArray{Float32,Uint32,2}),
                             (PooledDataArray{Float64}, PooledDataArray{Float64,Uint32,2}),
                             (PooledDataArray, PooledDataArray{Float64,Uint32,2}))
            rettype = typeof(convert(to, from))
            if rettype != totype
                error("convert($to, ::$(typeof(from))) returned $rettype (expected $totype)")
            end
        end
    end
end
