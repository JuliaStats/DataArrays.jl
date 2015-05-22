module TestData
    using Base.Test
    using DataArrays

    #test_context("Data types and NA's")

    #test_group("NA's")
    @assert length(NA) == 1
    @assert size(NA) == ()
    @assert isna(3 == NA)
    @assert isna(NA == 3)
    @assert isna(NA == NA)

    #test_group("DataVector creation")
    dvint = @data [1, 2, NA, 4]
    dvint2 = DataArray(collect(5:8))
    dvint3 = convert(DataArray, 5:8)
    dvflt = @data [1.0, 2, NA, 4]
    dvstr = @data ["one", "two", NA, "four"]
    dvdict = DataArray(Dict, 4) # for issue #199
    dvany = convert(DataArray{Any, 1}, dvint)

    @assert isa(dvint, DataVector{Int})
    @assert isa(dvint2, DataVector{Int})
    @assert isa(dvint3, DataVector{Int})
    @assert isa(dvflt, DataVector{Float64})
    @assert isa(dvstr, DataVector{ASCIIString})
    # @test throws_exception(DataArray([5:8], falses(2)), Exception)

    #test_group("PooledDataVector creation")
    pdvstr = @pdata ["one", "one", "two", "two", NA, "one", "one"]
    @assert isa(pdvstr, PooledDataVector{ASCIIString})
    # @test throws_exception(PooledDataVector["one", "one", 9], Exception)
    @assert isequal(PooledDataArray(pdvstr), pdvstr)

    #test_group("PooledDataVector creation with predetermined pool")
    pdvpp = PooledDataArray([1, 2, 2, 3], [1, 2, 3, 4])
    @assert isequal(pdvpp.pool, [1, 2, 3, 4])
    @assert string(pdvpp) == "[1, 2, 2, 3]"
    pdvpp = PooledDataArray([1, 2, 2, 3, 2, 1], [1, 2, 3, 4])
    @assert isequal(pdvpp.pool, [1, 2, 3, 4])
    @assert string(pdvpp) == "[1, 2, 2, 3, 2, 1]"
    pdvpp = PooledDataArray(["one", "two", "two"], ["one", "two", "three"])
    @assert isequal(convert(DataArray, pdvpp), @data(["one", "two", "two"]))
    @assert isequal(levels(pdvpp), @data(["one", "two", "three"]))
    @assert isequal(pdvpp.pool, ["one", "two", "three"])
    @assert string(pdvpp) == "[one, two, two]"
    @assert string(PooledDataArray(["one", "two", "four"],
                                   ["one", "two", "three"])) ==
            "[one, two, NA]"

    #test_group("PooledDataVector utf8 support")
    pdvpp = PooledDataArray([utf8("hello")], [false])
    @assert isa(pdvpp[1], UTF8String)
    pdvpp = PooledDataArray([utf8("hello")])
    @assert isa(pdvpp[1], UTF8String)

    #test_group("DataVector access")
    @assert dvint[1] == 1
    @assert isna(dvint[3])
    @assert isequal(dvflt[3:4], @data([NA, 4.0]))
    @assert isequal(dvint[[true, false, true, false]], @data([1, NA]))
    @assert isequal(dvstr[[1, 2, 1, 4]], @data(["one", "two", "one", "four"]))
    # Indexing produces #undef?
    # @assert isequal(dvstr[[1, 2, 1, 3]], DataVector["one", "two", "one", NA])

    #test_group("PooledDataVector access")
    @assert pdvstr[1] == "one"
    @assert isna(pdvstr[5])
    @assert isequal(pdvstr[1:3], @data(["one", "one", "two"]))
    @assert isequal(pdvstr[[true, false, true, false, true, false, true]],
                    @pdata(["one", "two", NA, "one"]))
    @assert isequal(pdvstr[[1, 3, 1, 2]], @data(["one", "two", "one", "one"]))

    #test_group("DataVector methods")
    @assert size(dvint) == (4,)
    @assert length(dvint) == 4
    @assert sum(isna(dvint)) == 1
    @assert eltype(dvint) == Int

    #test_group("PooledDataVector methods")
    @assert size(pdvstr) == (7,)
    @assert length(pdvstr) == 7
    @assert sum(isna(pdvstr)) == 1
    @assert eltype(pdvstr) == ASCIIString

    #test_group("DataVector operations")
    @assert isequal(dvint .+ 1, DataArray([2, 3, 4, 5], [false, false, true, false]))
    @assert isequal(dvint .* 2, @data([2, 4, NA, 8]))
    @assert isequal(dvint .== 2, @data([false, true, NA, false]))
    @assert isequal(dvint .> 1, @data([false, true, NA, true]))

    #test_group("PooledDataVector operations")
    # @assert isequal(pdvstr .== "two", PooledDataVector[false, false, true, true, NA, false, false])

    #test_group("DataVector to something else")
    @assert all(dropna(dvint) .== [1, 2, 4])
    @assert all(convert(Vector, dvint, 0) .== [1, 2, 0, 4])
    @assert all(convert(Vector, dvany, 0) .== [1, 2, 0, 4])
    utf8three = convert(UTF8String, "three")
    asciithree = convert(ASCIIString, "three")
    @assert all(convert(Vector, dvstr, utf8three) .== ["one", "two", "three", "four"])
    @assert all(convert(Vector, dvstr, asciithree) .== ["one", "two", "three", "four"])
    @assert all(convert(Vector{Int}, dvint2) .== [5:8])
    @assert all([i + 1 for i in dvint2] .== [6:9])
    @assert all([length(x)::Int for x in dvstr] == [3, 3, 1, 4])
    @assert repr(dvint) == "[1,2,NA,4]"

    #test_group("PooledDataVector to something else")
    @assert all(dropna(pdvstr) .== ["one", "one", "two", "two", "one", "one"])
    @assert all(convert(Vector, pdvstr, "nine") .== ["one", "one", "two", "two", "nine", "one", "one"])
    @assert all([length(i)::Int for i in pdvstr] .== [3, 3, 3, 3, 1, 3, 3])
    @assert string(pdvstr[1:3]) == "[one, one, two]"

    #test_group("DataVector Filter and Replace")
    @assert isequal(dropna(dvint), [1, 2, 4])
    @assert isequal(convert(Vector, dvint, 7), [1, 2, 7, 4])
    @assert sum(dropna(dvint)) == 7
    @assert sum(convert(Vector, dvint, 7)) == 14

    #test_group("PooledDataVector Filter and Replace")
    @assert reduce(string, "", dropna(pdvstr)) == "oneonetwotwooneone"
    @assert reduce(string, "", convert(Vector, pdvstr, "!")) == "oneonetwotwo!oneone"

    #test_group("DataVector assignment")
    assigntest = @data [1, 2, NA, 4]
    assigntest[1] = 8
    @assert isequal(assigntest, (@data [8, 2, NA, 4]))
    assigntest[1:2] = 9
    @assert isequal(assigntest, (@data [9, 9, NA, 4]))
    assigntest[[1,3]] = 10
    @assert isequal(assigntest, (@data [10, 9, 10, 4]))
    assigntest[[true, false, true, true]] = 11
    @assert isequal(assigntest, (@data [11, 9, 11, 11]))
    assigntest[1:2] = [12, 13]
    @assert isequal(assigntest, (@data [12, 13, 11, 11]))
    assigntest[[1, 4]] = [14, 15]
    @assert isequal(assigntest, (@data [14, 13, 11, 15]))
    assigntest[[true, false, true, false]] = [16, 17]
    @assert isequal(assigntest, (@data [16, 13, 17, 15]))
    assigntest[1] = NA
    @assert isequal(assigntest, (@data [NA, 13, 17, 15]))
    assigntest[[1, 2]] = NA
    @assert isequal(assigntest, (@data [NA, NA, 17, 15]))
    assigntest[[true, false, true, false]] = NA
    @assert isequal(assigntest, (@data [NA, NA, NA, 15]))
    assigntest[1] = 1
    assigntest[2:4] = NA
    @assert isequal(assigntest, (@data [1, NA, NA, NA]))

    #test_group("PooledDataVector assignment")
    ret = (pdvstr[2] = "three")
    @assert ret == "three"
    @assert pdvstr[2] == "three"
    ret = pdvstr[[1,2]] = "two"
    @assert ret == "two"
    @assert pdvstr[2] == "two"
    pdvstr2 = @pdata ["one", "one", "two", "two"]
    ret = (pdvstr2[[true, false, true, false]] = "three")
    @assert ret == "three"
    @assert pdvstr2[1] == "three"
    ret = (pdvstr2[[false, true, false, true]] = ["four", "five"])
    @assert isequal(ret, ["four", "five"])
    @assert isequal(pdvstr2[3:4], (@data ["three", "five"]))
    pdvstr2 = @pdata ["one", "one", "two", "two"]
    ret = (pdvstr2[2:3] = "three")
    @assert ret == "three"
    @assert isequal(pdvstr2[3:4], (@data ["three", "two"]))
    ret = (pdvstr2[2:3] = ["four", "five"])
    @assert ret == ["four", "five"]
    @assert isequal(pdvstr2[1:2], (@data ["one", "four"]))
    pdvstr2 = @pdata ["one", "one", "two", "two", "three"]
    @assert isna(begin pdvstr2[1] = NA end)
    @assert all(isna(begin pdvstr2[[1, 2]] = NA end))
    @assert all(isna(begin pdvstr2[[false, false, true, false, false]] = NA end))
    @assert all(isna(begin pdvstr2[4:5] = NA end))
    @assert all(isna(pdvstr2))

    #test_group("PooledDataVector replace!")
    pdvstr2 = @pdata ["one", "one", "two", "two", "three"]
    @assert replace!(pdvstr2, "two", "four") == "four"
    @assert replace!(pdvstr2, "three", "four") == "four"
    @assert isna(replace!(pdvstr2, "one", NA))
    @assert replace!(pdvstr2, NA, "five") == "five"
    @assert isequal(pdvstr2, (@data ["five", "five", "four", "four", "four"]))
end
