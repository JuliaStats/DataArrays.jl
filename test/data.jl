@testset "Data types and nulls" begin
    # TODO: Convert these test_group things to nested testsets
    #test_group("nulls")
    @test length(null) == 1
    @test size(null) == ()
    @test 3 != null
    @test null != 3
    @test null == null

    #test_group("DataVector creation")
    dvint = @data [1, 2, null, 4]
    dvint2 = DataArray(collect(5:8))
    dvint3 = convert(DataArray, 5:8)
    dvflt = @data [1.0, 2, null, 4]
    dvstr = @data ["one", "two", null, "four"]
    dvdict = DataArray(Dict, 4) # for issue #199
    dvany = convert(DataArray{Any, 1}, dvint)

    @test isa(dvint, DataVector{Int})
    @test isa(dvint2, DataVector{Int})
    @test isa(dvint3, DataVector{Int})
    @test isa(dvflt, DataVector{Float64})
    @test isa(dvstr, DataVector{String})
    @test_throws ArgumentError DataArray([5:8], falses(2))

    #test_group("PooledDataVector creation")
    pdvstr = @pdata ["one", "one", "two", "two", null, "one", "one"]
    @test isa(pdvstr, PooledDataVector{String})
    @test isequal(PooledDataArray(pdvstr), pdvstr)

    #test_group("PooledDataVector creation with predetermined pool")
    pdvpp = PooledDataArray([1, 2, 2, 3], [1, 2, 3, 4])
    @test isequal(pdvpp.pool, [1, 2, 3, 4])
    @test string(pdvpp) == "[1, 2, 2, 3]"
    pdvpp = PooledDataArray([1, 2, 2, 3, 2, 1], [1, 2, 3, 4])
    @test isequal(pdvpp.pool, [1, 2, 3, 4])
    @test string(pdvpp) == "[1, 2, 2, 3, 2, 1]"
    pdvpp = PooledDataArray(["one", "two", "two"], ["one", "two", "three"])
    @test isequal(convert(DataArray, pdvpp), @data(["one", "two", "two"]))
    @test isequal(levels(pdvpp), @data(["one", "two", "three"]))
    @test isequal(pdvpp.pool, ["one", "two", "three"])
    @test string(pdvpp) == "[one, two, two]"
    @test string(PooledDataArray(["one", "two", "four"],
                                   ["one", "two", "three"])) ==
            "[one, two, null]"

    #test_group("PooledDataVector utf8 support")
    pdvpp = PooledDataArray([String("hello")], [false])
    @test isa(pdvpp[1], String)
    pdvpp = PooledDataArray([String("hello")])
    @test isa(pdvpp[1], String)

    #test_group("DataVector access")
    @test dvint[1] == 1
    @test isnull(dvint[3])
    @test isequal(dvflt[3:4], @data([null, 4.0]))
    @test isequal(dvint[[true, false, true, false]], @data([1, null]))
    @test isequal(dvstr[[1, 2, 1, 4]], @data(["one", "two", "one", "four"]))
    # Indexing produces #undef?
    # @test isequal(dvstr[[1, 2, 1, 3]], DataVector["one", "two", "one", null])

    #test_group("PooledDataVector access")
    @test pdvstr[1] == "one"
    @test isnull(pdvstr[5])
    @test isequal(pdvstr[1:3], @data(["one", "one", "two"]))
    @test isequal(pdvstr[[true, false, true, false, true, false, true]],
                    @pdata(["one", "two", null, "one"]))
    @test isequal(pdvstr[[1, 3, 1, 2]], @data(["one", "two", "one", "one"]))

    #test_group("DataVector methods")
    @test size(dvint) == (4,)
    @test length(dvint) == 4
    @test sum(isnull.(dvint)) == 1
    @test eltype(dvint) == Union{Int,Null}

    #test_group("PooledDataVector methods")
    @test size(pdvstr) == (7,)
    @test length(pdvstr) == 7
    @test sum(isnull.(pdvstr)) == 1
    @test eltype(pdvstr) == Union{String,Null}

    #test_group("DataVector operations")
    @test isequal(dvint .+ 1, DataArray([2, 3, 4, 5], [false, false, true, false]))
    @test isequal(dvint .* 2, @data([2, 4, null, 8]))
    @test isequal(dvint .== 2, @data([false, true, false, false]))
    @test isequal(dvint .> 1, @data([false, true, false, true]))

    #test_group("PooledDataVector operations")
    # @test isequal(pdvstr .== "two", PooledDataVector[false, false, true, true, null, false, false])

    #test_group("DataVector to something else")
    @test all(dropna(dvint) .== [1, 2, 4])
    @test all(convert(Vector, dvint, 0) .== [1, 2, 0, 4])
    @test all(convert(Vector, dvany, 0) .== [1, 2, 0, 4])
    utf8three = convert(String, "three")
    asciithree = convert(String, "three")
    @test all(convert(Vector, dvstr, utf8three) .== ["one", "two", "three", "four"])
    @test all(convert(Vector, dvstr, asciithree) .== ["one", "two", "three", "four"])
    @test all(convert(Vector{Int}, dvint2) .== [5:8;])
    @test all([i + 1 for i in dvint2] .== [6:9;])
    @test all([length(x)::Int for x in dvstr] == [3, 3, 1, 4])
    @test repr(dvint) == "Union{$Int, Nulls.Null}[1, 2, null, 4]"

    #test_group("PooledDataVector to something else")
    @test all(dropna(pdvstr) .== ["one", "one", "two", "two", "one", "one"])
    @test all(convert(Vector, pdvstr, "nine") .== ["one", "one", "two", "two", "nine", "one", "one"])
    @test all([length(i)::Int for i in pdvstr] .== [3, 3, 3, 3, 1, 3, 3])
    @test string(pdvstr[1:3]) == "[one, one, two]"

    #test_group("DataVector Filter and Replace")
    @test isequal(dropna(dvint), [1, 2, 4])
    @test isequal(convert(Vector, dvint, 7), [1, 2, 7, 4])
    @test sum(dropna(dvint)) == 7
    @test sum(convert(Vector, dvint, 7)) == 14

    #test_group("PooledDataVector Filter and Replace")
    @test reduce(string, "", dropna(pdvstr)) == "oneonetwotwooneone"
    @test reduce(string, "", convert(Vector, pdvstr, "!")) == "oneonetwotwo!oneone"

    #test_group("DataVector assignment")
    assigntest = @data [1, 2, null, 4]
    assigntest[1] = 8
    @test isequal(assigntest, (@data [8, 2, null, 4]))
    assigntest[1:2] = 9
    @test isequal(assigntest, (@data [9, 9, null, 4]))
    assigntest[[1,3]] = 10
    @test isequal(assigntest, (@data [10, 9, 10, 4]))
    assigntest[[true, false, true, true]] = 11
    @test isequal(assigntest, (@data [11, 9, 11, 11]))
    assigntest[1:2] = [12, 13]
    @test isequal(assigntest, (@data [12, 13, 11, 11]))
    assigntest[[1, 4]] = [14, 15]
    @test isequal(assigntest, (@data [14, 13, 11, 15]))
    assigntest[[true, false, true, false]] = [16, 17]
    @test isequal(assigntest, (@data [16, 13, 17, 15]))
    assigntest[1] = null
    @test isequal(assigntest, (@data [null, 13, 17, 15]))
    assigntest[[1, 2]] = null
    @test isequal(assigntest, (@data [null, null, 17, 15]))
    assigntest[[true, false, true, false]] = null
    @test isequal(assigntest, (@data [null, null, null, 15]))
    assigntest[1] = 1
    assigntest[2:4] = null
    @test isequal(assigntest, (@data [1, null, null, null]))

    #test_group("PooledDataVector assignment")
    ret = (pdvstr[2] = "three")
    @test ret == "three"
    @test pdvstr[2] == "three"
    ret = pdvstr[[1,2]] = "two"
    @test ret == "two"
    @test pdvstr[2] == "two"
    pdvstr2 = @pdata ["one", "one", "two", "two"]
    ret = (pdvstr2[[true, false, true, false]] = "three")
    @test ret == "three"
    @test pdvstr2[1] == "three"
    ret = (pdvstr2[[false, true, false, true]] = ["four", "five"])
    @test isequal(ret, ["four", "five"])
    @test isequal(pdvstr2[3:4], (@data ["three", "five"]))
    pdvstr2 = @pdata ["one", "one", "two", "two"]
    ret = (pdvstr2[2:3] = "three")
    @test ret == "three"
    @test isequal(pdvstr2[3:4], (@data ["three", "two"]))
    ret = (pdvstr2[2:3] = ["four", "five"])
    @test ret == ["four", "five"]
    @test isequal(pdvstr2[1:2], (@data ["one", "four"]))
    pdvstr2 = @pdata ["one", "one", "two", "two", "three"]
    @test isnull(begin pdvstr2[1] = null end)
    @test all(isnull(begin pdvstr2[[1, 2]] = null end))
    @test all(isnull(begin pdvstr2[[false, false, true, false, false]] = null end))
    @test all(isnull(begin pdvstr2[4:5] = null end))
    @test all(isnull.(pdvstr2))

    #test_group("PooledDataVector replace!")
    pdvstr2 = @pdata ["one", "one", "two", "two", "three"]
    @test replace!(pdvstr2, "two", "four") == "four"
    @test replace!(pdvstr2, "three", "four") == "four"
    @test isnull.(replace!(pdvstr2, "one", null))
    @test replace!(pdvstr2, null, "five") == "five"
    @test isequal(pdvstr2, (@data ["five", "five", "four", "four", "four"]))
end
