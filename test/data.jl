@testset "Data types and missings" begin
    # TODO: Convert these test_group things to nested testsets
    #test_group("missings")
    @test ismissing(3 == missing)
    @test ismissing(missing == 3)
    @test ismissing(missing == missing)

    #test_group("DataVector creation")
    dvint = @data [1, 2, missing, 4]
    dvint2 = DataArray(collect(5:8))
    dvint3 = convert(DataArray, 5:8)
    dvflt = @data [1.0, 2, missing, 4]
    dvstr = @data ["one", "two", missing, "four"]
    # FIXME: triggers a segfault on Julia 0.6.0
    # dvdict = DataArray(Dict, 4) # for issue DataFrames#199
    dvany = convert(DataArray{Any, 1}, dvint)

    @test isa(dvint, DataVector{Int})
    @test isa(dvint2, DataVector{Int})
    @test isa(dvint3, DataVector{Int})
    @test isa(dvflt, DataVector{Float64})
    @test isa(dvstr, DataVector{String})
    @test_throws ArgumentError DataArray([5:8], falses(2))

    #test_group("PooledDataVector creation")
    pdvstr = @pdata ["one", "one", "two", "two", missing, "one", "one"]
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
            "[one, two, missing]"

    #test_group("PooledDataVector utf8 support")
    pdvpp = PooledDataArray([String("hello")], [false])
    @test isa(pdvpp[1], String)
    pdvpp = PooledDataArray([String("hello")])
    @test isa(pdvpp[1], String)

    #test_group("DataVector access")
    @test dvint[1] == 1
    @test ismissing(dvint[3])
    @test isequal(dvflt[3:4], @data([missing, 4.0]))
    @test isequal(dvint[[true, false, true, false]], @data([1, missing]))
    @test isequal(dvstr[[1, 2, 1, 4]], @data(["one", "two", "one", "four"]))
    # Indexing produces #undef?
    # @test isequal(dvstr[[1, 2, 1, 3]], DataVector["one", "two", "one", missing])

    #test_group("PooledDataVector access")
    @test pdvstr[1] == "one"
    @test ismissing(pdvstr[5])
    @test isequal(pdvstr[1:3], @data(["one", "one", "two"]))
    @test isequal(pdvstr[[true, false, true, false, true, false, true]],
                    @pdata(["one", "two", missing, "one"]))
    @test isequal(pdvstr[[1, 3, 1, 2]], @data(["one", "two", "one", "one"]))

    #test_group("DataVector methods")
    @test size(dvint) == (4,)
    @test length(dvint) == 4
    @test sum(ismissing.(dvint)) == 1
    @test eltype(dvint) == Union{Int,Missing}

    #test_group("PooledDataVector methods")
    @test size(pdvstr) == (7,)
    @test length(pdvstr) == 7
    @test sum(ismissing.(pdvstr)) == 1
    @test eltype(pdvstr) == Union{String,Missing}

    #test_group("DataVector operations")
    @test isequal(dvint .+ 1, DataArray([2, 3, 4, 5], [false, false, true, false]))
    @test isequal(dvint .* 2, @data([2, 4, missing, 8]))
    @test isequal(dvint .== 2, @data([false, true, missing, false]))
    @test isequal(dvint .> 1, @data([false, true, missing, true]))

    #test_group("PooledDataVector operations")
    # @test isequal(pdvstr .== "two", PooledDataVector[false, false, true, true, missing, false, false])

    #test_group("DataVector to something else")
    @test collect(skipmissing(dvint)) == [1, 2, 4]
    @test all(convert(Vector, dvint, 0) .== [1, 2, 0, 4])
    @test all(convert(Vector, dvany, 0) .== [1, 2, 0, 4])
    utf8three = convert(String, "three")
    asciithree = convert(String, "three")
    @test all(convert(Vector, dvstr, utf8three) .== ["one", "two", "three", "four"])
    @test all(convert(Vector, dvstr, asciithree) .== ["one", "two", "three", "four"])
    @test all(convert(Vector{Int}, dvint2) .== [5:8;])
    @test all([i + 1 for i in dvint2] .== [6:9;])
    #@test all([length(x)::Int for x in dvstr] == [3, 3, 1, 4])
    # Julia 0.6 and 0.7 differ in ordering of Unions
    @test repr(dvint) in ("Union{$Int, Missings.Missing}[1, 2, missing, 4]",
                          "Union{Missings.Missing, $Int}[1, 2, missing, 4]")

    #test_group("PooledDataVector to something else")
    @test collect(skipmissing(pdvstr)) == ["one", "one", "two", "two", "one", "one"]
    @test all(convert(Vector, pdvstr, "nine") .== ["one", "one", "two", "two", "nine", "one", "one"])
    #@test all([length(i)::Int for i in pdvstr] .== [3, 3, 3, 3, 1, 3, 3])
    @test string(pdvstr[1:3]) == "[one, one, two]"

    #test_group("DataVector Filter and Replace")
    @test collect(skipmissing(dvint)) == [1, 2, 4]
    @test isequal(convert(Vector, dvint, 7), [1, 2, 7, 4])
    @test sum(skipmissing(dvint)) == 7
    @test sum(convert(Vector, dvint, 7)) == 14

    #test_group("PooledDataVector Filter and Replace")
    @test reduce(string, "", skipmissing(pdvstr)) == "oneonetwotwooneone"
    @test reduce(string, "", convert(Vector, pdvstr, "!")) == "oneonetwotwo!oneone"

    #test_group("DataVector assignment")
    assigntest = @data [1, 2, missing, 4]
    assigntest[1] = 8
    @test isequal(assigntest, (@data [8, 2, missing, 4]))
    assigntest[1:2] = 9
    @test isequal(assigntest, (@data [9, 9, missing, 4]))
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
    assigntest[1] = missing
    @test isequal(assigntest, (@data [missing, 13, 17, 15]))
    assigntest[[1, 2]] = missing
    @test isequal(assigntest, (@data [missing, missing, 17, 15]))
    assigntest[[true, false, true, false]] = missing
    @test isequal(assigntest, (@data [missing, missing, missing, 15]))
    assigntest[1] = 1
    assigntest[2:4] = missing
    @test isequal(assigntest, (@data [1, missing, missing, missing]))

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
    @test ismissing(begin pdvstr2[1] = missing end)
    @test all(ismissing(begin pdvstr2[[1, 2]] = missing end))
    @test all(ismissing(begin pdvstr2[[false, false, true, false, false]] = missing end))
    @test all(ismissing(begin pdvstr2[4:5] = missing end))
    @test all(ismissing.(pdvstr2))

    #test_group("PooledDataVector replace!")
    pdvstr2 = @pdata ["one", "one", "two", "two", "three"]
    @test replace!(pdvstr2, "two", "four") == "four"
    @test replace!(pdvstr2, "three", "four") == "four"
    @test ismissing.(replace!(pdvstr2, "one", missing))
    @test replace!(pdvstr2, missing, "five") == "five"
    @test isequal(pdvstr2, (@data ["five", "five", "four", "four", "four"]))
end
