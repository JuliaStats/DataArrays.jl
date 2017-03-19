@testset "Extras" begin
    ##########
    ## countmap
    ##########

    d = @data [NA,3,3]
    w = weights([1.1,2.2,3.3])
    cm = Dict{Union{Int, NAtype}, Int}([(NA, 1), (3, 2)])
    cmw = Dict{Union{Int, NAtype}, Real}([(NA, 1.1), (3, 5.5)])
    @test isequal(countmap(d), cm)
    @test isequal(countmap(d, w), cmw)

    ##########
    ## cut
    ##########

    @test isequal(cut([2, 3, 5], [1, 3, 6]), PooledDataArray(["(1,3]", "(1,3]", "(3,6]"]))
    @test isequal(cut([2, 3, 5], [3, 6]), PooledDataArray(["[2,3]", "[2,3]", "(3,6]"]))
    @test isequal(cut([2, 3, 5, 6], [3, 6]), PooledDataArray(["[2,3]", "[2,3]", "(3,6]", "(3,6]"]))
    @test isequal(cut([1, 2, 4], [1, 3, 6]), PooledDataArray(["[1,3]", "[1,3]", "(3,6]"]))
    @test isequal(cut([1, 2, 4], [3, 6]), PooledDataArray(["[1,3]", "[1,3]", "(3,6]"]))
    @test isequal(cut([1, 2, 4], [3]), PooledDataArray(["[1,3]", "[1,3]", "(3,4]"]))
    @test isequal(cut([1, 5, 7], [3, 6]), PooledDataArray(["[1,3]", "(3,6]", "(6,7]"]))

    ages = [20, 22, 25, 27, 21, 23, 37, 31, 61, 45, 41, 32]
    bins = [18, 25, 35, 60, 100]
    cats = cut(ages, bins)
    pdv = PooledDataArray(["(18,25]", "(18,25]", "(18,25]",
                           "(25,35]", "(18,25]", "(18,25]",
                           "(35,60]", "(25,35]", "(60,100]",
                           "(35,60]", "(35,60]", "(25,35]"])
    @test isequal(cats, pdv)

    ##########
    ## repeat
    ##########

    @test isequal(repeat(@data [3.0, 2.0, NA]; inner = 2, outer = 1),
                  @data [3.0, 3.0, 2.0, 2.0, NA, NA])
    @test isequal(repeat(@pdata ["a", "b", NA]; inner = 2, outer = 1),
                  @pdata ["a", "a", "b", "b", NA, NA])
    @test isequal(repeat(@data [1 2; 3 NA]; inner = [1, 2], outer = [2, 1]),
                  @data [1 1 2 2; 3 3 NA NA; 1 1 2 2; 3 3 NA NA])
    @test isequal(repeat(@pdata [:a :b NA]; inner = [2,1], outer = [1,3]),
                  @pdata [:a :b NA :a :b NA :a :b NA;
                          :a :b NA :a :b NA :a :b NA])
end
