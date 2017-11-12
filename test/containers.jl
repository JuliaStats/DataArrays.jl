@testset "Containers" begin
    dv = @data ones(3)
    push!(dv, 3.0)
    push!(dv, missing)

    @test isequal(dv, (@data [1.0, 1.0, 1.0, 3.0, missing]))

    a, b = pop!(dv), pop!(dv)
    @test ismissing(a)
    @test b == 3.0

    unshift!(dv, 3.0)
    unshift!(dv, missing)

    @test isequal(dv, (@data [missing, 3.0, 1.0, 1.0, 1.0]))

    a, b = shift!(dv), shift!(dv)
    @test ismissing(a)
    @test b == 3.0

    ## SPLICE
    function test_splice(dv, spliceout, splicein...)
        dv = copy(dv)
        v = Any[x for x in dv]
        expval = dv[spliceout]
        retval = splice!(dv, spliceout, splicein...)
        isequal(retval, expval) ||
            error("splice!$(tuple(dv, spliceout, splicein...)) gave incorrect return value $retval (expected $expval)")
        splice!(v, spliceout, splicein...)
        for i = 1:length(v)
            isequal(dv[i], v[i]) ||
                error("splice!$(tuple(dv, spliceout, splicein...)) gave incorrect output")
        end
    end

    function test_deleteat(dv, spliceout)
        dv = copy(dv)
        v = Any[x for x in dv]
        @test deleteat!(dv, spliceout) === dv
        deleteat!(v, spliceout)
        for i = 1:length(v)
            isequal(dv[i], v[i]) ||
                error("deleteat!($(tuple(dv, spliceout)) gave incorrect output")
        end
    end

    dv1 = @data [1.0, 2.0, missing, 2.0, missing, 3.0]
    for dv in (dv1, convert(DataVector{Number}, dv1), convert(PooledDataArray, dv1))
        for spliceout in (2, 3, 2:3, 5:6)
            test_splice(dv, spliceout)
            test_deleteat(dv, spliceout)
            for splicein in ([], [3], @data([3]), @pdata([3]),
                             [3, 4, 5], [3., 4., 5.], @data([3, missing, 4]),
                             @pdata([3, missing, 4]), @data([missing, 3.0, 4.0]),
                             @pdata([missing, 3.0, 4.0]))
                test_splice(dv, spliceout, splicein)
            end
        end
    end

    ## sizehint
    sizehint!(@data([1.0, 2.0]), 5)
    sizehint!(@pdata([1.0, 2.0]), 5)
end
