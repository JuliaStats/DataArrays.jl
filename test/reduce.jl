macro same_behavior(ex1, ex2)
    quote
        v = try
            $ex2
        catch e
            e
        end
        isa(v, Exception) ? @test_throws(typeof(v), $ex1) : @test isapprox($ex1, v)
    end
end

@testset "Reduce" begin
    srand(1337)

    ## extended test of sum

    for skipmissing in (true, false)
        @test sum(@data(Int8[]); skipmissing=skipmissing) === Int32(0)
        @test sum(@data(Int[]); skipmissing=skipmissing) === 0
        @test sum(@data(Float64[]); skipmissing=skipmissing) === 0.0

        @test sum(@data([Int8(3)]); skipmissing=skipmissing) === Int32(3)
        @test sum(@data([3]); skipmissing=skipmissing) === 3
        @test sum(@data([3.0]); skipmissing=skipmissing) === 3.0

        z = DataArray(reshape(1:16, (2,2,2,2)))
        fz = convert(DataArray{Float64}, z)
        bfz = convert(DataArray{BigFloat}, z)
        @test sum(z) === 136
        @test sum(fz) === 136.0
        @test sum(bfz) == 136
    end

    @test sum(@data(Int[missing])) === missing
    @test sum(@data(Int[missing]); skipmissing=true) === 0
    @test sum(@data(Int[missing, missing])) === missing
    @test sum(@data(Int[missing, missing]); skipmissing=true) === 0
    @test sum(@data(Int[missing, missing, 1]); skipmissing=true) === 1
    @test sum(@data(Int[missing, missing, 1, 2]); skipmissing=true) === 3
    @test sum(@data(Int[missing, 1, missing, 1, 2]); skipmissing=true) === 4

    z = DataArray(reshape(1:16, (2,2,2,2)))
    z[6] = missing
    fz = convert(DataArray{Float64}, z)
    bfz = convert(DataArray{BigFloat}, z)
    @test ismissing(sum(z))
    @test ismissing(sum(fz))
    @test ismissing(sum(bfz))
    @test sum(z; skipmissing=true) === 130
    @test sum(fz; skipmissing=true) === 130.0
    @test sum(bfz; skipmissing=true) == 130

    bs = DataArrays.sum_pairwise_blocksize(identity)
    for n in [bs-64, bs-1, bs, bs+1, bs+2, 2*bs-2:2*bs+3..., 4*bs-2:4*bs+3...]
        da = DataArray(randn(n))
        s = sum(da.data)
        @test sum(da) ≈ s
        @test sum(da; skipmissing=true) ≈ s

        da2 = copy(da)
        da2[1:2:end] = missing
        @test ismissing(sum(da2))
        @test sum(da2; skipmissing=true) ≈ sum(skipmissing(da2))

        da2 = convert(DataArray{BigFloat}, da2)
        @test ismissing(sum(da2))
        @test sum(da2; skipmissing=true) ≈ sum(skipmissing(da2))

        da2 = copy(da)
        da2[2:2:end] = missing
        @test ismissing(sum(da2))
        @test sum(da2; skipmissing=true) ≈ sum(skipmissing(da2))

        da2 = convert(DataArray{BigFloat}, da2)
        @test ismissing(sum(da2))
        @test sum(da2; skipmissing=true) ≈ sum(skipmissing(da2))
    end

    ## other reductions

    _varuc(x; kw...) = var(x; corrected=false, kw...)
    _varzm(x; kw...) = var(x; mean=0, kw...)
    _varzmuc(x; kw...) = var(x; corrected=false, mean=0, kw...)
    _var1m(x; kw...) = var(x; mean=1, kw...)
    _var1muc(x; kw...) = var(x; corrected=false, mean=1, kw...)
    _std1m(x; kw...) = stdm(x, 1; kw...)

    for fn in (prod, minimum, maximum, mean,
               var, _varuc, _varzm, _varzmuc, _var1m, _var1muc,
               std, _std1m, Base.sumabs, Base.sumabs2)
        for n in [0, 1, 2, 62, 63, 64, 65, 66]
            da = DataArray(randn(n))
            @same_behavior fn(da) fn(da.data)
            @same_behavior fn(da; skipmissing=true) fn(da.data)

            da2 = copy(da)
            da2[1:2:end] = missing
            n > 0 && @test ismissing(fn(da2))
            @same_behavior fn(da2; skipmissing=true) fn(skipmissing(da2))

            da2 = convert(DataArray{BigFloat}, da2)
            n > 0 && @test ismissing(fn(da2))
            @same_behavior fn(da2; skipmissing=true) fn(skipmissing(da2))

            da2 = copy(da)
            da2[2:2:end] = missing
            n > 1 && @test ismissing(fn(da2))
            @same_behavior fn(da2; skipmissing=true) fn(skipmissing(da2))

            da2 = convert(DataArray{BigFloat}, da2)
            n > 1 && @test ismissing(fn(da2))
            @same_behavior fn(da2; skipmissing=true) fn(skipmissing(da2))
        end
    end

    ## reduce and mapreduce drivers

    for fn in (+, *, |, &)
        da = convert(DataArray, bitrand(10))

        s = mapreduce(identity, fn, da.data)
        @test mapreduce(identity, fn, da) == s
        @test mapreduce(identity, fn, da; skipmissing=true) == s
        @test reduce(fn, da) == s
        @test reduce(fn, da; skipmissing=true) == s
    end

    # make sure reductions of & and | are still calling Base
    @test ismissing(reduce(&, @data([true, missing])))
    @test !reduce(&, @data([false, missing]))
    @test reduce(|, @data([true, missing]))
    @test ismissing(reduce(|, @data([false, missing])))

    # weighted mean
    da1 = DataArray(randn(128))
    da2 = DataArray(randn(128))
    @same_behavior mean(da1, weights(da2)) mean(da1.data, weights(da2.data))
    @same_behavior mean(da1, weights(da2.data)) mean(da1.data, weights(da2.data))
    @same_behavior mean(da1, weights(da2); skipmissing=true) mean(da1.data, weights(da2.data))
    @same_behavior mean(da1, weights(da2.data); skipmissing=true) mean(da1.data, weights(da2.data))

    da1[1:3:end] = missing
    @same_behavior mean(da1, weights(da2); skipmissing=true) mean(skipmissing(da1), weights(da2.data[(!).(da1.na)]))
    @same_behavior mean(da1, weights(da2.data); skipmissing=true) mean(skipmissing(da1), weights(da2.data[(!).(da1.na)]))

    da2[1:2:end] = missing
    keep = .!da1.na .& .!da2.na
    @same_behavior mean(da1, weights(da2); skipmissing=true) mean(da1.data[keep], weights(da2.data[keep]))
end
