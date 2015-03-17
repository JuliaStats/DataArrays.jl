module TestReduce
using DataArrays, Base.Test, StatsBase, Compat

srand(1337)

## extended test of sum

for skipna in (true, false)
    @test sum(@data(Int8[]); skipna=skipna) === 0
    @test sum(@data(Int[]); skipna=skipna) === 0
    @test sum(@data(Float64[]); skipna=skipna) === 0.0

    @test sum(@data([@compat(Int8(3))]); skipna=skipna) === 3
    @test sum(@data([3]); skipna=skipna) === 3
    @test sum(@data([3.0]); skipna=skipna) === 3.0

    z = DataArray(reshape(1:16, (2,2,2,2)))
    fz = convert(DataArray{Float64}, z)
    bfz = convert(DataArray{BigFloat}, z)
    @test sum(z) === 136
    @test sum(fz) === 136.0
    @test sum(bfz) == 136
end

@test sum(@data(Int[NA])) === NA
@test sum(@data(Int[NA]); skipna=true) === 0
@test sum(@data(Int[NA, NA])) === NA
@test sum(@data(Int[NA, NA]); skipna=true) === 0
@test sum(@data(Int[NA, NA, 1]); skipna=true) === 1
@test sum(@data(Int[NA, NA, 1, 2]); skipna=true) === 3
@test sum(@data(Int[NA, 1, NA, 1, 2]); skipna=true) === 4

z = DataArray(reshape(1:16, (2,2,2,2)))
z[6] = NA
fz = convert(DataArray{Float64}, z)
bfz = convert(DataArray{BigFloat}, z)
@test isna(sum(z))
@test isna(sum(fz))
@test isna(sum(bfz))
@test sum(z; skipna=true) === 130
@test sum(fz; skipna=true) === 130.0
@test sum(bfz; skipna=true) == 130

bs = Base.sum_pairwise_blocksize(Base.IdFun())
for n in [bs-64, bs-1, bs, bs+1, bs+2, 2*bs-2:2*bs+3..., 4*bs-2:4*bs+3...]
    da = DataArray(randn(n))
    s = sum(da.data)
    @test_approx_eq sum(da) s
    @test_approx_eq sum(da; skipna=true) s

    da2 = copy(da)
    da2[1:2:end] = NA
    @test isna(sum(da2))
    @test_approx_eq sum(da2; skipna=true) sum(dropna(da2))

    da2 = convert(DataArray{BigFloat}, da2)
    @test isna(sum(da2))
    @test_approx_eq sum(da2; skipna=true) sum(dropna(da2))

    da2 = copy(da)
    da2[2:2:end] = NA
    @test isna(sum(da2))
    @test_approx_eq sum(da2; skipna=true) sum(dropna(da2))

    da2 = convert(DataArray{BigFloat}, da2)
    @test isna(sum(da2))
    @test_approx_eq sum(da2; skipna=true) sum(dropna(da2))
end

## other reductions

macro same_behavior(ex1, ex2)
    quote
        v = try
            $ex2
        catch e
            e
        end
        isa(v, Exception) ? @test_throws(typeof(v), $ex1) : @test_approx_eq($ex1, v)
    end
end

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
        @same_behavior fn(da; skipna=true) fn(da.data)

        da2 = copy(da)
        da2[1:2:end] = NA
        n > 0 && @test isna(fn(da2))
        @same_behavior fn(da2; skipna=true) fn(dropna(da2))

        da2 = convert(DataArray{BigFloat}, da2)
        n > 0 && @test isna(fn(da2))
        @same_behavior fn(da2; skipna=true) fn(dropna(da2))

        da2 = copy(da)
        da2[2:2:end] = NA
        n > 1 && @test isna(fn(da2))
        @same_behavior fn(da2; skipna=true) fn(dropna(da2))

        da2 = convert(DataArray{BigFloat}, da2)
        n > 1 && @test isna(fn(da2))
        @same_behavior fn(da2; skipna=true) fn(dropna(da2))
    end
end

## reduce and mapreduce drivers

for fn in (+, *, |, &)
    da = convert(DataArray, bitrand(10))

    s = mapreduce(Base.IdFun(), fn, da.data)
    @test mapreduce(Base.IdFun(), fn, da) == s
    @test mapreduce(Base.IdFun(), fn, da; skipna=true) == s
    @test reduce(fn, da) == s
    @test reduce(fn, da; skipna=true) == s
end

# make sure reductions of & and | are still calling Base
@test isna(reduce(&, @data([true, NA])))
@test !reduce(&, @data([false, NA]))
@test reduce(|, @data([true, NA]))
@test isna(reduce(|, @data([false, NA])))

# weighted mean
da1 = DataArray(randn(128))
da2 = DataArray(randn(128))
@same_behavior mean(da1, weights(da2)) mean(da1.data, weights(da2.data))
@same_behavior mean(da1, weights(da2.data)) mean(da1.data, weights(da2.data))
@same_behavior mean(da1, weights(da2); skipna=true) mean(da1.data, weights(da2.data))
@same_behavior mean(da1, weights(da2.data); skipna=true) mean(da1.data, weights(da2.data))

da1[1:3:end] = NA
@same_behavior mean(da1, weights(da2); skipna=true) mean(dropna(da1), weights(da2.data[!da1.na]))
@same_behavior mean(da1, weights(da2.data); skipna=true) mean(dropna(da1), weights(da2.data[!da1.na]))

da2[1:2:end] = NA
keep = !da1.na & !da2.na
@test isna(mean(da1, weights(da2)))
@same_behavior mean(da1, weights(da2); skipna=true) mean(da1.data[keep], weights(da2.data[keep]))
end
