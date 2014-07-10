module TestSort
using DataArrays, Base.Test

for T in (Float64, BigFloat)
    n = 1000
    na = randbool(n)
    nna = sum(na)
    a = Array(T, n)
    ra = randn(n-nna)
    a[!na] = ra
    da = DataArray(a, na)
    @test isequal(sort(da), [DataArray(sort(dropna(da))), DataArray(T, nna)])
    @test isequal(da[sortperm(da)], [DataArray(sort(dropna(da))), DataArray(T, nna)])
    @test isequal(sort(da, rev=true), [DataArray(T, nna), DataArray(sort(dropna(da), rev=true))])
    @test isequal(da[sortperm(da, rev=true)], [DataArray(T, nna), DataArray(sort(dropna(da), rev=true))])
end
end