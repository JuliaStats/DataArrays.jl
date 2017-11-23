@testset "Sort" begin
    dv1 = @data([9, 1, 8, missing, 3, 3, 7, missing])
    dv2 = 1.0 * dv1
    dv3 = DataArray(collect(1:8))
    pdv1 = convert(PooledDataArray, dv1)

    @test sortperm(dv1) == sortperm(dv2)
    @test sortperm(dv1) == sortperm(pdv1)
    @test isequal(sort(dv1), convert(DataArray, sort(dv1)))
    @test isequal(sort(dv1), convert(DataArray, sort(pdv1)))

    for T in (Float64, BigFloat)
        n = 1000
        na = bitrand(n)
        nna = sum(na)
        a = Vector{T}(n)
        ra = randn(n-nna)
        a[.!na] = ra
        for da in (DataArray(a, na), PooledDataArray(a, na), (pda = PooledDataArray(a, na); setlevels!(pda, shuffle!(pda.pool))))
            @test isequal(sort(da), [DataArray(sort!(collect(skipmissing(da)))); DataArray(T, nna)])
            @test isequal(sort(da; lt=(x,y)->isless(x,y)), [DataArray(sort!(collect(skipmissing(da)))); DataArray(T, nna)])
            @test isequal(da[sortperm(da)], [DataArray(sort!(collect(skipmissing(da)))); DataArray(T, nna)])
            @test isequal(sort(da, rev=true), [DataArray(T, nna); DataArray(sort!(collect(skipmissing(da)), rev=true))])
            @test isequal(da[sortperm(da, rev=true)], [DataArray(T, nna); DataArray(sort!(collect(skipmissing(da)), rev=true))])
        end
    end
end
