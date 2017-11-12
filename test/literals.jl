@testset "Literals" begin
    dv = @data []
    @test isequal(dv, DataArray([], Bool[]))
    @test typeof(dv) == DataVector{Any}

    dv = @data Float64[]
    @test isequal(dv, DataArray(Float64[], Bool[]))
    @test typeof(dv) == DataVector{Float64}

    dv = @data [1, missing, 3]
    @test isequal(dv,
                  DataArray([1, 0, 3],
                            [false, true, false]))

    dv = @data [1 missing 3]
    @test isequal(dv,
                  DataArray([1 0 3],
                            [false true false]))

    dv = @data Float64[1, missing, 3]
    @test isequal(dv,
                  DataArray(Float64[1, 0, 3],
                            [false, true, false]))
    @test typeof(dv) == DataVector{Float64}

    dv = @data Float64[1 missing 3]
    @test isequal(dv,
                  DataArray(Float64[1 0 3],
                            [false true false]))
    @test typeof(dv) == DataMatrix{Float64}

    dv = @data [missing, missing]
    @test isequal(dv, DataArray(Any, 2))
    @test typeof(dv) == DataVector{Any}

    dv = @data [missing missing]
    @test isequal(dv, DataArray(Any, 1, 2))
    @test typeof(dv) == DataMatrix{Any}

    dm = @data [1 missing; 3 4]
    @test isequal(dm,
                  DataArray([1 0; 3 4],
                            [false true; false false]))

    dm = @data Float64[1 missing; 3 4]
    @test isequal(dm,
                  DataArray(Float64[1 0; 3 4],
                            [false true; false false]))
    @test typeof(dm) == DataMatrix{Float64}

    dm = @data [missing missing; missing missing]
    @test isequal(dm, DataArray(Any, 2, 2))
    @test typeof(dm) == DataMatrix{Any}

    pdv = @pdata [1, missing, 3]
    @test isequal(pdv,
                  PooledDataArray([1, 0, 3],
                                  [false, true, false]))

    pdv = @pdata Float64[1, missing, 3]
    @test isequal(pdv,
                  PooledDataArray(Float64[1, 0, 3],
                                  [false, true, false]))
    @test typeof(pdv) == PooledDataArray{Float64,UInt32,1}

    pdv = @pdata [1 missing 3]
    @test isequal(pdv,
                  PooledDataArray([1 0 3],
                                  [false true false]))

    pdv = @pdata Float64[1 missing 3]
    @test isequal(pdv,
                  PooledDataArray(Float64[1 0 3],
                                  [false true false]))
    @test typeof(pdv) == PooledDataArray{Float64,UInt32,2}

    pdm = @pdata [1 missing; 3 4]
    @test isequal(pdm,
                  PooledDataArray([1 0; 3 4],
                                  [false true; false false]))

    pdm = @pdata Float64[1 missing; 3 4]
    @test isequal(pdm,
                  PooledDataArray(Float64[1 0; 3 4],
                                  [false true; false false]))
    @test typeof(pdm) == PooledDataArray{Float64,UInt32,2}

    pdm = @pdata [1 missing;
                3 4]
    @test isequal(pdm,
                  PooledDataArray([1 0; 3 4],
                                  [false true; false false]))

    dv1 = @data zeros(4)
    dv2 = @data ones(4)
    dv3 = @data rand(4)

    dm1 = @data zeros(4, 4)
    dm2 = @data ones(4, 4)
    dm3 = @data rand(4, 4)

    pdv1 = @pdata zeros(4)
    pdv2 = @pdata ones(4)
    pdv3 = @pdata rand(4)

    pdm1 = @pdata zeros(4, 4)
    pdm2 = @pdata ones(4, 4)
    pdm3 = @pdata rand(4, 4)

    mixed1 = @data ["x", 1, 1.23, missing]
    mixed2 = @data [missing, "x", 1, 1.23, missing]

    @test isequal(mixed1, DataArray(Any["x", 1, 1.23, 0],
                                    [false, false, false, true]))
    @test isequal(mixed2, DataArray(Any[missing, "x", 1, 1.23, 0],
                                    [true, false, false, false, true]))

    x = 5.1
    ex = :([1, 2, 3])
    DataArrays.parsedata(ex)
    @test isequal(@data([1, 2, 3]),
                  DataArray([1, 2, 3], [false, false, false]))

    ex = :([1, 2, 3.0])
    DataArrays.parsedata(ex)
    @test isequal(@data([1, 2, 3.0]),
                  DataArray([1, 2, 3.0], [false, false, false]))

    ex = :([1, 2, x])
    DataArrays.parsedata(ex)
    @test isequal(@data([1, 2, x]),
                  DataArray([1, 2, x], [false, false, false]))

    ex = :([1, 2, missing])
    DataArrays.parsedata(ex)
    @test isequal(@data([1, 2, missing]),
                  DataArray([1, 2, 1], [false, false, true]))

    ex = :([1, 2, x, missing])
    DataArrays.parsedata(ex)
    @test isequal(@data([1, 2, x, missing]),
                  DataArray([1, 2, x, 1], [false, false, false, true]))

    # Matrices
    ex = :([1 2; 3 4])
    DataArrays.parsedata(ex)
    @data([1 2; 3 4])
    @test isequal(@data([1 2; 3 4]),
                  DataArray([1 2; 3 4], [false false; false false]))

    ex = :([1 2; 3.0 4])
    DataArrays.parsedata(ex)
    @test isequal(@data([1 2; 3.0 4]),
                  DataArray([1 2; 3.0 4], [false false; false false]))

    ex = :([1 2; x x])
    DataArrays.parsedata(ex)
    @test isequal(@data([1 2; x x]),
                  DataArray([1 2; x x], [false false; false false]))

    ex = :([1 2; missing missing])
    DataArrays.parsedata(ex)
    @test isequal(@data([1 2; missing missing]),
                  DataArray([1 2; 1 1], [false false; true true]))

    ex = :([1 2; x missing])
    DataArrays.parsedata(ex)
    @test isequal(@data([1 2; x missing]),
                  DataArray([1 2; x 1], [false false; false true]))

    # Complex vector expressions
    ex = :([1 + 1, 2 + 2, x * x, missing])
    DataArrays.parsedata(ex)
    @test isequal(@data([1 + 1, 2 + 2, x * x, missing]),
                  DataArray([1 + 1, 2 + 2, x * x, 1],
                            [false, false, false, true]))

    ex = :([sin(1), cos(2) + cos(2), exp(x * x), sum([1, 1, 1])])
    DataArrays.parsedata(ex)
    @test isequal(@data([sin(1),
                         cos(2) + cos(2),
                         exp(x * x),
                         sum([1, 1, 1])]),
                  DataArray([sin(1),
                             cos(2) + cos(2),
                             exp(x * x),
                             sum([1, 1, 1])],
                            [false, false, false, false]))

    ex = :([1 + 1im, 2 + 2im])
    DataArrays.parsedata(ex)
    @test isequal(@data([1 + 1im, 2 + 2im]),
                  DataArray([1 + 1im, 2 + 2im],
                            [false, false]))

    # Complex matrix expressions
    ex = :([1 + 1 2 + 2; x * x missing])
    DataArrays.parsedata(ex)
    @test isequal(@data([1 + 1 2 + 2; x * x missing]),
                  DataArray([1 + 1 2 + 2; x * x 1],
                            [false false; false true]))

    ex = :([sin(1) cos(2) + cos(2);
            exp(x * x) sum([1, 1, 1])])
    DataArrays.parsedata(ex)
    @test isequal(@data([sin(1) cos(2) + cos(2);
                        exp(x * x) sum([1, 1, 1])]),
                  DataArray([sin(1) cos(2) + cos(2);
                             exp(x * x) sum([1, 1, 1])],
                            [false false;
                             false false]))

    @test isequal(DataArrays.fixargs(:([1, 2, missing, x]).args, -1),
                  (Any[1, 2, -1, :x], Any[false, false, true, false]))

    @test isequal(DataArrays.findstub_vector(:([1, 2, missing, x])), 1)
    @test isequal(DataArrays.findstub_vector(:([missing, missing, missing, x])), :x)

    # Lots of variables
    a, b, c, d = 1, 2, 3, 4
    @data [a, b, c, d]
end
