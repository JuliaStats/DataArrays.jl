module TestLiterals
    using Base.Test
    using DataArrays

    dv = @data [1, NA, 3]
    @test isequal(dv,
                  DataArray([1, 0, 3],
                            [false, true, false]))

    dm = @data [1 NA; 3 4]
    @test isequal(dm,
                  DataArray([1 0; 3 4],
                            [false true; false false]))

    dm = @data [1 NA;
                3 4]
    @test isequal(dm,
                  DataArray([1 0; 3 4],
                            [false true; false false]))

    pdv = @pdata [1, NA, 3]
    @test isequal(pdv,
                  PooledDataArray([1, 0, 3],
                                  [false, true, false]))

    pdm = @pdata [1 NA; 3 4]
    @test isequal(pdm,
                  PooledDataArray([1 0; 3 4],
                                  [false true; false false]))

    pdm = @pdata [1 NA;
                3 4]
    @test isequal(pdm,
                  PooledDataArray([1 0; 3 4],
                                  [false true; false false]))
end
