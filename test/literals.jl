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
end
