@testset "Constructors" begin
    #
    # nulls
    #

    @test isnull(Null())
    @test isnull(null)

    #
    # DataVector's
    #

    dv = DataArray([1, 2, 3], falses(3))
    @test isequal(dv.data, [1, 2, 3])
    @test isequal(dv.na, falses(3))
    @test isequal(dv, DataArray([1, 2, 3], [false, false, false]))
    @test isequal(dv, DataArray([1, 2, 3]))

    dv = convert(DataArray, trues(3))
    @test isequal(dv.data, [true, true, true])
    @test isequal(dv.na, falses(3))
    @test isequal(dv, convert(DataArray, trues(3)))

    dv = DataArray([1, 2, 3], falses(3))
    @test isequal(dv, convert(DataArray, 1:3))

    dv = DataArray(Int, 3)
    @test isequal(eltype(dv), Union{Int,Null})
    @test isequal(dv.na, trues(3))

    dv = convert(DataArray, zeros(3))
    @test isequal(dv, convert(DataArray, zeros(3)))

    dv = convert(DataArray, zeros(Int, 3))
    @test isequal(dv, convert(DataArray, zeros(Int, 3)))

    dv = convert(DataArray, ones(3))
    @test isequal(dv, convert(DataArray, ones(3)))

    dv = convert(DataArray, ones(Int, 3))
    @test isequal(dv, convert(DataArray, ones(Int, 3)))

    dv = convert(DataArray, falses(3))
    @test isequal(dv, convert(DataArray, falses(3)))

    dv = convert(DataArray, trues(3))
    @test isequal(dv, convert(DataArray, trues(3)))

    @test DataArray([1, null]) isa DataVector{Int}
    @test isequal(DataArray([1, null]), [1, null])
    @test DataArray{Int}([1, null]) isa DataVector{Int}
    @test isequal(DataArray{Int}([1, null]), [1, null])
    @test DataArray{Any}([1, null]) isa DataVector{Any}
    @test isequal(DataArray{Any}([1, null]), [1, null])
    @test DataArray{Int, 1}([1, null]) isa DataVector{Int}
    @test isequal(DataArray{Int, 1}([1, null]), [1, null])
    @test DataArray{Any, 1}([1, null]) isa DataVector{Any}
    @test isequal(DataArray{Any, 1}([1, null]), [1, null])

    #
    # PooledDataArray's
    #

    pdv = PooledDataArray([1, 2, 3], falses(3))
    @test all(pdv .== [1, 2, 3])
    @test all(isnull.(pdv) .== falses(3))

    @test isequal(pdv, PooledDataArray([1, 2, 3], [false, false, false]))
    @test isequal(pdv, PooledDataArray([1, 2, 3]))

    pdv = convert(PooledDataArray, trues(3))
    @test all(pdv .== [true, true, true])
    @test all(isnull.(pdv) .== falses(3))
    @test isequal(pdv, convert(PooledDataArray, trues(3)))

    pdv = PooledDataArray([1, 2, 3], falses(3))
    @test isequal(pdv, convert(PooledDataArray, 1:3))
    @test isequal(pdv, convert(PooledDataArray, PooledDataArray([1, 2, 3])))

    pdv = PooledDataArray(Int, 3)
    @test isequal(eltype(pdv), Union{Int, Null})
    @test all(isnull.(pdv) .== trues(3))

    pdv = convert(PooledDataArray, zeros(3))
    @test isequal(pdv, convert(PooledDataArray, zeros(3)))

    pdv = convert(PooledDataArray, zeros(Int, 3))
    @test isequal(pdv, convert(PooledDataArray, zeros(Int, 3)))

    pdv = convert(PooledDataArray, ones(3))
    @test isequal(pdv, convert(PooledDataArray, ones(3)))

    pdv = convert(PooledDataArray, ones(Int, 3))
    @test isequal(pdv, convert(PooledDataArray, ones(Int, 3)))

    pdv = convert(PooledDataArray, falses(3))
    @test isequal(pdv, convert(PooledDataArray, falses(3)))

    pdv = convert(PooledDataArray, trues(3))
    @test isequal(pdv, convert(PooledDataArray, trues(3)))

    #
    # DataMatrix
    #

    dm = DataArray([1 2; 3 4], falses(2, 2))
    @test isequal(dm.data, [1 2; 3 4])
    @test isequal(dm.na, falses(2, 2))

    @test isequal(dm, DataArray([1 2; 3 4], [false false; false false]))
    @test isequal(dm, DataArray([1 2; 3 4]))

    dm = convert(DataArray, trues(2, 2))
    @test isequal(dm.data, trues(2, 2))
    @test isequal(dm.na, falses(2, 2))

    @test isequal(dm, convert(DataArray, trues(2, 2)))

    dm = DataArray(Int, 2, 2)
    @test isequal(eltype(dm), Union{Int, Null})
    @test isequal(dm.na, trues(2, 2))

    @test_nowarn convert(DataArray, zeros(2, 2))

    @test_nowarn convert(DataArray, zeros(Int, 2, 2))

    @test_nowarn convert(DataArray, ones(2, 2))
    @test_nowarn convert(DataArray, ones(Int, 2, 2))

    @test_nowarn convert(DataArray, falses(2, 2))
    @test_nowarn convert(DataArray, trues(2, 2))

    @test_nowarn convert(DataArray, eye(3, 2))
    @test_nowarn convert(DataArray, eye(2))
    @test_nowarn convert(DataArray, diagm(Float64[pi, pi]))

    @test DataArray([1 null]) isa DataMatrix{Int}
    @test isequal(DataArray([1 null]), [1 null])
    @test DataArray{Int}([1 null]) isa DataMatrix{Int}
    @test isequal(DataArray{Int}([1 null]), [1 null])
    @test DataArray{Any}([1 null]) isa DataMatrix{Any}
    @test isequal(DataArray{Any}([1 null]), [1 null])
    @test DataArray{Int, 2}([1 null]) isa DataMatrix{Int}
    @test isequal(DataArray{Int, 2}([1 null]), [1 null])
    @test DataArray{Any, 2}([1 null]) isa DataMatrix{Any}
    @test isequal(DataArray{Any, 2}([1 null]), [1 null])
end
