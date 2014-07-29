module TestConstructors
    using Base.Test
    using DataArrays

    #
    # NA's
    #

    @assert isna(NAtype())
    @assert isna(NA)

    #
    # DataVector's
    #

    dv = DataArray([1, 2, 3], falses(3))
    @assert isequal(dv.data, [1, 2, 3])
    @assert isequal(dv.na, falses(3))
    @assert isequal(dv, DataArray([1, 2, 3], [false, false, false]))
    @assert isequal(dv, DataArray([1, 2, 3]))

    dv = convert(DataArray, trues(3))
    @assert isequal(dv.data, [true, true, true])
    @assert isequal(dv.na, falses(3))
    @assert isequal(dv, convert(DataArray, trues(3)))

    dv = DataArray([1, 2, 3], falses(3))
    @assert isequal(dv, convert(DataArray, 1:3))

    dv = DataArray(Int, 3)
    @assert isequal(eltype(dv), Int)
    @assert isequal(dv.na, trues(3))

    dv = convert(DataArray, zeros(3))
    @assert isequal(dv, convert(DataArray, zeros(3)))

    dv = convert(DataArray, zeros(Int, 3))
    @assert isequal(dv, convert(DataArray, zeros(Int, 3)))

    dv = convert(DataArray, ones(3))
    @assert isequal(dv, convert(DataArray, ones(3)))

    dv = convert(DataArray, ones(Int, 3))
    @assert isequal(dv, convert(DataArray, ones(Int, 3)))

    dv = convert(DataArray, falses(3))
    @assert isequal(dv, convert(DataArray, falses(3)))

    dv = convert(DataArray, trues(3))
    @assert isequal(dv, convert(DataArray, trues(3)))

    #
    # PooledDataArray's
    #

    pdv = PooledDataArray([1, 2, 3], falses(3))
    @assert all(pdv .== [1, 2, 3])
    @assert all(isna(pdv) .== falses(3))

    @assert isequal(pdv, PooledDataArray([1, 2, 3], [false, false, false]))
    @assert isequal(pdv, PooledDataArray([1, 2, 3]))

    pdv = convert(PooledDataArray, trues(3))
    @assert all(pdv .== [true, true, true])
    @assert all(isna(pdv) .== falses(3))
    @assert isequal(pdv, convert(PooledDataArray, trues(3)))

    pdv = PooledDataArray([1, 2, 3], falses(3))
    @assert isequal(pdv, convert(PooledDataArray, 1:3))
    @assert isequal(pdv, convert(PooledDataArray, PooledDataArray([1, 2, 3])))

    pdv = PooledDataArray(Int, 3)
    @assert isequal(eltype(pdv), Int)
    @assert all(isna(pdv) .== trues(3))

    pdv = convert(PooledDataArray, zeros(3))
    @assert isequal(pdv, convert(PooledDataArray, zeros(3)))

    pdv = convert(PooledDataArray, zeros(Int, 3))
    @assert isequal(pdv, convert(PooledDataArray, zeros(Int, 3)))

    pdv = convert(PooledDataArray, ones(3))
    @assert isequal(pdv, convert(PooledDataArray, ones(3)))

    pdv = convert(PooledDataArray, ones(Int, 3))
    @assert isequal(pdv, convert(PooledDataArray, ones(Int, 3)))

    pdv = convert(PooledDataArray, falses(3))
    @assert isequal(pdv, convert(PooledDataArray, falses(3)))

    pdv = convert(PooledDataArray, trues(3))
    @assert isequal(pdv, convert(PooledDataArray, trues(3)))

    #
    # DataMatrix
    #

    dm = DataArray([1 2; 3 4], falses(2, 2))
    @assert isequal(dm.data, [1 2; 3 4])
    @assert isequal(dm.na, falses(2, 2))

    @assert isequal(dm, DataArray([1 2; 3 4], [false false; false false]))
    @assert isequal(dm, DataArray([1 2; 3 4]))

    dm = convert(DataArray, trues(2, 2))
    @assert isequal(dm.data, trues(2, 2))
    @assert isequal(dm.na, falses(2, 2))

    @assert isequal(dm, convert(DataArray, trues(2, 2)))

    dm = DataArray(Int, 2, 2)
    @assert isequal(eltype(dm), Int)
    @assert isequal(dm.na, trues(2, 2))

    convert(DataArray, zeros(2, 2))

    convert(DataArray, zeros(Int, 2, 2))

    convert(DataArray, ones(2, 2))
    convert(DataArray, ones(Int, 2, 2))

    convert(DataArray, falses(2, 2))
    convert(DataArray, trues(2, 2))

    convert(DataArray, eye(3, 2))
    convert(DataArray, eye(2))
    convert(DataArray, diagm(Float64[pi, pi]))
end
