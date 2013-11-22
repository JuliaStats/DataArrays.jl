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

	dv = DataArray(trues(3), falses(3))
	@assert isequal(dv.data, [true, true, true])
	@assert isequal(dv.na, falses(3))
	@assert isequal(dv, DataArray(trues(3)))

	dv = DataArray([1, 2, 3], falses(3))
	@assert isequal(dv, DataArray(1:3))

	dv = DataArray(Int, 3)
	@assert isequal(eltype(dv), Int)
	@assert isequal(dv.na, trues(3))

	dv = @data zeros(3)
	@assert isequal(dv, DataArray(zeros(3)))
	dv = @data zeros(Int, 3)
	@assert isequal(dv, DataArray(zeros(Int, 3)))
	dv = @data ones(3)
	@assert isequal(dv, DataArray(ones(3)))
	dv = @data ones(Int, 3)
	@assert isequal(dv, DataArray(ones(Int, 3)))
	dv = @data falses(3)
	@assert isequal(dv, DataArray(falses(3)))
	dv = @data trues(3)
	@assert isequal(dv, DataArray(trues(3)))

	#
	# PooledDataArray's
	#

	pdv = PooledDataArray([1, 2, 3], falses(3))
	@assert all(pdv .== [1, 2, 3])
	@assert all(isna(pdv) .== falses(3))

	@assert isequal(pdv, PooledDataArray([1, 2, 3], [false, false, false]))
	@assert isequal(pdv, PooledDataArray([1, 2, 3]))

	pdv = PooledDataArray(trues(3), falses(3))
	@assert all(pdv .== [true, true, true])
	@assert all(isna(pdv) .== falses(3))
	@assert isequal(pdv, PooledDataArray(trues(3)))

	pdv = PooledDataArray([1, 2, 3], falses(3))
	@assert isequal(pdv, PooledDataArray(1:3))
	@assert isequal(pdv, PooledDataArray(PooledDataArray([1, 2, 3])))

	pdv = PooledDataArray(Int, 3)
	@assert isequal(eltype(pdv), Int)
	@assert all(isna(pdv) .== trues(3))

	pdv = @pdata zeros(3)
	@assert isequal(pdv, PooledDataArray(zeros(3)))
	pdv = @pdata zeros(Int, 3)
	@assert isequal(pdv, PooledDataArray(zeros(Int, 3)))
	pdv = @pdata ones(3)
	@assert isequal(pdv, PooledDataArray(ones(3)))
	pdv = @pdata ones(Int, 3)
	@assert isequal(pdv, PooledDataArray(ones(Int, 3)))
	pdv = @pdata falses(3)
	@assert isequal(pdv, PooledDataArray(falses(3)))
	pdv = @pdata trues(3)
	@assert isequal(pdv, PooledDataArray(trues(3)))

	#
	# DataMatrix
	#

	dm = DataArray([1 2; 3 4], falses(2, 2))
	@assert isequal(dm.data, [1 2; 3 4])
	@assert isequal(dm.na, falses(2, 2))

	@assert isequal(dm, DataArray([1 2; 3 4], [false false; false false]))
	@assert isequal(dm, DataArray([1 2; 3 4]))

	dm = DataArray(trues(2, 2), falses(2, 2))
	@assert isequal(dm.data, trues(2, 2))
	@assert isequal(dm.na, falses(2, 2))

	@assert isequal(dm, DataArray(trues(2, 2)))

	dm = DataArray(Int, 2, 2)
	@assert isequal(eltype(dm), Int)
	@assert isequal(dm.na, trues(2, 2))

	@assert isequal((@data zeros(2, 2)), DataArray(zeros(2, 2)))
	@assert isequal((@data zeros(Int, 2, 2)), DataArray(zeros(Int, 2, 2)))

	@assert isequal((@data ones(2, 2)), DataArray(ones(2, 2)))
	@assert isequal((@data ones(Int, 2, 2)), DataArray(ones(Int, 2, 2)))

	@assert isequal((@data falses(2, 2)), DataArray(falses(2, 2)))
	@assert isequal((@data trues(2, 2)), DataArray(trues(2, 2)))

	@assert isequal((@data eye(3, 2)), DataArray(eye(3, 2)))
	@assert isequal((@data eye(2)), DataArray(eye(2)))
	@assert isequal((@data diagm(Float64[pi, pi])), DataArray(diagm(Float64[pi, pi])))
end
