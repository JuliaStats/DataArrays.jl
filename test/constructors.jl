module DataArraysConstructors
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
	@assert isequal(dv, DataArray(DataArray([1, 2, 3])))

	dv = DataArray(Int, 3)
	@assert isequal(eltype(dv), Int)
	@assert isequal(dv.na, trues(3))

	# dv = DataArray(3)
	# @assert isequal(eltype(dv), Float64)
	# @assert isequal(dv.na, trues(3))

	# dv = DataArray()
	# @assert isequal(eltype(dv), Float64)
	# @assert isequal(dv.na, trues(0))

	@assert isequal(datazeros(3), DataArray(zeros(3)))
	@assert isequal(datazeros(Int, 3), DataArray(zeros(Int, 3)))
	@assert isequal(dataones(3), DataArray(ones(3)))
	@assert isequal(dataones(Int, 3), DataArray(ones(Int, 3)))
	@assert isequal(datafalses(3), DataArray(falses(3)))
	@assert isequal(datatrues(3), DataArray(trues(3)))

	dv = DataVector[1, 2, NA]
	@assert dv[1] == 1
	@assert dv[2] == 2
	@assert isna(dv[3])
	@assert isequal(eltype(dv), Int)

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

	# pdv = PooledDataArray(3)
	# @assert isequal(eltype(pdv), Float64)
	# @assert all(isna(pdv) .== trues(3))

	# pdv = PooledDataArray()
	# @assert isequal(eltype(pdv), Float64)
	# @assert all(isna(pdv) .== trues(0))

	@assert isequal(pdatazeros(3), PooledDataArray(zeros(3)))
	@assert isequal(pdatazeros(Int, 3), PooledDataArray(zeros(Int, 3)))
	@assert isequal(pdataones(3), PooledDataArray(ones(3)))
	@assert isequal(pdataones(Int, 3), PooledDataArray(ones(Int, 3)))
	@assert isequal(pdatafalses(3), PooledDataArray(falses(3)))
	@assert isequal(pdatatrues(3), PooledDataArray(trues(3)))

	pdv = PooledDataVector[1, 2, NA]
	@assert pdv[1] == 1
	@assert pdv[2] == 2
	@assert isna(pdv[3])
	@assert isequal(eltype(pdv), Int)

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

	#DataMatrix(dvzeros(3), dvzeros(3))
	#DataMatrix(1:3, 1:3)

	@assert isequal(DataArray([1 2; 3 4]), DataArray(DataArray([1 2; 3 4])))

	dm = DataArray(Int, 2, 2)
	@assert isequal(eltype(dm), Int)
	@assert isequal(dm.na, trues(2, 2))

	# dm = DataArray(2, 2)
	# @assert isequal(eltype(dm), Float64)
	# @assert isequal(dm.na, trues(2, 2))

	# dm = DataArray(Int)
	# @assert isequal(eltype(dm), Int)
	# @assert isequal(dm.na, trues(0, 0))

	# dm = DataArray()
	# @assert isequal(eltype(dm), Float64)
	# @assert isequal(dm.na, trues(0, 0))

	@assert isequal(datazeros(2, 2), DataArray(zeros(2, 2)))
	@assert isequal(datazeros(Int, 2, 2), DataArray(zeros(Int, 2, 2)))

	@assert isequal(dataones(2, 2), DataArray(ones(2, 2)))
	@assert isequal(dataones(Int, 2, 2), DataArray(ones(Int, 2, 2)))

	@assert isequal(datafalses(2, 2), DataArray(falses(2, 2)))
	@assert isequal(datatrues(2, 2), DataArray(trues(2, 2)))

	@assert isequal(dataeye(3, 2), DataArray(eye(3, 2)))
	@assert isequal(dataeye(2), DataArray(eye(2)))
	@assert isequal(datadiagm(Float64[pi, pi]), DataArray(diagm(Float64[pi, pi])))
end
