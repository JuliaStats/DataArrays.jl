module TestDataVector
	using Base.Test
	using DataArrays

  # padNA test
	dv = DataArray([1, 2, 3, 4])
  padNA(dv, 2, 2)

	@assert length(dv) == 8
	@assert sum(removeNA(dv)) == 10
  @assert sum(replaceNA(dv, 1)) == 14
end

