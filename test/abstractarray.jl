module TestAbstractArray
	using Base.Test
	using DataArrays

	unsorted_dv = @data [2, 1, NA]

	# TODO: Make this work
	# tiedrank(dv)

	@assert first(unsorted_dv) == 2
	@assert isna(last(unsorted_dv))
end
