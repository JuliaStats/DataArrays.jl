module TestPadding
	using Base.Test
	using DataArrays

	dv = @data ones(3)
	@assert isequal(dv, padna(dv, 0, 0))
	@assert length(padna(dv, 2, 0)) == length(dv) + 2
	@assert length(padna(dv, 0, 2)) == length(dv) + 2
	@assert length(padna(dv, 2, 2)) == length(dv) + 4
end
