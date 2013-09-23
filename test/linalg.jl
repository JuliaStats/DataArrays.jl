module DataArraysLinAlg
	using Base.Test
	using DataArrays

	d = dataeye(3, 3)
	d[1, 1] = NA

	svd(d)
end
