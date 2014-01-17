module TestExtras
	using Base.Test
	using DataArrays
	using StatsBase
	
	##########
	## countmap
	##########

	d = @data [NA,3,3]
	w = weights([1.1,2.2,3.3])
	cm = Dict{Union(Int, NAtype), Int}(@data([NA,3]), [1,2])
	cmw = Dict{Union(Int, NAtype), Real}(@data([NA,3]), [1.1,5.5])
	@assert isequal(countmap(d), cm)
	@assert isequal(countmap(d, w), cmw)

	##########
	## cut
	##########

	@assert isequal(cut([2, 3, 5], [1, 3, 6]), PooledDataArray(["(1,3]", "(1,3]", "(3,6]"]))
	@assert isequal(cut([2, 3, 5], [3, 6]), PooledDataArray(["[2,3]", "[2,3]", "(3,6]"]))
	@assert isequal(cut([2, 3, 5, 6], [3, 6]), PooledDataArray(["[2,3]", "[2,3]", "(3,6]", "(3,6]"]))
	@assert isequal(cut([1, 2, 4], [1, 3, 6]), PooledDataArray(["[1,3]", "[1,3]", "(3,6]"]))
	@assert isequal(cut([1, 2, 4], [3, 6]), PooledDataArray(["[1,3]", "[1,3]", "(3,6]"]))
	@assert isequal(cut([1, 2, 4], [3]), PooledDataArray(["[1,3]", "[1,3]", "(3,4]"]))
	@assert isequal(cut([1, 5, 7], [3, 6]), PooledDataArray(["[1,3]", "(3,6]", "(6,7]"]))

	ages = [20, 22, 25, 27, 21, 23, 37, 31, 61, 45, 41, 32]
	bins = [18, 25, 35, 60, 100]
	cats = cut(ages, bins)
	pdv = PooledDataArray(["(18,25]", "(18,25]", "(18,25]",
	                       "(25,35]", "(18,25]", "(18,25]",
	                       "(35,60]", "(25,35]", "(60,100]",
	                       "(35,60]", "(35,60]", "(25,35]"])
	@assert isequal(cats, pdv)

	##########
	## rep
	##########

	@assert rep(3, 2) == [3,3]
	@assert rep([3,4], 2) == [3,4,3,4]
	@assert rep([3,4], 1, 2) == [3,3,4,4]
	@assert rep([3,4], each = 2) == [3,3,4,4]
	@assert rep([3,4], times = 2) == [3,4,3,4]
	@assert rep([3,4], [2,3]) == [3,3,4,4,4]
	@assert isequal(rep((@data [NA,3,4]), 2), (@data [NA,3,4,NA,3,4]))
	@assert isequal(rep((@data [NA,3,4]), [2,1,2]), (@data [NA,NA,3,4,4]))
	@assert isequal(rep((@data [NA,3,4]), [2,1,0]), (@data [NA,NA,3]))
end
