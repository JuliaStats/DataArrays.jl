# TODO: Finish this test file
# TODO: Pull in existing tests into this file
# TODO: Rename to TestDataArray
module TestDataArrays
	using DataArrays

	# DataArray{T, N}(d::Array{T, N}, m::BitArray{N} = falses(size(d)))
	DataArray([1, 2], falses(2))
	DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2))
	DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2))

	DataArray([1, 2], trues(2))
	DataArray(repeat([1, 2], outer = [1, 2]), trues(2, 2))
	DataArray(repeat([1, 2], outer = [1, 2, 2]), trues(2, 2, 2))

	DataArray(['a', 'b'], falses(2))
	DataArray(repeat(['a', 'b'], outer = [1, 2]), falses(2, 2))
	DataArray(repeat(['a', 'b'], outer = [1, 2, 2]), falses(2, 2, 2))

	DataArray(['a', 'b'], trues(2))
	DataArray(repeat(['a', 'b'], outer = [1, 2]), trues(2, 2))
	DataArray(repeat(['a', 'b'], outer = [1, 2, 2]), trues(2, 2, 2))

	# DataArray{T, N}(d::Array{T, N}, m::BitArray{N} = falses(size(d)))
	DataArray([1, 2])
	DataArray(repeat([1, 2], outer = [1, 2]))
	DataArray(repeat([1, 2], outer = [1, 2, 2]))

	DataArray(['a', 'b'])
	DataArray(repeat(['a', 'b'], outer = [1, 2]))
	DataArray(repeat(['a', 'b'], outer = [1, 2, 2]))

	# DataArray(d::Array, m::Array{Bool})
	DataArray([1, 2], [false, false])
	DataArray(['a', 'b'], [false, false])

	DataArray([1, 2], [true, true])
	DataArray(['a', 'b'], [true, true])

	# DataArray(d::BitArray, m::BitArray = falses(size(d)))
	DataArray(falses(2), falses(2))
	DataArray(trues(2), falses(2))

	DataArray(falses(2), trues(2))
	DataArray(trues(2), trues(2))

	# DataArray(d::BitArray, m::BitArray = falses(size(d)))
	DataArray(falses(2))
	DataArray(trues(2))

	# DataArray(d::Ranges, m::BitArray = falses(length(d)))
	DataArray(1:2, falses(2))
	DataArray(1:2, trues(2))

	# DataArray(t::Type, dims::Integer...)
	DataArray(Float64, 2)
	DataArray(Float64, 2, 2)
	DataArray(Float64, 2, 2, 2)

	DataArray(Char, 2)
	DataArray(Char, 2, 2)
	DataArray(Char, 2, 2, 2)

	# DataArray{N}(t::Type, dims::NTuple{N,Int})
	DataArray(Float64, (2, ))
	DataArray(Float64, (2, 2))
	DataArray(Float64, (2, 2, 2))

	DataArray(Char, (2, ))
	DataArray(Char, (2, 2))
	DataArray(Char, (2, 2, 2))

	# Base.copy(d::DataArray)
	copy(DataArray([1, 2], falses(2)))

	# Base.deepcopy(d::DataArray)
	deepcopy(DataArray([1, 2], falses(2)))

	# Base.copy!(dest::DataArray, src::Any)
	da = DataArray([1, 2], falses(2))
	copy!(da, [3, 4])
	da

	# function Base.similar(d::DataArray, T::Type, dims::Dims)
	similar(DataArray([1, 2], falses(2)), Float64, 2)
	similar(DataArray([1, 2], falses(2)), Float64, 2, 2)
	similar(DataArray([1, 2], falses(2)), Float64, 2, 2, 2)

	# Base.size(d::DataArray) = size(d.data)
	size(DataArray([1, 2], falses(2)))
	size(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
	size(DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))

	# Base.ndims(d::DataArray) = ndims(d.data)
	ndims(DataArray([1, 2], falses(2)))
	ndims(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
	ndims(DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))

	# Base.length(d::DataArray) = length(d.data)
	length(DataArray([1, 2], falses(2)))
	length(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
	length(DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))

	# Base.endof(d::DataArray) = endof(d.data)
	endof(DataArray([1, 2], falses(2)))
	endof(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
	endof(DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))

	# Base.eltype{T, N}(d::DataArray{T, N}) = T
	eltype(DataArray([1, 2], falses(2)))
	eltype(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
	eltype(DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))

	# Base.find(da::AbstractDataArray{Bool}) = find(array(da, false))
	find(DataArray([false, true, true, false]))

	# function array{T}(da::DataArray{T})
	# array(DataArray([1, 0, 3], [false, true, false]))
	# array(DataArray([1, 2, 3], [false, false, false]))

	# function array{T}(da::DataArray{T}, replacement::T)
	array(DataArray([1, 0, 3], [false, true, false]), -1)
	array(DataArray([1, 2, 3], [false, false, false]), -1)

	# removeNA(da::DataArray)
	removeNA(DataArray([1, 0, 3], [false, true, false]))
	removeNA(DataArray([1, 2, 3], [false, false, false]))
	# removeNA{T}(da::AbstractDataVector{T})
	# removeNA(@data([1, NA, 3]))

	# Iterators

	# GetIndex
	# Base.getindex(x::Vector, inds::AbstractDataVector{Bool})
	[1, 2, 3][DataArray([true, false, false], [false, true, false])]

	# Base.getindex(x::Vector, inds::AbstractDataArray{Bool})
	[1, 2, 3][DataArray([true, false, false], [false, true, false])]

	# Base.getindex(x::Array, inds::AbstractDataVector{Bool})
	[1 2; 3 4][DataArray([true, false, false, true], [false, true, false, true])]

	# Base.getindex(x::Array, inds::AbstractDataArray{Bool})
	[1 2; 3 4][DataArray([true, false, false, true], [false, true, false, true])]

	# Base.getindex{S, T}(x::Vector{S}, inds::AbstractDataArray{T})
	# Base.getindex(x::Array, inds::AbstractDataVector{Bool})
	[1, 2, 3, 4][DataArray([1, 2, 3, 4], [false, true, false, true])]

	# Base.getindex(x::Array, inds::AbstractDataArray{Bool})
	[1 2; 3 4][DataArray([1, 2, 3, 4], [false, true, false, true])]

	# Base.getindex{S, T}(x::Array{S}, inds::AbstractDataArray{T})
	# Base.getindex(d::DataArray, i::SingleIndex)
	# Base.getindex(d::DataArray, inds::AbstractDataVector{Bool})
	# Base.getindex(d::DataArray, inds::AbstractDataVector)
	# Base.getindex{T <: Number, N}(d::DataArray{T,N}, inds::BooleanIndex)
	# Base.getindex(d::DataArray, inds::BooleanIndex)
	# Base.getindex{T <: Number, N}(d::DataArray{T, N}, inds::MultiIndex)
	# Base.getindex(d::DataArray, inds::MultiIndex)
	# Base.getindex{T <: Number, N}(d::DataArray{T, N}, inds::BooleanIndex)
	# Base.getindex{T <: Number, N}(d::DataArray{T, N}, inds::MultiIndex)

	# Base.setindex!(da::DataArray, val::NAtype, i::SingleIndex)
	# Base.setindex!(da::DataArray, val::Any, i::SingleIndex)
	# Base.setindex!(da::DataArray{NAtype}, val::NAtype, inds::AbstractVector{Bool})
	# Base.setindex!(da::DataArray{NAtype}, val::NAtype, inds::AbstractVector)
	# Base.setindex!(da::DataArray, val::NAtype, inds::AbstractVector{Bool})
	# Base.setindex!(da::DataArray, val::NAtype, inds::AbstractVector)
	# Base.setindex!(da::AbstractDataArray, vals::AbstractVector, inds::AbstractVector{Bool})
	# Base.setindex!(da::AbstractDataArray, vals::AbstractVector, inds::AbstractVector)
	# Base.setindex!{T}(da::AbstractDataArray{T}, val::Union(Number, String, T), inds::AbstractVector{Bool})
	# Base.setindex!{T}(da::AbstractDataArray{T}, val::Union(Number, String, T), inds::AbstractVector)
	# Base.setindex!(da::AbstractDataArray, val::Any, inds::AbstractVector{Bool})
	# Base.setindex!{T}(da::AbstractDataArray{T}, val::Any, inds::AbstractVector)

	# isna(a::AbstractArray)
	isna([1, 2])
	isna(repeat([1, 2], outer = [1, 2]))
	isna(repeat([1, 2], outer = [1, 2, 2]))

	# isna(da::DataArray)
	isna(DataArray([1, 2], falses(2)))
	isna(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
	isna(DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))

	# Base.isnan(da::DataArray)
	isnan(DataArray([1, 2], falses(2)))
	isnan(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
	isnan(DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))

	# Base.isfinite(da::DataArray)
	isfinite(DataArray([1, 2], falses(2)))
	isfinite(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
	isfinite(DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))

	# anyna(a::AbstractArray)
	anyna([1, 2])
	anyna(repeat([1, 2], outer = [1, 2]))
	anyna(repeat([1, 2], outer = [1, 2, 2]))

	# anyna(d::AbstractDataArray)
	anyna(DataArray([1, 2], falses(2)))
	anyna(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
	anyna(DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))

	# allna(a::AbstractArray)
	allna([1, 2])
	allna(repeat([1, 2], outer = [1, 2]))
	allna(repeat([1, 2], outer = [1, 2, 2]))

	# allna(d::AbstractDataArray)
	allna(DataArray([1, 2], falses(2)))
	allna(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
	allna(DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))

	# Base.start(x::AbstractDataArray)
	start(DataArray([1, 2], falses(2)))
	start(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
	start(DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))

	# Base.next(x::AbstractDataArray, state::Integer)
	next(DataArray([1, 2], falses(2)), 1)
	next(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)), 1)
	next(DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)), 1)

	# Base.done(x::AbstractDataArray, state::Integer)
	done(DataArray([1, 2], falses(2)), 1)
	done(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)), 1)
	done(DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)), 1)

	# Base.convert{N}(::Type{BitArray{N}}, d::DataArray{BitArray{N}, N})

	# Base.convert{T, N}(::Type{BitArray{N}}, d::DataArray{T, N})
	# convert(BitArray, DataArray([1, 2], falses(2)))
	# convert(BitArray, DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
	# convert(BitArray, DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))

	# Base.convert{S, T, N}(::Type{Array{S, N}}, x::DataArray{T, N})
	convert(Array{Float64}, DataArray([1, 2], falses(2)))
	convert(Array{Float64}, DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
	convert(Array{Float64}, DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))

	# Base.convert{T, N}(::Type{Array{T, N}}, x::DataArray{T, N})
	convert(Array, DataArray([1, 2], falses(2)))
	convert(Array, DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
	convert(Array, DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))

	# Base.convert{S, T, N}(::Type{DataArray{S, N}}, x::Array{T, N})
	convert(DataArray{Float64}, [1, 2])
	convert(DataArray{Float64}, repeat([1, 2], outer = [1, 2]))
	convert(DataArray{Float64}, repeat([1, 2], outer = [1, 2, 2]))

	# Base.convert{T, N}(::Type{DataArray}, x::Array{T, N})
	convert(DataArray, [1, 2])
	convert(DataArray, repeat([1, 2], outer = [1, 2]))
	convert(DataArray, repeat([1, 2], outer = [1, 2, 2]))

	# Base.convert{S, T, N}(::Type{DataArray{S, N}}, x::DataArray{T, N})
	convert(DataArray{Float64}, DataArray([1, 2], falses(2)))
	convert(DataArray{Float64}, DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
	convert(DataArray{Float64}, DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))

	# Base.convert{T, N}(::Type{DataArray}, x::DataArray{T, N})
	convert(DataArray, DataArray([1, 2], falses(2)))
	convert(DataArray, DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
	convert(DataArray, DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))

	# int(da::DataArray)
	int(DataArray([1, 2], falses(2)))
	int(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
	int(DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))

	# float(da::DataArray)
	float(DataArray([1, 2], falses(2)))
	float(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
	float(DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))

	# bool(da::DataArray)
	bool(DataArray([1, 0], falses(2)))
	bool(DataArray(repeat([1, 0], outer = [1, 2]), falses(2, 2)))
	bool(DataArray(repeat([1, 0], outer = [1, 2, 2]), falses(2, 2, 2)))

	# Base.hash(a::AbstractDataArray)
	hash(DataArray([1, 2], falses(2)))
	hash(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
	hash(DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))
end
