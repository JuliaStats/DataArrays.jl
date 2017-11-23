# TODO: Finish this test file
# TODO: Pull in existing tests into this file
# TODO: Rename to TestDataArray
module TestDataArrays
    using DataArrays, Base.Test

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
    convert(DataArray, falses(2))
    convert(DataArray, trues(2))

    # DataArray(d::BitArray, m::BitArray = falses(size(d)))
    convert(DataArray, falses(2))
    convert(DataArray, trues(2))

    # DataArray(d::Ranges, m::BitArray = falses(length(d)))
    convert(DataArray, 1:2)
    convert(DataArray, 1:2)

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
    convert(Vector, DataArray([1, 0, 3], [false, true, false]), -1)
    convert(Vector, DataArray([1, 2, 3], [false, false, false]), -1)

    # skipmissing(da::DataArray)
    skipmissing(DataArray([1, 0, 3], [false, true, false]))
    skipmissing(DataArray([1, 2, 3], [false, false, false]))

    # Iterators

    # GetIndex
    # Base.getindex(x::Vector, inds::AbstractDataVector{Bool})
    dinds = @data([true, false, false])
    [1, 2, 3][dinds]

    # Base.getindex(x::Vector, inds::AbstractDataArray{Bool})
    dinds = @data([true, false, false])
    [1, 2, 3][dinds]

    # Base.getindex{S, T}(x::Vector{S}, inds::AbstractDataArray{T})
    dinds = @data([1, 2, missing])
    @test_throws MissingException [1.0, 2.0, 3.0, 4.0][dinds]

    # Base.getindex{S, T}(x::Array{S}, inds::AbstractDataArray{T})
    dinds = @data([1, 2, missing])
    @test_throws MissingException [1.0 2.0; 3.0 4.0][dinds]

    # Base.getindex(d::DataArray, i::SingleIndex)
    da = @data([1, 2, missing, 4])
    da[1]
    da[3]
    # da[1.0] deprecated
    # da[3.0] deprecated

    # Base.getindex(d::DataArray, inds::AbstractDataVector{Bool})
    da = @data([1, 2, missing, 4])
    dinds = @data([true, false, false, missing])
    @test_throws MissingException da[dinds]

    # Base.getindex(d::DataArray, inds::AbstractDataVector)
    da = @data([1, 2, missing, 4])
    dinds = @data([1, 2, missing, 2])
    @test_throws MissingException da[dinds]

    # Base.getindex{T <: Number, N}(d::DataArray{T,N}, inds::BooleanIndex)
    # da = @data([1, 2, missing, 4])
    # inds = [1, 2, missing, 2]
    # da[inds]

    # Base.getindex(d::DataArray, inds::BooleanIndex)
    # da = @data([1.0, 2.0, missing, 4.0])
    # inds = [1, 2, missing, 2]
    # da[inds]

    # Base.getindex{T <: Number, N}(d::DataArray{T, N}, inds::MultiIndex)
    da = @data([1.0, 2.0, missing, 4.0])
    inds = [1, 2, 2]
    da[inds]

    # Base.getindex(d::DataArray, inds::MultiIndex)
    da = @data([1.0, 2.0, missing, 4.0])
    inds = [1, 2, 2]
    da[inds]

    # Base.getindex{T <: Number, N}(d::DataArray{T, N}, inds::BooleanIndex)
    da = @data([1.0, 2.0, missing, 4.0])
    inds = [true, true, false, false]
    da[inds]

    # Base.getindex{T <: Number, N}(d::DataArray{T, N}, inds::MultiIndex)
    da = @data([1.0, 2.0, missing, 4.0])
    inds = [1, 2, 2]
    da[inds]

    # Base.setindex!(da::DataArray, val::missingtype, i::SingleIndex)
    da = @data([1.0, 2.0, missing, 4.0])
    da[1] = missing

    # Base.setindex!(da::DataArray, val::Any, i::SingleIndex)
    da = @data([1.0, 2.0, missing, 4.0])
    da[1] = 3.0

    # Base.setindex!(da::DataArray{Missing}, val::Missing, inds::AbstractVector{Bool})
    # da = DataArray([missing, missing], falses(2))
    # da[[true, false]] = missing

    # Base.setindex!(da::DataArray{Missing}, val::Missing, inds::AbstractVector)
    # da = DataArray([missing, missing], falses(2))
    # da[[1, 2]] = missing

    # Base.setindex!(da::DataArray, val::Missing, inds::AbstractVector{Bool})
    da = @data([1, 2])
    da[[true, false]] = missing

    # Base.setindex!(da::DataArray, val::Missing, inds::AbstractVector)
    da = @data([1, 2])
    da[[1, 2]] = missing

    # Base.setindex!(da::AbstractDataArray, vals::AbstractVector, inds::AbstractVector{Bool})
    da = @data([1, 2])
    da[[true, false]] = [3]

    # Base.setindex!(da::AbstractDataArray, vals::AbstractVector, inds::AbstractVector)
    da = @data([1, 2])
    da[[1, 2]] = [3, 4]

    # Base.setindex!{T}(da::AbstractDataArray{T}, val::Union(Number, AbstractString, T), inds::AbstractVector{Bool})
    da = @data([1, 2])
    da[[true, false]] = 5

    # Base.setindex!{T}(da::AbstractDataArray{T}, val::Union(Number, AbstractString, T), inds::AbstractVector)
    da = @data([1, 2])
    da[[1, 2]] = 5

    # Base.setindex!(da::AbstractDataArray, val::Any, inds::AbstractVector{Bool})
    da = @data([1, 2])
    da[[true, false]] = 5.0

    # Base.setindex!{T}(da::AbstractDataArray{T}, val::Any, inds::AbstractVector)
    da = @data([1, 2])
    da[[1, 2]] = 5

    # ismissing(a::AbstractArray)
    ismissing.([1, 2])
    ismissing.(repeat([1, 2], outer = [1, 2]))
    ismissing.(repeat([1, 2], outer = [1, 2, 2]))

    # ismissing(da::DataArray)
    ismissing.(DataArray([1, 2], falses(2)))
    ismissing.(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
    ismissing.(DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))

    # Base.isnan(da::DataArray)
    isnan.(DataArray([1, 2], falses(2)))
    isnan.(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
    isnan.(DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))

    # Base.isfinite(da::DataArray)
    isfinite(DataArray([1, 2], falses(2)))
    isfinite(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
    isfinite(DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))

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

    # round(Int, da::DataArray)
    round.(Int, DataArray([1, 2], falses(2)))
    round.(Int, DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
    round.(Int, DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))

    # float(da::DataArray)
    float.(DataArray([1, 2], falses(2)))
    float.(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
    float.(DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))

    # map(Bool, da::DataArray)
    map(Bool, DataArray([1, 0], falses(2)))
    map(Bool, DataArray(repeat([1, 0], outer = [1, 2]), falses(2, 2)))
    map(Bool, DataArray(repeat([1, 0], outer = [1, 2, 2]), falses(2, 2, 2)))

    # Base.hash(a::AbstractDataArray)
    hash(DataArray([1, 2], falses(2)))
    hash(DataArray(repeat([1, 2], outer = [1, 2]), falses(2, 2)))
    hash(DataArray(repeat([1, 2], outer = [1, 2, 2]), falses(2, 2, 2)))
end
