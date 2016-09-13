module TestConversions
    using Base.Test
    using DataArrays

    @assert isequal(@data([1, 2, NA]),
                    convert(DataArray, @pdata([1, 2, NA])))

    # Test vector() and matrix() conversion tools
    dv = @data ones(5)
    @assert isa(convert(Vector{Float64}, dv), Vector{Float64})
    dv[1] = NA
    # Should raise errors:
    # vector(dv)
    # convert(Vector{Float64}, dv)

    dm = @data ones(3, 3)
    @assert isa(convert(Matrix{Float64}, dm), Matrix{Float64})
    dm[1, 1] = NA
    # Should raise errors:
    # matrix(dm)
    # convert(Matrix{Float64}, dm)

    a = DataArray(Any,2)
    convert(DataArray{Integer}, a)
    a[1] = 2
    convert(DataArray{Integer}, a)
end
