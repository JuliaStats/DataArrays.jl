@testset "Conversions" begin
    @test isequal(@data([1, 2, null]),
                  convert(DataArray, @pdata([1, 2, null])))

    # Test vector() and matrix() conversion tools
    dv = @data ones(5)
    @test isa(convert(Vector{Float64}, dv), Vector{Float64})
    dv[1] = null
    # Should raise errors:
    # vector(dv)
    # convert(Vector{Float64}, dv)

    dm = @data ones(3, 3)
    @test isa(convert(Matrix{Float64}, dm), Matrix{Float64})
    dm[1, 1] = null
    # Should raise errors:
    # matrix(dm)
    # convert(Matrix{Float64}, dm)

    a = DataArray(Any,2)
    convert(DataArray{Integer}, a)
    a[1] = 2
    convert(DataArray{Integer}, a)

    @test convert(DataArray, [1, null]) isa DataVector{Int}
    @test isequal(convert(DataArray, [1, null]), [1, null])
    @test convert(DataArray{Int}, [1, null]) isa DataVector{Int}
    @test isequal(convert(DataArray{Int}, [1, null]), [1, null])
    @test convert(DataArray{Any}, [1, null]) isa DataVector{Any}
    @test isequal(convert(DataArray{Any}, [1, null]), [1, null])
    @test convert(DataArray{Int, 1}, [1, null]) isa DataVector{Int}
    @test isequal(convert(DataArray{Int, 1}, [1, null]), [1, null])
    @test convert(DataArray{Any, 1}, [1, null]) isa DataVector{Any}
    @test isequal(convert(DataArray{Any, 1}, [1, null]), [1, null])

    @test convert(Array, @data [1, null]) isa Vector{Union{Int, Null}}
    @test isequal(convert(Array, @data [1, null]), [1, null])
    @test_throws MethodError convert(Array{Int}, @data [1, null])
    @test convert(Array{Union{Int, Null}}, @data [1, null]) isa Vector{Union{Int, Null}}
    @test isequal(convert(Array{Union{Int, Null}}, @data [1, null]), [1, null])
    @test convert(Array{Any}, @data [1, null]) isa Vector{Any}
    @test isequal(convert(Array{Any}, @data [1, null]), [1, null])
    @test convert(Array{Union{Int, Null}, 1}, @data [1, null]) isa Vector{Union{Int, Null}}
    @test isequal(convert(Array{Union{Int, Null}}, @data [1, null]), [1, null])
    @test convert(Array{Any, 1}, @data [1, null]) isa Vector{Any}
    @test isequal(convert(Array{Any, 1}, @data [1, null]), [1, null])
end
