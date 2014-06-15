module TestBroadcast
using DataArrays, Base.Test

# Based on broadcast tests from base
as_dataarray(x) = convert(DataArray, x)
as_dataarray_bigfloat(x) = convert(DataArray{BigFloat}, x)
as_pda(x) = convert(PooledDataArray, x)
as_pda_bigfloat(x) = convert(PooledDataArray{BigFloat}, x)

bittest(f::Function, ewf::Function, a...) = (@test ewf(a...) ==
        invoke(broadcast, tuple(Function, ntuple(length(a), x->AbstractArray)...), f, a...))
n1 = 21
n2 = 32
n3 = 17
rb = 1:5

@test broadcast!(+, DataArray(Int, 2, 2), eye(2), [1, 4]) == [2 1; 4 5]
@test broadcast!(+, DataArray(Int, 2, 2), eye(2), [1  4]) == [2 4; 1 5]
@test broadcast!(+, DataArray(Int, 2, 2), [1  0], [1, 4]) == [2 1; 5 4]
@test broadcast!(+, DataArray(Int, 2, 2), [1, 0], [1  4]) == [2 5; 1 4]
@test broadcast!(+, DataArray(Int, 2), [1, 0], [1, 4]) == [2, 4]
@test broadcast!(+, DataArray(Int, 2), [1, 0], 2) == [3, 2]
for arr in (identity, as_dataarray, as_pda, as_dataarray_bigfloat, as_pda_bigfloat)
    @test broadcast(+, arr(eye(2)), arr([1, 4])) == [2 1; 4 5]
    @test broadcast(+, arr(eye(2)), arr([1  4])) == [2 4; 1 5]
    @test broadcast(+, arr([1  0]), arr([1, 4])) == [2 1; 5 4]
    @test broadcast(+, arr([1, 0]), arr([1  4])) == [2 5; 1 4]
    @test broadcast(+, arr([1, 0]), arr([1, 4])) == [2, 4]
    @test broadcast(+, arr([1, 0]), 2) == [3, 2]

    @test isequal(broadcast(+, arr(eye(2)), arr(@data [NA, 4])), @data [NA NA; 4 5])
    @test isequal(broadcast(+, arr(eye(2)), arr(@data [NA  4])), @data [NA 4; NA 5])
    @test isequal(broadcast(+, arr(@data [1  NA]), arr([1, 4])), @data [2 NA; 5 NA])
    @test isequal(broadcast(+, arr(@data [1, NA]), arr([1  4])), @data [2 5; NA NA])
    @test isequal(broadcast(+, arr(@data [1, NA]), arr([1, 4])), @data [2, NA])

    @test @inferred(arr(eye(2)) .+ arr([1, 4])) == arr([2 1; 4 5])
    @test arr(eye(2)) .+ arr([1  4]) == arr([2 4; 1 5])
    @test arr([1  0]) .+ arr([1, 4]) == arr([2 1; 5 4])
    @test arr([1, 0]) .+ arr([1  4]) == arr([2 5; 1 4])
    @test arr([1, 0]) .+ arr([1, 4]) == arr([2, 4])
    @test arr([1]) .+ arr([]) == arr([])

    A = arr(eye(2)); @test broadcast!(+, A, A, arr([1, 4])) == arr([2 1; 4 5])
    A = arr(eye(2)); @test broadcast!(+, A, A, arr([1  4])) == arr([2 4; 1 5])
    A = arr([1  0]); @test_throws ErrorException broadcast!(+, A, A, arr([1, 4]))
    A = arr([1  0]); @test broadcast!(+, A, A, arr([1  4])) == arr([2 4])
    A = arr([1  0]); @test broadcast!(+, A, A, 2) == arr([3 2])

    @test arr([ 1    2])   .* arr([3,   4])   == [ 3 6; 4 8]
    @test arr([24.0 12.0]) ./ arr([2.0, 3.0]) == [12 6; 8 4]
    @test arr([1 2]) ./ arr([8, 4]) == [1/8 2/8; 1/4 2/4]
    @test arr([1 2]) .\ arr([3, 4]) == [3 1.5; 4 2]
    @test arr([3 4]) .^ arr([1, 2]) == [3 4; 9 16]
    @test arr(bitpack([true false])) .* arr(bitpack([true, true])) == [true false; true false]
    @test arr(bitpack([true false])) .^ arr(bitpack([false, true])) == [true true; true false]
    @test arr(bitpack([true false])) .^ arr([0, 3]) == [true true; true false]

    # NOT YET IMPLEMENTED
    # M = arr([11 12; 21 22])
    # @test broadcast_getindex(M, eye(Int, 2).+1,arr([1, 2])) == [21 11; 12 22]
    # @test_throws BoundsError broadcast_getindex(M, eye(Int, 2).+1,arr([1, -1]))
    # @test_throws BoundsError broadcast_getindex(M, eye(Int, 2).+1,arr([1, 2]), [2])
    # @test broadcast_getindex(M, eye(Int, 2).+1,arr([2, 1]), [1]) == [22 12; 11 21]

    # A = arr(zeros(2,2))
    # broadcast_setindex!(A, arr([21 11; 12 22]), eye(Int, 2).+1,arr([1, 2]))
    # @test A == M
    # broadcast_setindex!(A, 5, [1,2], [2 2])
    # @test A == [11 5; 21 5]
    # broadcast_setindex!(A, 7, [1,2], [1 2])
    # @test A == fill(7, 2, 2)
    # A = arr(zeros(3,3))
    # broadcast_setindex!(A, 10:12, 1:3, 1:3)
    # @test A == diagm(10:12)
    # @test_throws BoundsError broadcast_setindex!(A, 7, [1,-1], [1 2])

    for (f, ewf) in (((==), (.==)),
                     ((<) , (.<) ),
                     ((!=), (.!=)),
                     ((<=), (.<=)))
        bittest(f, ewf, arr(eye(2)), arr([1, 4]))
        bittest(f, ewf, arr(eye(2)), arr([1  4]))
        bittest(f, ewf, arr([0, 1]), arr([1  4]))
        bittest(f, ewf, arr([0  1]), arr([1, 4]))
        bittest(f, ewf, arr([1, 0]), arr([1, 4]))

        # these should work once indexing is fixed
        #bittest(f, ewf, arr(rand(rb, n1, n2, n3)), arr(rand(rb, n1, n2, n3)))
        #bittest(f, ewf, arr(rand(rb,  1, n2, n3)), arr(rand(rb, n1,  1, n3)))
        #bittest(f, ewf, arr(rand(rb,  1, n2,  1)), arr(rand(rb, n1,  1, n3)))
        #bittest(f, ewf, arr(randbool(n1, n2, n3)), arr(randbool(n1, n2, n3)))
    end
end

r1 = 1:1
r2 = 1:5
ratio = @data [1,1/2,1/3,1/4,1/5]
@test r1.*r2 == [1:5]
@test r1./r2 == ratio
m = @data [1:2]'
@test m.*r2 == DataArray([1:5 2:2:10])
@test_approx_eq m./r2 [ratio 2ratio]
@test_approx_eq m./[r2] [ratio 2ratio]

@test @inferred([0,1.2].+reshape([0,-2],1,1,2)) == reshape([0 -2; 1.2 -0.8],2,1,2)
rt = Base.return_types(.+, (DataArray{Float64, 3}, DataArray{Int, 1}))
@test length(rt) == 1 && rt[1] == DataArray{Float64, 3}
rt = Base.return_types(broadcast, (Function, Array{Float64, 3}, DataArray{Int, 1}))
@test length(rt) == 1 && rt[1] == DataArray{Float64, 3}
rt = Base.return_types(broadcast!, (Function, DataArray{Float64, 3}, Array{Float64, 3}, Array{Int, 1}))
@test length(rt) == 1 && rt[1] == DataArray{Float64, 3}

# Test broadcasting of functions that do something besides propagate NA
@test isequal(broadcast(isequal, @data([NA, 1]), @data([NA 1])), @data([true false; false true]))
@test isequal(broadcast(isequal, @pdata([NA, 1]), @data([NA 1])), @data([true false; false true]))
@test isequal(broadcast(isequal, @data([NA, 1]), @pdata([NA 1])), @data([true false; false true]))
@test isequal(broadcast(isequal, @pdata([NA, 1]), @pdata([NA 1])), @pdata([true false; false true]))
@test isequal(broadcast(&, @data([NA, false]), @data([NA true false])), @data([NA NA false; false false false]))
@test isequal(broadcast(|, @data([NA, false]), @data([NA true false])), @data([NA true NA; NA true false]))

# Test map!
@test_throws ErrorException map!(+, DataArray(Float64, 2, 2), @data([1, 2]), @data([1 2]))
@test map!(+, DataArray(Float64, 2), @data([1, 2]), @data([1, 2])) == @data([2, 4])
@test isequal(map!(+, DataArray(Float64, 3), @data([1, NA, 3]), @data([NA, 2, 3])), @data([NA, NA, 6]))
@test map!(isequal, DataArray(Float64, 3), @data([1, NA, NA]), @data([1, NA, 3])) == @data([true, true, false])
end