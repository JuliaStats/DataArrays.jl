module TestBooleans
    using Base.Test
    using DataArrays

    @assert NA | true == true
    @assert isna(NA | false)
    @assert isna(NA | NA)
    @assert true | NA == true
    @assert isna(false | NA)

    @assert isna(NA & true)
    @assert NA & false == false
    @assert isna(NA & NA)
    @assert isna(true & NA)
    @assert false & NA == false

    @assert any((@data [1, 2, NA]) .== 1) == true
    @assert any((@data [NA, 1, 2]) .== 1) == true
    @assert isna(any((@data [1, 2, NA]) .== 3))
    @assert any((@data [1, 2, 3] ).== 4) == false

    @assert isna(all((@data [1, 1, NA]) .== 1))
    @assert isna(all((@data [NA, 1, 1]) .== 1))
    @assert all((@data [1, 1, 1]) .== 1) == true
    @assert all((@data [1, 2, 1]) .== 1) == false
end
