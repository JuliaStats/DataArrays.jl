module TestDataVectors
    using Base.Test
    using DataArrays

    # Base.getindex(d::DataVector, i::SingleIndex, j::SingleIndex)
    da = @data([1, 2, missing, 4])
    da[1, 1]
    da[2, 1]
    # da[1, 2]

    # Base.push!{T}(dv::DataVector{T}, v::Missing)
    push!(da, missing)

    # Base.push!{S, T}(dv::DataVector{S}, v::T)
    push!(da, 6)

    # Base.pop!(dv::DataVector)
    pop!(da)

    # Base.unshift!{T}(dv::DataVector{T}, v::Missing)
    unshift!(da, missing)

    # Base.unshift!{S, T}(dv::DataVector{S}, v::T)
    unshift!(da, -1)

    # Base.shift!{T}(dv::DataVector{T})
    pop!(da)

    # Base.map(f::Function, dv::DataVector)
    map(sin, da)

    # Base.push!{T,R}(pdv::PooledDataVector{T,R}, v::Missing)
    pda = @pdata([1, 2, missing, 4])
    push!(pda, missing)

    # Base.push!{S,R,T}(pdv::PooledDataVector{S,R}, v::T)
    push!(pda, 6)

    # Base.pop!(pdv::PooledDataVector) = pdv.pool[pop!(pdv.refs)]
    pop!(pda)

    # Base.unshift!{T,R}(pdv::PooledDataVector{T,R}, v::Missing)
    unshift!(pda, missing)

    # Base.unshift!{S,R,T}(pdv::PooledDataVector{S,R}, v::T)
    unshift!(pda, 6)

    # Base.shift!(pdv::PooledDataVector) = pdv.pool[shift!(pdv.refs)]
    shift!(pda)

    # Base.reverse(x::AbstractDataVector) = x[end:-1:1]
    reverse(da)
    reverse(pda)

    # padmissing(dv::AbstractDataVector, front::Integer, back::Integer)
    padmissing(da, 5, 5)
    padmissing(pda, 5, 5)
end
