module TestLinAlg
    using Base.Test
    using DataArrays

    d = @data eye(3, 3)
    d[1, 1] = NA

    svd(d)
end
