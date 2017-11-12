@testset "DataMatrix" begin
    a = @data [1.0, 2.0, 3.0]
    v_a = [1.0, 2.0, 3.0]

    b = @data eye(3, 3)
    m_b = eye(3, 3)

    #
    # Transposes
    #

    @test all(a' .== v_a')
    @test all(a'' .== v_a'')
    @test all(b' .== m_b')
    @test all(b'' .== m_b'')

    #
    # DataVector * DataMatrix
    #

    # TODO: Get indexing for b[1, :] to work
    # @test all(a * b[1, :] .== v_a * m_b[1, :])

    #
    # DataMatrix * DataVector
    #

    @test all(b * a .== m_b * v_a)
    @test all(convert(Array, b * a) .== m_b * v_a)

    #
    # DataMatrix * DataMatrix
    #

    @test all(b * b .== m_b * m_b)

    #
    # DataVector * DataMatrix w/ missings
    #

    b[1, 1] = missing
    res = a * b[1:1, :]
    @test all(ismissing.(res[:, 1]))
    @test all(.!(ismissing.(res[:, 2])))
    @test all(.!(ismissing.(res[:, 3])))
    res = a * b[2:2, :]
    @test all(.!(ismissing.(res)))

    #
    # DataMatrix w missings * DataVector
    #

    res = b * a
    @test ismissing.(res[1])
    @test .!(ismissing.(res[2]))
    @test .!(ismissing.(res[3]))

    #
    # DataMatrix * DataMatrix
    #

    res = b * b
    # 3x3 Float64 DataMatrix:
    #  missing   missing   missing
    #  missing  1.0  0.0
    #  missing  0.0  1.0
    @test ismissing.(res[1, 1])
    @test ismissing.(res[1, 2])
    @test ismissing.(res[1, 3])
    @test ismissing.(res[2, 1])
    @test .!(ismissing.(res[2, 2]))
    @test .!(ismissing.(res[2, 3]))
    @test ismissing.(res[3, 1])
    @test .!(ismissing.(res[3, 2]))
    @test .!(ismissing.(res[3, 3]))

    res = b * @data eye(3)
    # 3x3 Float64 DataMatrix:
    #   missing   missing   missing
    #  0.0  1.0  0.0
    #  0.0  0.0  1.0
    @test ismissing.(res[1, 1])
    @test ismissing.(res[1, 2])
    @test ismissing.(res[1, 3])
    @test .!(ismissing.(res[2, 1]))
    @test .!(ismissing.(res[2, 2]))
    @test .!(ismissing.(res[2, 3]))
    @test .!(ismissing.(res[3, 1]))
    @test .!(ismissing.(res[3, 2]))
    @test .!(ismissing.(res[3, 3]))

    res = (@data eye(3)) * b
    # julia> dataeye(3) * b
    # 3x3 Float64 DataMatrix:
    #  missing  0.0  0.0
    #  missing  1.0  0.0
    #  missing  0.0  1.0
    @test ismissing.(res[1, 1])
    @test .!(ismissing.(res[1, 2]))
    @test .!(ismissing.(res[1, 3]))
    @test ismissing.(res[2, 1])
    @test .!(ismissing.(res[2, 2]))
    @test .!(ismissing.(res[2, 3]))
    @test ismissing.(res[3, 1])
    @test .!(ismissing.(res[3, 2]))
    @test .!(ismissing.(res[3, 3]))

    # Test row operations
    dm = @data eye(6, 2)
    # rowmeans(dm)

    # Test column operations
    dm = @data eye(6, 2)
    # colmeans(dm)

    # Test linear algebra
    du, dd, dv = svd((@data eye(3, 3)))
    u, d, v = svd(eye(3, 3))
    @test all(du .== u)
    @test all(dd .== d)
    @test all(dv .== v)

    # Test elementary functions
    dm = -(@data eye(5, 5))
    @test all(abs(dm) .== eye(5, 5))
end
