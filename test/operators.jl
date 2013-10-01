module DataArraysOperators
	using Base.Test
	using DataArrays, Stats

	macro test_da_pda(da, code)
	    esc(quote
	        let $da = copy($da)
	            $code
	        end
	        let $da = PooledDataArray($da)
	            $code
	        end
	    end)
	end

	# All unary operators return NA when evaluating NA
    for f in map(eval, DataArrays.unary_operators)
        @assert isna(f(NA))
    end

    # All elementary functions return NA when evaluating NA
    for f in map(eval, DataArrays.elementary_functions)
        @assert isna(f(NA))
    end

    # All comparison operators return NA when comparing NA with NA
    # All comparison operators return NA when comparing scalars with NA
    # All comparison operators return NA when comparing NA with scalars
    for f in map(eval, DataArrays.comparison_operators)
        @assert isna(f(NA, NA))
        @assert isna(f(NA, 1))
        @assert isna(f(1, NA))
    end

    # All arithmetic operators return NA when operating on two NA's
    # All arithmetic operators return NA when operating on a scalar and an NA
    # All arithmetic operators return NA when operating on an NA and a scalar
    for f in map(eval, DataArrays.arithmetic_operators)
        @assert isna(f(NA, NA))
        @assert isna(f(1, NA))
        @assert isna(f(NA, 1))
    end

    # All bit operators return NA when operating on two NA's
    # All bit operators return NA when operating on a scalar and an NA
    # All bit operators return NA when operating on an NA and a scalar
    for f in map(eval, DataArrays.bit_operators)
        @assert isna(f(NA, NA))
        @assert isna(f(1, NA))
        @assert isna(f(NA, 1))
    end

    # Unary operators on DataVector's should be equivalent to elementwise
    # application of those same operators
    N = 5
    dv = dataones(N)
    @test_da_pda dv begin
        for f in map(eval, DataArrays.numeric_unary_operators)
            for i in 1:length(dv)
                @assert f(dv)[i] == f(dv[i])
            end
        end
    end
    dv = datatrues(N)
    @test_da_pda dv begin
        for f in map(eval, DataArrays.logical_unary_operators)
            for i in 1:length(dv)
                @assert f(dv)[i] == f(dv[i])
            end
        end
    end

    # Elementary functions on DataVector's
    N = 5
    dv = dataones(N)
    @test_da_pda dv begin
        for f in map(eval, DataArrays.elementary_functions)
            for i in 1:length(dv)
                @assert f(dv)[i] == f(dv[i])
            end
        end
    end

    # Broadcasting operations between NA's and DataVector's
    N = 5
    dv = dataones(N)
    @test_da_pda dv begin
        for f in map(eval, DataArrays.arithmetic_operators)
            for i in 1:length(dv)
                @assert isna(f(dv, NA)[i])
                @assert isna(f(NA, dv)[i])
            end
        end
    end

    # Broadcasting operations between scalars and DataVector's
    N = 5
    dv = dataones(N)
    @test_da_pda dv begin
        for f in map(eval, DataArrays.arithmetic_operators)
            for i in 1:length(dv)
                @assert f(dv, 1)[i] == f(dv[i], 1)
                @assert f(1, dv)[i] == f(1, dv[i])
            end
        end
    end
    dv = DataVector[false, true, false, true, false]
    for f in map(eval, DataArrays.bit_operators)
        for i in 1:length(dv)
            @assert f(dv, true)[i] == f(dv[i], true)
            @assert f(true, dv)[i] == f(true, dv[i])
        end
    end

    # Binary operations on (DataVector, Vector) or (Vector, DataVector)
    N = 5
    v = ones(N)
    dv = dataones(N)
    dv[1] = NA
    @test_da_pda dv begin
        for f in map(eval, DataArrays.array_arithmetic_operators)
            for i in 1:length(dv)
                @assert isna(f(v, dv)[i]) && isna(dv[i]) ||
                        f(v, dv)[i] == f(v[i], dv[i])
                @assert isna(f(dv, v)[i]) && isna(dv[i]) ||
                        f(dv, v)[i] == f(dv[i], v[i])
            end
        end
    end

    # Binary operations on pairs of DataVector's
    N = 5
    dv = dataones(N)
    dv[1] = NA
    @test_da_pda dv begin
        for f in map(eval, DataArrays.array_arithmetic_operators)
            for i in 1:length(dv)
                @assert isna(f(dv, dv)[i]) && isna(dv[i]) ||
                        f(dv, dv)[i] == f(dv[i], dv[i])
            end
        end
    end

    # Unary vector operators on DataVector's
    N = 5
    dv = dataones(5)
    for f in map(eval, DataArrays.unary_vector_operators)
        @assert isequal(f(dv), f(dv.data))
    end
    dv[1] = NA
    for f in map(eval, DataArrays.unary_vector_operators)
        @assert isna(f(dv))
    end

    # Pairwise vector operators on DataVector's
    N = 5
    dv = DataVector[911, 269, 835.0, 448, 772]
    for f in map(eval, DataArrays.pairwise_vector_operators)
        @assert isequal(f(dv), f(dv.data))
    end
    dv = DataVector[NA, 269, 835.0, 448, 772]
    for f in map(eval, DataArrays.pairwise_vector_operators)
        v = f(dv)
        @assert isna(v[1])
        @assert isequal(v[2:4], f(dv.data)[2:4])
    end
    dv = DataVector[911, NA, 835.0, 448, 772]
    for f in map(eval, DataArrays.pairwise_vector_operators)
        v = f(dv)
        @assert isna(v[1])
        @assert isna(v[2])
        @assert isequal(v[3:4], f(dv.data)[3:4])
    end
    dv = DataVector[911, 269, 835.0, 448, NA]
    for f in map(eval, DataArrays.pairwise_vector_operators)
        v = f(dv)
        @assert isna(v[4])
        @assert isequal(v[1:3], f(dv.data)[1:3])
    end

    # Cumulative vector operators on DataVector's
    N = 5
    dv = dataones(N)
    for f in map(eval, DataArrays.cumulative_vector_operators)
        for i in 1:length(dv)
            @assert f(dv)[i] == f(dv.data)[i]
        end
    end
    dv[4] = NA
    for f in map(eval, DataArrays.cumulative_vector_operators)
        for i in 1:3
            @assert f(dv)[i] == f(dv.data)[i]
        end
        for i in 4:N
            @assert isna(f(dv)[i])
        end
    end

    # FFT's on DataVector's
    N = 5
    dv = dataones(5)
    for f in map(eval, DataArrays.ffts)
        @assert f(dv) == f(dv.data)
    end
    dv[1] = NA
    for f in map(eval, DataArrays.ffts)
        @assert isna(f(dv))
    end

    # Binary vector operators on DataVector's
    N = 5
    dv = dataones(5)
    for f in map(eval, DataArrays.binary_vector_operators)
        @assert f(dv, dv) == f(dv.data, dv.data) ||
                (isnan(f(dv, dv)) && isnan(f(dv.data, dv.data)))
    end
    dv[1] = NA
    for f in map(eval, DataArrays.binary_vector_operators)
        @assert isna(f(dv, dv))
    end

    # Boolean operators on DataVector's
    N = 5
    @assert any(datafalses(N)) == false
    @assert any(datatrues(N)) == true
    @assert all(datafalses(N)) == false
    @assert all(datatrues(N)) == true
    @assert any(PooledDataArray(datafalses(N))) == false
    @assert any(PooledDataArray(datatrues(N))) == true
    @assert all(PooledDataArray(datafalses(N))) == false
    @assert all(PooledDataArray(datatrues(N))) == true

    dv = datafalses(N)
    dv[3] = true
    @test_da_pda dv begin
        @assert any(dv) == true
        @assert all(dv) == false
    end

    dv = datafalses(N)
    dv[2] = NA
    dv[3] = true
    @test_da_pda dv begin
        @assert any(dv) == true
        @assert all(dv) == false
    end

    dv = datafalses(N)
    dv[2] = NA
    @test_da_pda dv begin
        @assert isna(any(dv))
        @assert all(dv) == false
    end

    dv = datafalses(1)
    dv[1] = NA
    @test_da_pda dv begin
        @assert isna(any(dv))
        @assert isna(all(dv))
    end

    #
    # Equality tests
    #

    dv = DataVector[1, NA]
    alt_dv = DataVector[2, NA]
    pdv = PooledDataArray(DataVector[1, NA])
    alt_pdv = PooledDataArray(DataVector[2, NA])

    @assert isna(NA == NA)
    @assert isna(NA != NA)

    # @assert isna(dv == dv) # SHOULD RAISE ERROR
    # @assert isna(dv != dv) # SHOULD RAISE ERROR

    @assert isequal(dv, dv)
    @assert isequal(pdv, pdv)

    @assert !isequal(dv, alt_dv)
    @assert !isequal(pdv, alt_pdv)

    @assert isequal(DataVector[1, NA] .== DataVector[1, NA], DataVector[true, NA])
    @assert isequal(PooledDataVector[1, NA] .== PooledDataVector[1, NA], DataVector[true, NA])

    @assert all(isna(NA .== dataones(5)))
    @assert all(isna(dataones(5) .== NA))
    @assert all(isna(NA .== PooledDataArray(dataones(5))))
    @assert all(isna(PooledDataArray(dataones(5)) .== NA))

    # Run length encoding
    dv = dataones(5)
    dv[3] = NA

    v, l = DataArrays.rle(dv)
    @assert isequal(v, DataVector[1.0, NA, 1.0])
    @assert (l == [2, 1, 2])

    rdv = DataArrays.inverse_rle(v, l)
    @assert isequal(dv, rdv)
end
