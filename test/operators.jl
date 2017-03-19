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

@testset "Operators" begin
    const bit_operators = [&, |, ⊻]

    const arithmetic_operators = [+, -, *, /, Base.div, Base.mod, Base.fld, Base.rem]

    const comparison_operators = [==, !=, >, >=, <, <=]

    const elementary_functions = [abs,
                                  abs2,
                                  sign,
                                  acos,
                                  acosh,
                                  asin,
                                  asinh,
                                  atan,
                                  atanh,
                                  sin,
                                  sinh,
                                  conj,
                                  cos,
                                  cosh,
                                  tan,
                                  tanh,
                                  ceil,
                                  floor,
                                  round,
                                  trunc,
                                  exp,
                                  exp2,
                                  expm1,
                                  log,
                                  log10,
                                  log1p,
                                  log2,
                                  exponent,
                                  sqrt,
                                  gamma,
                                  lgamma]

    # All unary operators return NA when evaluating NA
    for f in [+, -]
        @test isna(f(NA))
    end

    # All elementary functions return NA when evaluating NA
    for f in elementary_functions
        @test isna(f(NA))
    end

    # All comparison operators return NA when comparing NA with NA
    # All comparison operators return NA when comparing scalars with NA
    # All comparison operators return NA when comparing NA with scalars
    for f in comparison_operators
        @test isna(f(NA, NA))
        @test isna(f(NA, 1))
        @test isna(f(1, NA))
    end

    # All arithmetic operators return NA when operating on two NA's
    # All arithmetic operators return NA when operating on a scalar and an NA
    # All arithmetic operators return NA when operating on an NA and a scalar
    for f in arithmetic_operators
        @test isna(f(NA, NA))
        @test isna(f(1, NA))
        @test isna(f(NA, 1))
    end

    # All bit operators return NA when operating on two NA's
    # All bit operators return NA when operating on a scalar and an NA
    # All bit operators return NA when operating on an NA and a scalar
    for f in bit_operators
        @test isna(f(NA, NA))
        @test isna(f(1, NA))
        @test isna(f(NA, 1))
    end

    # Unary operators on DataVector's should be equivalent to elementwise
    # application of those same operators
    dv = @data ones(5)
    @test_da_pda dv begin
        for f in [+,-]
            for i in 1:length(dv)
                @test f(dv)[i] == f(dv[i])
            end
        end
    end
    dv = convert(DataArray, trues(5))
    @test_da_pda dv begin
        for f in [!]
            for i in 1:length(dv)
                @test f(dv)[i] == f(dv[i])
            end
        end
    end

    # transpose and ctranpose on DataVectors and DataMatrices
    for (da, dat) in ((@data([5, 2, 3, 4]), @data([5 2 3 4])),
                      (@data([1.0+1.0im, 5.0, 3.0+3.0im, 4.0-4.0im]), @data([1.0+1.0im 5.0 3.0+3.0im 4.0-4.0im])),
                      (@data([1 2; 5 4]), @data([1 5; 2 4])),
                      (@data([1.0+1.0im 2.0-2.0im; 3.0+3.0im 5.0]), @data([1.0+1.0im 3.0+3.0im; 2.0-2.0im 5.0])))
        # Test for both bits and non-bits types
        for da in (da, convert(DataArray{Number}, da))
            let da = copy(da), dat = copy(dat)
                # No NA
                @test isequal(da.', dat)
                @test isequal(da', conj(dat))

                # With NA
                # XXX we should fix indexing so that this isn't necessary
                for i = 1:length(da)
                    da[i] == 5 && (da[i] = NA)
                    dat[i] == 5 && (dat[i] = NA)
                end

                # Make sure that NAs are undefined in the non-bits array
                da = conj(conj(da))
                @test isequal(da.', dat)
                @test isequal(da', conj(dat))
            end
        end
    end

    # Elementary functions on DataVector's
    dv = convert(DataArray, ones(5))
    @test_da_pda dv begin
        for f in elementary_functions
            for i in 1:length(dv)
                @test f.(dv)[i] == f(dv[i])
            end
        end
    end

    # Broadcasting operations between NA's and DataVector's
    dv = convert(DataArray, ones(5))
    @test_da_pda dv begin
        for f in [+, *, Base.div, Base.mod, Base.fld, Base.rem]
            for i in 1:length(dv)
                @test isna(f(dv, NA)[i])
                @test isna(f(NA, dv)[i])
                @test f(dv, 1)[i] == f(dv[i], 1)
                @test f(1, dv)[i] == f(1, dv[i])
            end
        end
        for f in arithmetic_operators
            for i in 1:length(dv)
                @test isna(f.(dv, NA)[i])
                @test isna(f.(NA, dv)[i])
                @test f.(dv, 1)[i] == f(dv[i], 1)
                @test f.(1, dv)[i] == f(1, dv[i])
            end
        end
    end

    @test_da_pda dv begin
        for i in 1:length(dv)
            @test isna((dv / NA)[i])
            @test (dv / 1)[i] == dv[i] / 1
        end
    end

    dv = @data([false, true, false, true, false])
    for f in bit_operators
        for i in 1:length(dv)
            @test f.(dv, true)[i] == f.(dv[i], true)
            @test f.(true, dv)[i] == f.(true, dv[i])
        end
    end

    # Binary operations on (DataVector, Vector) or (Vector, DataVector)
    v = ones(5)
    dv = convert(DataArray, ones(5))
    dv[1] = NA
    bv = [true, false, false, true, true]
    bbv = BitArray([true, false, false, true, true])
    bdv = @data [false, true, false, false, true]
    @test_da_pda dv begin
        for f in [+, -, *, ^]
            for i in 1:length(dv)
                @test isna(f.(v, dv)[i]) && isna(dv[i]) || f.(v, dv)[i] == f(v[i], dv[i])
                @test isna(f.(dv, v)[i]) && isna(dv[i]) || f.(dv, v)[i] == f(dv[i], v[i])
            end
        end
        for f in bit_operators
            for i in 1:length(bdv)
                @test f.(bv, bdv)[i]  == f.(bv[i], bdv[i])
                @test f.(bdv, bv)[i]  == f.(bdv[i], bv[i])
                @test f.(bbv, bdv)[i] == f.(bbv[i], bdv[i])
                @test f.(bdv, bbv)[i] == f.(bdv[i], bbv[i])
            end
        end
    end

    # Binary operations on pairs of DataVector's
    dv = convert(DataArray, ones(5))
    # Dates are an example of type for which - and .- return a different type from its inputs
    dvd = @data([Base.Date("2000-01-01"), Base.Date("2010-01-01"), Base.Date("2010-01-05")])
    dv[1] = dvd[1] = NA
    @test_da_pda dv begin
        for f in [+, -, *, ^]
            for i in 1:length(dv)
                @test isna(f.(dv, dv)[i]) && isna(dv[i]) || f.(dv, dv)[i] == f(dv[i], dv[i])
            end
        end
        for f in [+,-]
            for i in 1:length(dv)
                @test isna((f)(dv, dv)[i]) && isna(dv[i]) || (f)(dv, dv)[i] == (f)(dv[i], dv[i])
            end
        end
        for f in bit_operators
            for i in 1:length(bv)
                @test f.(bv, bv)[i] == f.(bv[i], bv[i])
            end
        end
        for i in 1:length(dvd)
            @test isna((dvd - dvd)[i]) && isna(dvd[i]) || (dvd - dvd)[i] == dvd[i] - dvd[i]
            @test isna((dvd .- dvd)[i]) && isna(dvd[i]) || (dvd .- dvd)[i] == dvd[i] - dvd[i]
        end
    end

    # + and - with UniformScaling
    mI = zeros(Int, 5, 5) + 5I
    for dm in (convert(DataArray, ones(5, 5)), convert(DataArray, trues(5, 5)))
        dm[1] = NA
        @test_da_pda dm begin
            @test isequal(dm + 5I, dm + mI)
            @test isequal(5I + dm, mI + dm)
            @test isequal(dm - 5I, dm - mI)
            @test isequal(5I - dm, mI - dm)
        end
    end

    # Division (special case since return type for Int is a Float64)
    for curdv in (dv,
                  convert(DataVector{Int}, dv),
                  convert(DataVector{Float32}, dv))
        for i in 1:length(curdv)
            @test isna((curdv./curdv)[i]) && isna(curdv[i]) ||
                    isequal((curdv./curdv)[i], (curdv[i]./curdv[i]))
            @test isna((curdv./2)[i]) && isna(curdv[i]) ||
                    isequal((curdv./2)[i], (curdv[i]./2))
            @test isna((curdv/2)[i]) && isna(curdv[i]) ||
                    isequal((curdv/2)[i], (curdv[i]/2))
        end
    end

    # Unary vector operators on DataVector's
    dv = convert(DataArray, ones(5))
    for f in map(eval, DataArrays.unary_vector_operators)
        @test isequal(f(dv), f(dv.data))
    end
    dv[1] = NA
    for f in map(eval, DataArrays.unary_vector_operators)
        @test isna(f(dv))
    end

    # Pairwise vector operators on DataVector's
    const pairwise_vector_operators = [diff]

    dv = @data([911, 269, 835.0, 448, 772])
    # Dates are an example of type for which operations return a different type from their inputs
    dvd = @data([Base.Date("2000-01-01"), Base.Date("2010-01-01"), Base.Date("2010-01-05")])
    for f in pairwise_vector_operators
        @test isequal(f(dv), f(dv.data))
        @test isequal(f(dvd), f(dvd.data))
    end
    dv = @data([NA, 269, 835.0, 448, 772])
    dvd = @data([NA, Base.Date("2000-01-01"), Base.Date("2010-01-01"), Base.Date("2010-01-05")])
    for f in pairwise_vector_operators
        v = f(dv)
        @test isna(v[1])
        @test isequal(v[2:4], f(dv.data)[2:4])

        d = f(dvd)
        @test isna(d[1])
        @test isequal(d[2:3], f(dvd.data)[2:3])
    end
    dv = @data([911, NA, 835.0, 448, 772])
    dvd = @data([Base.Date("2000-01-01"), NA, Base.Date("2010-01-01"), Base.Date("2010-01-05")])
    for f in pairwise_vector_operators
        v = f(dv)
        @test isna(v[1])
        @test isna(v[2])
        @test isequal(v[3:4], f(dv.data)[3:4])

        d = f(dvd)
        @test isna(d[1])
        @test isna(d[2])
        @test isequal(d[3:3], f(dvd.data)[3:3])
    end
    dv = @data([911, 269, 835.0, 448, NA])
    dvd = @data([Base.Date("2000-01-01"), Base.Date("2010-01-01"), Base.Date("2010-01-05"), NA])
    for f in pairwise_vector_operators
        v = f(dv)
        @test isna(v[4])
        @test isequal(v[1:3], f(dv.data)[1:3])

        d = f(dvd)
        @test isna(d[3])
        @test isequal(d[1:2], f(dvd.data)[1:2])
    end

    # Cumulative vector operators on DataVector's
    dv = convert(DataArray, ones(5))
    for f in [Base.cumprod, Base.cumsum, t -> accumulate(min, t), t -> accumulate(max, t)]
        for i in 1:length(dv)
            @test f(dv)[i] == f(dv.data)[i]
        end
    end
    dv[4] = NA
    for f in [Base.cumprod, Base.cumsum]
        for i in 1:3
            @test f(dv)[i] == f(dv.data)[i]
        end
        for i in 4:5
            @test isna(f(dv)[i])
        end
    end

    # FFT's on DataVector's
    dv = convert(DataArray, ones(5))
    for f in map(eval, DataArrays.ffts)
        @test f(dv) == f(dv.data)
    end
    dv[1] = NA
    for f in map(eval, DataArrays.ffts)
        @test isna(f(dv))
    end

    # Binary vector operators on DataVector's
    dv = convert(DataArray, ones(5))
    for f in map(eval, DataArrays.binary_vector_operators)
        @test f(dv, dv) == f(dv.data, dv.data) ||
                (isnan(f(dv, dv)) && isnan(f(dv.data, dv.data)))
    end
    dv[1] = NA
    for f in map(eval, DataArrays.binary_vector_operators)
        @test isna(f(dv, dv))
    end

    # Boolean operators on DataVector's
    @test any(convert(DataArray, falses(5))) == false
    @test any(convert(DataArray, trues(5))) == true
    @test all(convert(DataArray, falses(5))) == false
    @test all(convert(DataArray, trues(5))) == true
    @test any(PooledDataArray(convert(DataArray, falses(5)))) == false
    @test any(PooledDataArray(convert(DataArray, trues(5)))) == true
    @test all(PooledDataArray(convert(DataArray, falses(5)))) == false
    @test all(PooledDataArray(convert(DataArray, trues(5)))) == true

    dv = convert(DataArray, falses(5))
    dv[3] = true
    @test_da_pda dv begin
        @test any(dv) == true
        @test all(dv) == false
    end

    dv = convert(DataArray, falses(5))
    dv[1] = NA
    @test_da_pda dv begin
        @test isna(any(dv))
        @test all(dv) == false
    end

    dv = convert(DataArray, falses(5))
    dv[2] = NA
    dv[3] = true
    @test_da_pda dv begin
        @test any(dv) == true
        @test all(dv) == false
    end

    dv = convert(DataArray, falses(5))
    dv[2] = NA
    @test_da_pda dv begin
        @test isna(any(dv))
        @test all(dv) == false
    end

    dv = convert(DataArray, falses(1))
    dv[1] = NA
    @test_da_pda dv begin
        @test isna(any(dv))
        @test isna(all(dv))
    end

    dv = convert(DataArray, trues(5))
    dv[1] = NA
    @test_da_pda dv begin
        @test any(dv) == true
        @test isna(all(dv))
    end

    dv = convert(DataArray, trues(5))
    dv[2] = NA
    @test_da_pda dv begin
        @test any(dv) == true
        @test isna(all(dv))
    end

    #
    # Equality tests
    #

    v = [1, 2]
    dv = @data([1, NA])
    alt_dv = @data([2, NA])
    pdv = convert(PooledDataArray, @data([1, NA]))
    alt_pdv = convert(PooledDataArray, @data([2, NA]))

    @test isna(NA == NA)
    @test isna(NA != NA)

    function test_da_eq(v1::AbstractArray, v2::AbstractArray, out)
        for a in (v1, convert(DataArray, v1), convert(PooledDataArray, v1))
            for b in (v2, convert(DataArray, v2), convert(PooledDataArray, v2))
                try
                    @test isequal(a == b, out)
                    @test isequal(b == a, out)
                    if size(a) == size(b)
                        @test isequal(all(a .== b), out)
                        @test isequal(all(b .== a), out)
                    end
                catch e
                    println("a = $a")
                    println("b = $b")
                    rethrow(e)
                end
            end
        end
    end

    # Comparing two otherwise equal DataArray with NAs returns NA
    test_da_eq(dv, dv, NA)
    test_da_eq(dv, v, NA)
    test_da_eq(dv, @data([NA, 1]), NA)
    # Comparing two equal arrays with no NAs returns true
    test_da_eq(v, v, true)
    # Comparing two unequal arrays with no NAs returns false
    test_da_eq(v, @data([1, 3]), false)
    # Comparing two otherwise unequal arrays with NAs returns false
    test_da_eq(dv, alt_dv, false)
    # Comparing two arrays of unequal sizes returns false
    test_da_eq(dv, [1], false)

    @test isequal(dv, dv)
    @test isequal(pdv, pdv)

    @test !isequal(dv, alt_dv)
    @test !isequal(pdv, alt_pdv)

    @test isequal(@data([1, NA]) .== @data([1, NA]), @data([true, NA]))
    @test isequal(@pdata([1, NA]) .== @pdata([1, NA]), @data([true, NA]))

    @test all(isna(NA .== convert(DataArray, ones(5))))
    @test allna(isna(convert(DataArray, ones(5))) .== NA)
    @test all(isna(NA .== PooledDataArray(convert(DataArray, ones(5)))))
    @test allna(isna(convert(PooledDataArray, convert(DataArray, ones(5)))) .== NA)

    # Run length encoding
    dv = convert(DataArray, ones(5))
    dv[3] = NA

    v, l = DataArrays.rle(dv)
    @test isequal(v, @data([1.0, NA, 1.0]))
    @test l == [2, 1, 2]

    rdv = DataArrays.inverse_rle(v, l)
    @test isequal(dv, rdv)

    # Issue #90
    a = @data([false, true, false, true])
    b = @data([false, false, true, true])
    a[:] = NA
    b[:] = NA
    @test allna(a .& b)
    @test allna(a .| b)
end
