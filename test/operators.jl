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

    # All unary operators return missing when evaluating missing
    for f in [+, -]
        @test ismissing(f(missing))
    end

    # All elementary functions return missing when evaluating missing
    for f in elementary_functions
        @test ismissing(f(missing))
    end

    # All comparison operators return missing when comparing missing with missing
    # All comparison operators return missing when comparing scalars with missing
    # All comparison operators return missing when comparing missing with scalars
    for f in comparison_operators
        @test ismissing(f(missing, missing))
        @test ismissing(f(missing, 1))
        @test ismissing(f(1, missing))
    end

    # All arithmetic operators return missing when operating on two missings
    # All arithmetic operators return missing when operating on a scalar and an missing
    # All arithmetic operators return missing when operating on an missing and a scalar
    for f in arithmetic_operators
        @test ismissing(f(missing, missing))
        @test ismissing(f(1, missing))
        @test ismissing(f(missing, 1))
    end

    # All bit operators return missing when operating on two missings
    # All bit operators return missing when operating on a scalar and an missing
    # All bit operators return missing when operating on an missing and a scalar
    for f in bit_operators
        @test ismissing(f(missing, missing))
        @test ismissing(f(1, missing))
        @test ismissing(f(missing, 1))
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
                # No missing
                @test isequal(da.', dat)
                @test isequal(da', conj(dat))

                # With missing
                # XXX we should fix indexing so that this isn't necessary
                for i = 1:length(da)
                    da[i] == 5 && (da[i] = missing)
                    dat[i] == 5 && (dat[i] = missing)
                end

                # Make sure that missings are undefined in the non-bits array
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

    # Broadcasting operations between missings and DataVector's
    dv = convert(DataArray, ones(5))
    @test_da_pda dv begin
        for f in [+, *, Base.div, Base.mod, Base.fld, Base.rem]
            for i in 1:length(dv)
                @test ismissing(f(dv, missing)[i])
                @test ismissing(f(missing, dv)[i])
                @test f(dv, 1)[i] == f(dv[i], 1)
                @test f(1, dv)[i] == f(1, dv[i])
            end
        end
        for f in arithmetic_operators
            for i in 1:length(dv)
                @test ismissing(f.(dv, missing)[i])
                @test ismissing(f.(missing, dv)[i])
                @test f.(dv, 1)[i] == f(dv[i], 1)
                @test f.(1, dv)[i] == f(1, dv[i])
            end
        end
    end

    @test_da_pda dv begin
        for i in 1:length(dv)
            @test ismissing((dv / missing)[i])
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
    dv[1] = missing
    bv = [true, false, false, true, true]
    bbv = BitArray([true, false, false, true, true])
    bdv = @data [false, true, false, false, true]
    @test_da_pda dv begin
        for f in [+, -, *, ^]
            for i in 1:length(dv)
                @test ismissing(f.(v, dv)[i]) && ismissing(dv[i]) || f.(v, dv)[i] == f(v[i], dv[i])
                @test ismissing(f.(dv, v)[i]) && ismissing(dv[i]) || f.(dv, v)[i] == f(dv[i], v[i])
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
    dv[1] = dvd[1] = missing
    @test_da_pda dv begin
        for f in [+, -, *, ^]
            for i in 1:length(dv)
                @test ismissing(f.(dv, dv)[i]) && ismissing(dv[i]) || f.(dv, dv)[i] == f(dv[i], dv[i])
            end
        end
        for f in [+,-]
            for i in 1:length(dv)
                @test ismissing((f)(dv, dv)[i]) && ismissing(dv[i]) || (f)(dv, dv)[i] == (f)(dv[i], dv[i])
            end
        end
        for f in bit_operators
            for i in 1:length(bv)
                @test f.(bv, bv)[i] == f.(bv[i], bv[i])
            end
        end
        for i in 1:length(dvd)
            @test ismissing((dvd - dvd)[i]) && ismissing(dvd[i]) || (dvd - dvd)[i] == dvd[i] - dvd[i]
            @test ismissing((dvd .- dvd)[i]) && ismissing(dvd[i]) || (dvd .- dvd)[i] == dvd[i] - dvd[i]
        end
    end

    # + and - with UniformScaling
    mI = zeros(Int, 5, 5) + 5I
    for dm in (convert(DataArray, ones(5, 5)), convert(DataArray, trues(5, 5)))
        dm[1] = missing
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
            @test ismissing((curdv./curdv)[i]) && ismissing(curdv[i]) ||
                    isequal((curdv./curdv)[i], (curdv[i]./curdv[i]))
            @test ismissing((curdv./2)[i]) && ismissing(curdv[i]) ||
                    isequal((curdv./2)[i], (curdv[i]./2))
            @test ismissing((curdv/2)[i]) && ismissing(curdv[i]) ||
                    isequal((curdv/2)[i], (curdv[i]/2))
        end
    end

    # Unary vector operators on DataVector's
    dv = convert(DataArray, ones(5))
    for f in map(eval, DataArrays.unary_vector_operators)
        @test isequal(f(dv), f(dv.data))
    end
    dv[1] = missing
    for f in map(eval, DataArrays.unary_vector_operators)
        @test ismissing(f(dv))
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
    dv = @data([missing, 269, 835.0, 448, 772])
    dvd = @data([missing, Base.Date("2000-01-01"), Base.Date("2010-01-01"), Base.Date("2010-01-05")])
    for f in pairwise_vector_operators
        v = f(dv)
        @test ismissing(v[1])
        @test isequal(v[2:4], f(dv.data)[2:4])

        d = f(dvd)
        @test ismissing(d[1])
        @test isequal(d[2:3], f(dvd.data)[2:3])
    end
    dv = @data([911, missing, 835.0, 448, 772])
    dvd = @data([Base.Date("2000-01-01"), missing, Base.Date("2010-01-01"), Base.Date("2010-01-05")])
    for f in pairwise_vector_operators
        v = f(dv)
        @test ismissing(v[1])
        @test ismissing(v[2])
        @test isequal(v[3:4], f(dv.data)[3:4])

        d = f(dvd)
        @test ismissing(d[1])
        @test ismissing(d[2])
        @test isequal(d[3:3], f(dvd.data)[3:3])
    end
    dv = @data([911, 269, 835.0, 448, missing])
    dvd = @data([Base.Date("2000-01-01"), Base.Date("2010-01-01"), Base.Date("2010-01-05"), missing])
    for f in pairwise_vector_operators
        v = f(dv)
        @test ismissing(v[4])
        @test isequal(v[1:3], f(dv.data)[1:3])

        d = f(dvd)
        @test ismissing(d[3])
        @test isequal(d[1:2], f(dvd.data)[1:2])
    end

    # Cumulative vector operators on DataVector's
    dv = convert(DataArray, ones(5))
    for f in [Base.cumprod, Base.cumsum, t -> accumulate(min, t), t -> accumulate(max, t)]
        for i in 1:length(dv)
            @test f(dv)[i] == f(dv.data)[i]
        end
    end
    dv[4] = missing
    for f in [Base.cumprod, Base.cumsum]
        for i in 1:3
            @test f(dv)[i] == f(dv.data)[i]
        end
        for i in 4:5
            @test ismissing(f(dv)[i])
        end
    end

    # Binary vector operators on DataVector's
    dv = convert(DataArray, ones(5))
    for f in map(eval, DataArrays.binary_vector_operators)
        @test f(dv, dv) == f(dv.data, dv.data) ||
                (isnan(f(dv, dv)) && isnan(f(dv.data, dv.data)))
    end
    dv[1] = missing
    for f in map(eval, DataArrays.binary_vector_operators)
        @test ismissing(f(dv, dv))
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
    dv[1] = missing
    @test_da_pda dv begin
        @test ismissing(any(dv))
        @test all(dv) == false
    end

    dv = convert(DataArray, falses(5))
    dv[2] = missing
    dv[3] = true
    @test_da_pda dv begin
        @test any(dv) == true
        @test all(dv) == false
    end

    dv = convert(DataArray, falses(5))
    dv[2] = missing
    @test_da_pda dv begin
        @test ismissing(any(dv))
        @test all(dv) == false
    end

    dv = convert(DataArray, falses(1))
    dv[1] = missing
    @test_da_pda dv begin
        @test ismissing(any(dv))
        @test ismissing(all(dv))
    end

    dv = convert(DataArray, trues(5))
    dv[1] = missing
    @test_da_pda dv begin
        @test any(dv) == true
        @test ismissing(all(dv))
    end

    dv = convert(DataArray, trues(5))
    dv[2] = missing
    @test_da_pda dv begin
        @test any(dv) == true
        @test ismissing(all(dv))
    end

    #
    # Equality tests
    #

    v = [1, 2]
    dv = @data([1, missing])
    alt_dv = @data([2, missing])
    pdv = convert(PooledDataArray, @data([1, missing]))
    alt_pdv = convert(PooledDataArray, @data([2, missing]))

    @test ismissing(missing == missing)
    @test ismissing(missing != missing)

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

    # Comparing two otherwise equal DataArray with missings returns missing
    test_da_eq(dv, dv, missing)
    test_da_eq(dv, v, missing)
    test_da_eq(dv, @data([missing, 1]), missing)
    # Comparing two equal arrays with no missings returns true
    test_da_eq(v, v, true)
    # Comparing two unequal arrays with no missings returns false
    test_da_eq(v, @data([1, 3]), false)
    # Comparing two otherwise unequal arrays with missings returns false
    test_da_eq(dv, alt_dv, false)
    # Comparing two arrays of unequal sizes returns false
    test_da_eq(dv, [1], false)

    @test isequal(dv, dv)
    @test isequal(pdv, pdv)

    @test !isequal(dv, alt_dv)
    @test !isequal(pdv, alt_pdv)

    @test isequal(@data([1, missing]) .== @data([1, missing]), @data([true, missing]))
    @test isequal(@pdata([1, missing]) .== @pdata([1, missing]), @data([true, missing]))

    @test all(ismissing.(missing .== convert(DataArray, ones(5))))
    @test all(ismissing, ismissing.(convert(DataArray, ones(5))) .== missing)
    @test all(ismissing.(missing .== PooledDataArray(convert(DataArray, ones(5)))))
    @test all(ismissing, ismissing.(convert(PooledDataArray, convert(DataArray, ones(5)))) .== missing)

    # Run length encoding
    dv = convert(DataArray, ones(5))
    dv[3] = missing

    v, l = DataArrays.rle(dv)
    @test isequal(v, @data([1.0, missing, 1.0]))
    @test l == [2, 1, 2]

    rdv = DataArrays.inverse_rle(v, l)
    @test isequal(dv, rdv)

    # Issue #90
    a = @data([false, true, false, true])
    b = @data([false, false, true, true])
    a[:] = missing
    b[:] = missing
    @test all(ismissing, a .& b)
    @test all(ismissing, a .| b)
end
