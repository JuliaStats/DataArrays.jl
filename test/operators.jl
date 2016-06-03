module TestOperators
    using Base.Test
    using DataArrays, StatsBase

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

    # All arithmetic operators7 return NA when operating on two NA's
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
    dv = @data ones(5)
    @test_da_pda dv begin
        for f in map(eval, DataArrays.numeric_unary_operators)
            for i in 1:length(dv)
                @assert f(dv)[i] == f(dv[i])
            end
        end
    end
    dv = convert(DataArray, trues(5))
    @test_da_pda dv begin
        for f in map(eval, DataArrays.logical_unary_operators)
            for i in 1:length(dv)
                @assert f(dv)[i] == f(dv[i])
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
        for f in map(eval, DataArrays.elementary_functions)
            for i in 1:length(dv)
                @assert f(dv)[i] == f(dv[i])
            end
        end
    end

    # Broadcasting operations between NA's and DataVector's
    dv = convert(DataArray, ones(5))
    @test_da_pda dv begin
        for f in map(eval, [:(.+),
                            :(+),
                            :(.-),
                            :(-),
                            :(*),
                            :(.*),
                            :(./),
                            :(.^),
                            :(Base.div),
                            :(Base.mod),
                            :(Base.fld),
                            :(Base.rem)])
            for i in 1:length(dv)
                @assert isna(f(dv, NA)[i])
                @assert isna(f(NA, dv)[i])
                @assert f(dv, 1)[i] == f(dv[i], 1)
                @assert f(1, dv)[i] == f(1, dv[i])
            end
        end
    end

    @test_da_pda dv begin
        for i in 1:length(dv)
            @assert isna((dv / NA)[i])
            @assert (dv / 1)[i] == dv[i] / 1
        end
    end

    dv = @data([false, true, false, true, false])
    for f in map(eval, DataArrays.bit_operators)
        for i in 1:length(dv)
            @assert f(dv, true)[i] == f(dv[i], true)
            @assert f(true, dv)[i] == f(true, dv[i])
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
        for f in map(eval, DataArrays.array_arithmetic_operators)
            for i in 1:length(dv)
                @assert isna(f(v, dv)[i]) && isna(dv[i]) ||
                        f(v, dv)[i] == f(v[i], dv[i])
                @assert isna(f(dv, v)[i]) && isna(dv[i]) ||
                        f(dv, v)[i] == f(dv[i], v[i])
            end
        end
        for f in map(eval, DataArrays.bit_operators)
            for i in 1:length(bdv)
                @assert f(bv, bdv)[i] == f(bv[i], bdv[i])
                @assert f(bdv, bv)[i] == f(bdv[i], bv[i])
                @assert f(bbv, bdv)[i] == f(bbv[i], bdv[i])
                @assert f(bdv, bbv)[i] == f(bdv[i], bbv[i])
            end
        end
    end

    # Binary operations on pairs of DataVector's
    dv = convert(DataArray, ones(5))
    # Dates are an example of type for which - and .- return a different type from its inputs
    dvd = @data([Base.Date("2000-01-01"), Base.Date("2010-01-01"), Base.Date("2010-01-05")])
    dv[1] = dvd[1] = NA
    @test_da_pda dv begin
        for f in map(eval, DataArrays.array_arithmetic_operators)
            for i in 1:length(dv)
                @assert isna(f(dv, dv)[i]) && isna(dv[i]) ||
                        f(dv, dv)[i] == f(dv[i], dv[i])
            end
        end
        for f in map(eval, DataArrays.bit_operators)
            for i in 1:length(bv)
                @assert f(bv, bv)[i] == f(bv[i], bv[i])
            end
        end
        for i in 1:length(dvd)
            @assert isna((dvd - dvd)[i]) && isna(dvd[i]) ||
                    (dvd - dvd)[i] == dvd[i] - dvd[i]
            @assert isna((dvd .- dvd)[i]) && isna(dvd[i]) ||
                    (dvd .- dvd)[i] == dvd[i] - dvd[i]
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
            @assert isna((curdv./curdv)[i]) && isna(curdv[i]) ||
                    isequal((curdv./curdv)[i], (curdv[i]./curdv[i]))
            @assert isna((curdv./2)[i]) && isna(curdv[i]) ||
                    isequal((curdv./2)[i], (curdv[i]./2))
            @assert isna((curdv/2)[i]) && isna(curdv[i]) ||
                    isequal((curdv/2)[i], (curdv[i]/2))
        end
    end

    # Unary vector operators on DataVector's
    dv = convert(DataArray, ones(5))
    for f in map(eval, DataArrays.unary_vector_operators)
        @assert isequal(f(dv), f(dv.data))
    end
    dv[1] = NA
    for f in map(eval, DataArrays.unary_vector_operators)
        @assert isna(f(dv))
    end

    # Pairwise vector operators on DataVector's
    dv = @data([911, 269, 835.0, 448, 772])
    # Dates are an example of type for which operations return a different type from their inputs
    dvd = @data([Base.Date("2000-01-01"), Base.Date("2010-01-01"), Base.Date("2010-01-05")])
    for f in map(eval, DataArrays.pairwise_vector_operators)
        @assert isequal(f(dv), f(dv.data))
        @assert isequal(f(dvd), f(dvd.data))
    end
    dv = @data([NA, 269, 835.0, 448, 772])
    dvd = @data([NA, Base.Date("2000-01-01"), Base.Date("2010-01-01"), Base.Date("2010-01-05")])
    for f in map(eval, DataArrays.pairwise_vector_operators)
        v = f(dv)
        @assert isna(v[1])
        @assert isequal(v[2:4], f(dv.data)[2:4])

        d = f(dvd)
        @assert isna(d[1])
        @assert isequal(d[2:3], f(dvd.data)[2:3])
    end
    dv = @data([911, NA, 835.0, 448, 772])
    dvd = @data([Base.Date("2000-01-01"), NA, Base.Date("2010-01-01"), Base.Date("2010-01-05")])
    for f in map(eval, DataArrays.pairwise_vector_operators)
        v = f(dv)
        @assert isna(v[1])
        @assert isna(v[2])
        @assert isequal(v[3:4], f(dv.data)[3:4])

        d = f(dvd)
        @assert isna(d[1])
        @assert isna(d[2])
        @assert isequal(d[3:3], f(dvd.data)[3:3])
    end
    dv = @data([911, 269, 835.0, 448, NA])
    dvd = @data([Base.Date("2000-01-01"), Base.Date("2010-01-01"), Base.Date("2010-01-05"), NA])
    for f in map(eval, DataArrays.pairwise_vector_operators)
        v = f(dv)
        @assert isna(v[4])
        @assert isequal(v[1:3], f(dv.data)[1:3])

        d = f(dvd)
        @assert isna(d[3])
        @assert isequal(d[1:2], f(dvd.data)[1:2])
    end

    # Cumulative vector operators on DataVector's
    dv = convert(DataArray, ones(5))
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
        for i in 4:5
            @assert isna(f(dv)[i])
        end
    end

    # FFT's on DataVector's
    dv = convert(DataArray, ones(5))
    for f in map(eval, DataArrays.ffts)
        @assert f(dv) == f(dv.data)
    end
    dv[1] = NA
    for f in map(eval, DataArrays.ffts)
        @assert isna(f(dv))
    end

    # Binary vector operators on DataVector's
    dv = convert(DataArray, ones(5))
    for f in map(eval, DataArrays.binary_vector_operators)
        @assert f(dv, dv) == f(dv.data, dv.data) ||
                (isnan(f(dv, dv)) && isnan(f(dv.data, dv.data)))
    end
    dv[1] = NA
    for f in map(eval, DataArrays.binary_vector_operators)
        @assert isna(f(dv, dv))
    end

    # Boolean operators on DataVector's
    @assert any(convert(DataArray, falses(5))) == false
    @assert any(convert(DataArray, trues(5))) == true
    @assert all(convert(DataArray, falses(5))) == false
    @assert all(convert(DataArray, trues(5))) == true
    @assert any(PooledDataArray(convert(DataArray, falses(5)))) == false
    @assert any(PooledDataArray(convert(DataArray, trues(5)))) == true
    @assert all(PooledDataArray(convert(DataArray, falses(5)))) == false
    @assert all(PooledDataArray(convert(DataArray, trues(5)))) == true

    dv = convert(DataArray, falses(5))
    dv[3] = true
    @test_da_pda dv begin
        @assert any(dv) == true
        @assert all(dv) == false
    end

    dv = convert(DataArray, falses(5))
    dv[1] = NA
    @test_da_pda dv begin
        @assert isna(any(dv))
        @assert all(dv) == false
    end

    dv = convert(DataArray, falses(5))
    dv[2] = NA
    dv[3] = true
    @test_da_pda dv begin
        @assert any(dv) == true
        @assert all(dv) == false
    end

    dv = convert(DataArray, falses(5))
    dv[2] = NA
    @test_da_pda dv begin
        @assert isna(any(dv))
        @assert all(dv) == false
    end

    dv = convert(DataArray, falses(1))
    dv[1] = NA
    @test_da_pda dv begin
        @assert isna(any(dv))
        @assert isna(all(dv))
    end

    dv = convert(DataArray, trues(5))
    dv[1] = NA
    @test_da_pda dv begin
        @assert any(dv) == true
        @assert isna(all(dv))
    end

    dv = convert(DataArray, trues(5))
    dv[2] = NA
    @test_da_pda dv begin
        @assert any(dv) == true
        @assert isna(all(dv))
    end

    #
    # Equality tests
    #

    v = [1, 2]
    dv = @data([1, NA])
    alt_dv = @data([2, NA])
    pdv = convert(PooledDataArray, @data([1, NA]))
    alt_pdv = convert(PooledDataArray, @data([2, NA]))

    @assert isna(NA == NA)
    @assert isna(NA != NA)

    function test_da_eq(v1::AbstractArray, v2::AbstractArray, out)
        for a in (v1, convert(DataArray, v1), convert(PooledDataArray, v1))
            for b in (v2, convert(DataArray, v2), convert(PooledDataArray, v2))
                try
                    @assert isequal(a == b, out)
                    @assert isequal(b == a, out)
                    if size(a) == size(b)
                        @assert isequal(all(a .== b), out)
                        @assert isequal(all(b .== a), out)
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

    @assert isequal(dv, dv)
    @assert isequal(pdv, pdv)

    @assert !isequal(dv, alt_dv)
    @assert !isequal(pdv, alt_pdv)

    @assert isequal(@data([1, NA]) .== @data([1, NA]), @data([true, NA]))
    @assert isequal(@pdata([1, NA]) .== @pdata([1, NA]), @data([true, NA]))

    @assert all(isna(NA .== convert(DataArray, ones(5))))
    @assert allna(isna(convert(DataArray, ones(5))) .== NA)
    @assert all(isna(NA .== PooledDataArray(convert(DataArray, ones(5)))))
    @assert allna(isna(convert(PooledDataArray, convert(DataArray, ones(5)))) .== NA)

    # Run length encoding
    dv = convert(DataArray, ones(5))
    dv[3] = NA

    v, l = DataArrays.rle(dv)
    @assert isequal(v, @data([1.0, NA, 1.0]))
    @assert l == [2, 1, 2]

    rdv = DataArrays.inverse_rle(v, l)
    @assert isequal(dv, rdv)

    # Issue #90
    a = @data([false, true, false, true]);
    b = @data([false, false, true, true]);
    a[:] = NA;
    b[:] = NA;
    @test allna(a & b)
    @test allna(a | b)
end
