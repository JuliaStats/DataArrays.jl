@testset "Indexing" begin
    function run_tests(datype)
        data = rand(10, 10)
        na = bitrand(10, 10)
        A = datype(data, na)

        # Scalar getindex
        for i = 1:100
            if na[i]
                @test ismissing(A[i])
            else
                @test A[i] == data[i]
            end
        end
        for i = 1:10, j = 1:10
            if na[i, j]
                @test ismissing(A[i, j])
            else
                @test A[i, j] == data[i, j]
            end
        end

        # getindex with AbstractVectors
        rg = 2:9
        v = A[rg]
        for i = 1:length(rg)
            if na[rg[i]]
                @test ismissing(v[i])
            else
                @test v[i] == data[rg[i]]
            end
        end

        v = A[rg, 9]
        for i = 1:length(rg)
            if na[rg[i], 9]
                @test ismissing(v[i])
            else
                @test v[i] == data[rg[i], 9]
            end
        end

        rg2 = 5:7
        v = A[rg, rg2]
        for j = 1:length(rg2), i = 1:length(rg)
            if na[rg[i], rg2[j]]
                @test ismissing(v[i, j])
            else
                @test v[i, j] == data[rg[i], rg2[j]]
            end
        end

        # getindex with AbstractVector{Bool}
        b = bitrand(10, 10)
        rg = find(b)
        v = A[b]
        for i = 1:length(rg)
            if na[rg[i]]
                @test ismissing(v[i])
            else
                @test v[i] == data[rg[i]]
            end
        end

        # getindex with DataVectors with missingness throws
        @test_throws MissingException A[@data([1, 2, 3, missing])]

        # setindex! with scalar indices
        data = rand(10, 10)
        for i = 1:100
            A[i] = data[i]
        end
        @test A == data

        data = rand(10, 10)
        for i = 1:10, j = 1:10
            A[i, j] = data[i, j]
        end
        @test A == data

        na = bitrand(10, 10)
        for i = 1:100
            na[i] && (A[i] = missing)
        end

        # setindex! with scalar and vector indices
        rg = 2:9
        data[rg] = 1.0
        A[rg] = 1.0
        for i = 1:length(rg)
            @test A[rg[i]] == 1.0
        end

        # setindex! with missing and vector indices
        rg = 5:13
        na[rg] = true
        A[rg] = missing
        for i = 1:length(rg)
            @test ismissing(A[rg[i]])
        end

        # setindex! with vector and vector indices
        rg = 12:67
        data[rg] = rand(length(rg))
        A[rg] = data[rg]
        for i = 1:length(rg)
            @test A[rg[i]] == data[rg[i]]
        end

        # setindex! with DataArray/PooledDataArray and vector indices
        for datype2 in (DataArray, PooledDataArray)
            newdata = rand(3, 3)
            newdata[1:2:9] = data[1:2:9]
            newna = bitrand(3, 3)
            rg1, rg2 = 1:3, 5:7
            data[rg1, rg2] = newdata
            na[rg1, rg2] = newna
            A[rg1, rg2] = datype2(newdata, newna)
            for j = rg2, i = rg1
                if na[i, j]
                    @test ismissing(A[i, j])
                else
                    @test A[i, j] == data[i, j]
                end
            end
        end
    end

    run_tests(DataArray)
    run_tests(PooledDataArray)
end
