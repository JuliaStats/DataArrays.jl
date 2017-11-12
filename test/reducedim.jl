macro test_da_approx_eq(da1, da2)
    quote
        v1 = $(esc(da1))
        v2 = $(esc(da2))
        na = ismissing.(v1)
        @test na == ismissing.(v2)
        defined = (!).(na)
        if any(defined)
            @test isapprox(v1[defined], v2[defined], nans = true)
        end
    end
end

@testset "Reducedim" begin
    # Test for fast unit stride BitArray functions
    function test_any()
        bits = bitrand(64*5)
        for i = 1:length(bits), j = i:length(bits)
            v = false
            for k = i:j
                v |= bits[k]
            end
            Base.Test.@test DataArrays._any(bits, i, j) == v
        end
    end
    test_any()

    function test_count()
        bits = bitrand(64*5)
        for i = 1:length(bits), j = i:length(bits)
            v = 0
            for k = i:j
                v += bits[k]
            end
            Base.Test.@test DataArrays._count(bits, i, j) == v
        end
    end
    test_count()

    # mapslices from Base, hacked to work for these cases
    function safe_mapslices{T}(f::Function, A::AbstractArray{T}, region, skipmissing)
        dims = intersect(region, 1:ndims(A))
        if isempty(dims)
            if skipmissing
                naval = f(Missings.T(T)[], 1)
                A = copy(A)
                A[ismissing.(A)] = isempty(naval) ? missing : naval[1]
            end
            return A
        end

        if isempty(dims)
            return map(f,A)
        end

        dimsA = collect(size(A))
        ndimsA = ndims(A)
        alldims = collect(1:ndimsA)

        otherdims = setdiff(alldims, dims)

        idx = Vector{Any}(ndimsA)
        fill!(idx, 1)
        Asliceshape = tuple(dimsA[dims]...)
        itershape   = tuple(dimsA[otherdims]...)
        for d in dims
            idx[d] = 1:size(A,d)
        end

        r1 = f(copy(reshape(A[idx...], Asliceshape)); skipmissing=skipmissing)

        # determine result size and allocate
        Rsize = copy(dimsA)
        # TODO: maybe support removing dimensions
        if !isa(r1, AbstractArray) || ndims(r1) == 0
            r2 = similar(A, T, 1)
            r2[1] = r1
            r1 = r2
        end
        Rsize[dims] = [size(r1)...; ones(Int,max(0,length(dims)-ndims(r1)))]
        R = similar(r1, tuple(Rsize...))

        ridx = Vector{Any}(ndims(R))
        fill!(ridx, 1)
        for d in dims
            ridx[d] = 1:size(R,d)
        end

        R[ridx...] = r1

        first = true

        for idxs = CartesianRange(itershape)
            if first
                first = false
            else
                ia = [idxs.I...]
                idx[otherdims] = ia
                ridx[otherdims] = ia
                try
                    R[ridx...] = f(copy(reshape(A[idx...], Asliceshape)); skipmissing=skipmissing)
                catch e
                    if (isa(e, ErrorException) && e.msg == "Reducing over an empty array is not allowed.") || (isa(e, ArgumentError) && e.msg == "reducing over an empty collection is not allowed")

                        R[ridx...] = missing
                    else
                        println(typeof(e))
                        rethrow(e)
                    end
                end
            end
        end

        return R
    end

    myvarzm(x; skipmissing::Bool=false) = var(x; mean=0, skipmissing=skipmissing)
    myvar1m(x; skipmissing::Bool=false) = var(x; mean=1, skipmissing=skipmissing)

    for Areduc in (DataArray(rand(3, 4, 5, 6)),
                   DataArray(rand(3, 4, 5, 6), rand(3, 4, 5, 6) .< 0.2))
        for skipmissing = (false, true)
            for region in Any[
                1, 2, 3, 4, 5, (1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4),
                (1, 2, 3), (1, 3, 4), (2, 3, 4), (1, 2, 3, 4)]
                # println("region = $region, skipmissing = $skipmissing")

                outputs = Any[DataArray(fill(NaN, length.(Base.reduced_indices(indices(Areduc), region))))]
                hasmissings = any(ismissing, Areduc)
                if hasmissings && !skipmissing
                    # Should throw an error reducing to non-DataArray
                    @test_throws MissingException sum!(outputs[1].data, Areduc; skipmissing=skipmissing)
                else
                    # Should be able to reduce to non-DataArray
                    push!(outputs, outputs[1].data)
                end

                for r in outputs
                    @test_da_approx_eq sum!(r, Areduc; skipmissing=skipmissing) safe_mapslices(sum, Areduc, region, skipmissing)
                    @test_da_approx_eq prod!(r, Areduc; skipmissing=skipmissing) safe_mapslices(prod, Areduc, region, skipmissing)
                    if !hasmissings
                        @test_da_approx_eq maximum!(r, Areduc; skipmissing=skipmissing) safe_mapslices(maximum, Areduc, region, skipmissing)
                        @test_da_approx_eq minimum!(r, Areduc; skipmissing=skipmissing) safe_mapslices(minimum, Areduc, region, skipmissing)
                    end
                    @test_da_approx_eq Base.sumabs!(r, Areduc; skipmissing=skipmissing) safe_mapslices(sum, abs(Areduc), region, skipmissing)
                    @test_da_approx_eq Base.sumabs2!(r, Areduc; skipmissing=skipmissing) safe_mapslices(sum, abs2(Areduc), region, skipmissing)
                    @test_da_approx_eq mean!(r, Areduc; skipmissing=skipmissing) safe_mapslices(mean, Areduc, region, skipmissing)
                end

                @test_da_approx_eq sum(Areduc, region; skipmissing=skipmissing) safe_mapslices(sum, Areduc, region, skipmissing)
                @test_da_approx_eq prod(Areduc, region; skipmissing=skipmissing) safe_mapslices(prod, Areduc, region, skipmissing)
                @test_da_approx_eq maximum(Areduc, region; skipmissing=skipmissing) safe_mapslices(maximum, Areduc, region, skipmissing)
                @test_da_approx_eq minimum(Areduc, region; skipmissing=skipmissing) safe_mapslices(minimum, Areduc, region, skipmissing)
                @test_da_approx_eq Base.sumabs(Areduc, region; skipmissing=skipmissing) safe_mapslices(sum, abs(Areduc), region, skipmissing)
                @test_da_approx_eq Base.sumabs2(Areduc, region; skipmissing=skipmissing) safe_mapslices(sum, abs2(Areduc), region, skipmissing)
                @test_da_approx_eq mean(Areduc, region; skipmissing=skipmissing) safe_mapslices(mean, Areduc, region, skipmissing)

                if region != 5
                    @test_da_approx_eq var(Areduc, region; skipmissing=skipmissing) safe_mapslices(var, Areduc, region, skipmissing)
                    @test_da_approx_eq var(Areduc, region; mean=0, skipmissing=skipmissing) safe_mapslices(myvarzm, Areduc, region, skipmissing)
                    for r in outputs
                        @test_da_approx_eq var(Areduc, region; mean=fill!(r, 1), skipmissing=skipmissing) safe_mapslices(myvar1m, Areduc, region, skipmissing)
                    end
                end
            end
        end
    end

    # Test missing-skipping behavior for maximum
    a = @data([missing missing; 3 4])
    @test isequal(maximum(a, 1; skipmissing=true), [3 4])
    @test isequal(maximum!(zeros(1, 2), a; skipmissing=true), [3 4])

    # Maximum should give an missing in the output if all values along dimension are missing
    @test isequal(maximum(a, 2; skipmissing=true), @data([missing 4])')
    # Maximum should refuse to reduce to a non-DataArray
    @test_throws MissingException maximum!(zeros(2, 1), a; skipmissing=true)
end
