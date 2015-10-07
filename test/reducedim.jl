module TestReducedim
using DataArrays, Base.Test, Compat

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
function safe_mapslices{T}(f::Function, A::AbstractArray{T}, region, skipna)
    dims = intersect(region, 1:ndims(A))
    if isempty(dims)
        if skipna
            naval = f(T[], 1)
            A = copy(A)
            A[isna(A)] = isempty(naval) ? NA : naval[1]
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

    idx = cell(ndimsA)
    fill!(idx, 1)
    Asliceshape = tuple(dimsA[dims]...)
    itershape   = tuple(dimsA[otherdims]...)
    for d in dims
        idx[d] = 1:size(A,d)
    end

    r1 = f(reshape(A[idx...], Asliceshape); skipna=skipna)

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

    ridx = cell(ndims(R))
    fill!(ridx, 1)
    for d in dims
        ridx[d] = 1:size(R,d)
    end

    R[ridx...] = r1

    first = true

    if VERSION < v"0.4.0-"
        cartesianmap(itershape) do idxs...
            if first
                first = false
            else
                ia = [idxs...]
                idx[otherdims] = ia
                ridx[otherdims] = ia
                try
                    R[ridx...] = f(reshape(A[idx...], Asliceshape); skipna=skipna)
                catch e
                    if (isa(e, ErrorException) && e.msg == "Reducing over an empty array is not allowed.") ||
                        (isa(e, ArgumentError) && e.msg == "reducing over an empty collection is not allowed")

                        R[ridx...] = NA
                    else
                        println(typeof(e))
                        println(e.msg)
                        rethrow(e)
                    end
                end
            end
        end
    else
        for idxs = CartesianRange(itershape)
            if first
                first = false
            else
                ia = [idxs.I...]
                idx[otherdims] = ia
                ridx[otherdims] = ia
                try
                    R[ridx...] = f(reshape(A[idx...], Asliceshape); skipna=skipna)
                catch e
                    if (isa(e, ErrorException) && e.msg == "Reducing over an empty array is not allowed.") || (isa(e, ArgumentError) && e.msg == "reducing over an empty collection is not allowed")

                        R[ridx...] = NA
                    else
                        println(typeof(e))
                        println(e.msg)
                        rethrow(e)
                    end
                end
            end
        end
    end

    return R
end

macro test_da_approx_eq(da1, da2)
    quote
        v1 = $da1
        v2 = $da2
        na = isna(v1)
        @test na == isna(v2)
        defined = !na
        if any(defined)
            @test_approx_eq v1[defined] v2[defined]
        end
    end
end

myvarzm(x; skipna::Bool=false) = var(x; mean=0, skipna=skipna)
myvar1m(x; skipna::Bool=false) = var(x; mean=1, skipna=skipna)

for Areduc in (DataArray(rand(3, 4, 5, 6)),
               DataArray(rand(3, 4, 5, 6), rand(3, 4, 5, 6) .< 0.2))
    for skipna = (false, true)
        for region in Any[
            1, 2, 3, 4, 5, (1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4),
            (1, 2, 3), (1, 3, 4), (2, 3, 4), (1, 2, 3, 4)]
            # println("region = $region, skipna = $skipna")

            outputs = Any[DataArray(fill(NaN, Base.reduced_dims(size(Areduc), region)))]
            has_na = anyna(Areduc)
            if has_na && !skipna
                # Should throw an error reducing to non-DataArray
                @test_throws NAException sum!(outputs[1].data, Areduc; skipna=skipna)
            else
                # Should be able to reduce to non-DataArray
                push!(outputs, outputs[1].data)
            end

            for r in outputs
                @test_da_approx_eq sum!(r, Areduc; skipna=skipna) safe_mapslices(sum, Areduc, region, skipna)
                @test_da_approx_eq prod!(r, Areduc; skipna=skipna) safe_mapslices(prod, Areduc, region, skipna)
                if !has_na
                    @test_da_approx_eq maximum!(r, Areduc; skipna=skipna) safe_mapslices(maximum, Areduc, region, skipna)
                    @test_da_approx_eq minimum!(r, Areduc; skipna=skipna) safe_mapslices(minimum, Areduc, region, skipna)
                end
                @test_da_approx_eq Base.sumabs!(r, Areduc; skipna=skipna) safe_mapslices(sum, abs(Areduc), region, skipna)
                @test_da_approx_eq Base.sumabs2!(r, Areduc; skipna=skipna) safe_mapslices(sum, abs2(Areduc), region, skipna)
                @test_da_approx_eq mean!(r, Areduc; skipna=skipna) safe_mapslices(mean, Areduc, region, skipna)
            end

            @test_da_approx_eq sum(Areduc, region; skipna=skipna) safe_mapslices(sum, Areduc, region, skipna)
            @test_da_approx_eq prod(Areduc, region; skipna=skipna) safe_mapslices(prod, Areduc, region, skipna)
            @test_da_approx_eq maximum(Areduc, region; skipna=skipna) safe_mapslices(maximum, Areduc, region, skipna)
            @test_da_approx_eq minimum(Areduc, region; skipna=skipna) safe_mapslices(minimum, Areduc, region, skipna)
            @test_da_approx_eq Base.sumabs(Areduc, region; skipna=skipna) safe_mapslices(sum, abs(Areduc), region, skipna)
            @test_da_approx_eq Base.sumabs2(Areduc, region; skipna=skipna) safe_mapslices(sum, abs2(Areduc), region, skipna)
            @test_da_approx_eq mean(Areduc, region; skipna=skipna) safe_mapslices(mean, Areduc, region, skipna)

            if region != 5
                @test_da_approx_eq var(Areduc, region; skipna=skipna) safe_mapslices(var, Areduc, region, skipna)
                @test_da_approx_eq var(Areduc, region; mean=0, skipna=skipna) safe_mapslices(myvarzm, Areduc, region, skipna)
                for r in outputs
                    @test_da_approx_eq var(Areduc, region; mean=fill!(r, 1), skipna=skipna) safe_mapslices(myvar1m, Areduc, region, skipna)
                end
            end
        end
    end
end

# Test NA-skipping behavior for maximum
a = @data([NA NA; 3 4])
@test isequal(maximum(a, 1; skipna=true), [3 4])
@test isequal(maximum!(zeros(1, 2), a; skipna=true), [3 4])

# Maximum should give an NA in the output if all values along dimension are NA
@test isequal(maximum(a, 2; skipna=true), @data([NA 4])')
# Maximum should refuse to reduce to a non-DataArray
@test_throws NAException maximum!(zeros(2, 1), a; skipna=true)
end
