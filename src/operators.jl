const unary_vector_operators = [:(Base.median),
                                :(StatsBase.mad),
                                :(Base.norm),
                                :(StatsBase.skewness),
                                :(StatsBase.kurtosis)]

# TODO: dist, iqr

const binary_vector_operators = [:(Base.dot),
                                 :(Base.cor),
                                 :(Base.cov),
                                 :(StatsBase.corspearman)]

const rowwise_operators = [:rowminimums,
                           :rowmaxs,
                           :rowprods,
                           :rowsums,
                           :rowmeans,
                           :rowmedians,
                           :rowstds,
                           :rowvars,
                           :rownorms]

# Swap arguments to fname() anywhere in AST. Returns the number of
# arguments swapped
function swapargs(ast::Expr, fname::Union{Expr, Symbol})
    if ast.head == :call &&
       (ast.args[1] == fname ||
        (isa(ast.args[1], Expr) && ast.args[1].head == :curly &&
         ast.args[1].args[1] == fname)) &&
       length(ast.args) == 3

        ast.args[2], ast.args[3] = ast.args[3], ast.args[2]
        1
    else
        n = 0
        for arg in ast.args
            n += swapargs(arg, fname)
        end
        n
    end
end
function swapargs(ast, fname::Union{Expr, Symbol})
    ast
    0
end

# Return a block consisting of both the given function and a copy of
# the function in which arguments to the function itself and any
# 2-argument calls to a function of the same name are swapped
macro swappable(func, syms...)
    if (func.head != :function && func.head != :(=)) ||
       func.args[1].head != :call || length(func.args[1].args) != 3
        throw(ArgumentError("@swappable may only be applied to functions of two arguments"))
    end

    func2 = deepcopy(func)
    fname = func2.args[1].args[1]
    if isa(fname, Expr) && fname.head == :curly
        fname = fname.args[1]
    end

    for s in unique([fname, syms...])
        if swapargs(func2, s) < 1
            throw(ErrorException("No argument swapped"))
        end
    end
    esc(Expr(:block, func, func2))
end

#
# Unary operator macros for DataArrays
#

# Apply unary operator to non-missing members of a DataArray or
# AbstractDataArray
macro dataarray_unary(f, intype, outtype, N...)
    esc(quote
        function $(f){T<:$(intype)}(d::$(isempty(N) ? :(DataArray{T}) : :(DataArray{T,$(N[1])})))
            data = d.data
            res = similar(data, $(outtype))
            @bitenumerate d.na i na begin
                if !na
                    @inbounds res[i] = $(f)(data[i])
                end
            end
            DataArray(res, copy(d.na))
        end
        function $(f){T<:$(intype)}(adv::$(isempty(N) ? :(AbstractDataArray{T}) : :(AbstractDataArray{T,$(N[1])})))
            res = similar(adv, $(outtype))
            for i = 1:length(adv)
                res[i] = ($f)(adv[i])
            end
            res
        end
    end)
end

#
# Binary operator macros for DataArrays
#

# Binary operators with one scalar argument
macro dataarray_binary_scalar(vectorfunc, scalarfunc, outtype, swappable)
    esc(Expr(:block,
        # DataArray and AbstractDataArray with scalar
        # XXX It would be really nice to make this work with arbitrary
        # types, but doing so results in a bunch of method ambiguity
        # warnings
        Any[
            begin
                if outtype == :nothing
                    outtype = :(promote_op($scalarfunc,
                                           eltype(a), eltype(b)))
                end
                fns = Any[
                    :(function $(vectorfunc)(a::DataArray, b::$t)
                        data = a.data
                        res = similar(data, $outtype)
                        @bitenumerate a.na i na begin
                            if !na
                                @inbounds res[i] = $(scalarfunc)(data[i], b)
                            end
                        end
                        DataArray(res, copy(a.na))
                    end),
                    :(function $(vectorfunc)(a::AbstractDataArray, b::$t)
                        res = similar(a, $outtype)
                        for i = 1:length(a)
                            res[i] = $(scalarfunc)(a[i], b)
                        end
                        res
                    end)
                ]
                if swappable
                    # For /, Array/Number is valid but not Number/Array
                    # All other operators should be swappable
                    if VERSION < v"0.7.0-DEV.357"
                        map!(x->Expr(:macrocall, Symbol("@swappable"), x, scalarfunc), fns, fns)
                    else
                        map!(x->Expr(:macrocall, Symbol("@swappable"), LineNumberNode(@__LINE__), x, scalarfunc), fns, fns)
                    end
                end
                Expr(:block, fns...)
            end
            for t in (:AbstractString, :Number)
        ]...
    ))
end

# Binary operators with two array arguments
macro dataarray_binary_array(vectorfunc, scalarfunc)
    esc(Expr(:block,
        # DataArray with other array
        Any[
            quote
                function $(vectorfunc)(a::$atype, b::$btype)
                    data1 = $(atype == :DataArray || atype == :(DataArray{Bool}) ? :(a.data) : :a)
                    data2 = $(btype == :DataArray || btype == :(DataArray{Bool}) ? :(b.data) : :b)
                    res = Array{promote_op($vectorfunc, eltype(a), eltype(b))}(
                        promote_shape(size(a), size(b)))
                    resna = $narule
                    @bitenumerate resna i na begin
                        if !na
                            @inbounds res[i] = $(scalarfunc)(data1[i], data2[i])
                        end
                    end
                    DataArray(res, resna)
                end
            end
            for (atype, btype, narule) in ((:(DataArray), :(Range), :(copy(a.na))),
                                           (:(Range), :(DataArray), :(copy(b.na))),
                                           (:DataArray, :DataArray, :(a.na .| b.na)),
                                           (:DataArray, :AbstractArray, :(copy(a.na))),
                                           (:AbstractArray, :DataArray, :(copy(b.na))))
        ]...,
        # AbstractDataArray with other array
        # Definitinons with DataArray necessary to avoid ambiguity
        Any[
            quote
                function $(vectorfunc)(a::$atype, b::$btype)
                    res = similar($(asim ? :a : :b),
                                  promote_op($vectorfunc, eltype(a), eltype(b)),
                                  promote_shape(size(a), size(b)))
                    for i = 1:length(a)
                        res[i] = $(scalarfunc)(a[i], b[i])
                    end
                    res
                end
            end
            for (asim, atype, btype) in ((true, :AbstractDataArray, :Range),
                                         (false, :Range, :AbstractDataArray),
                                         (true, :DataArray, :AbstractDataArray),
                                         (false, :AbstractDataArray, :DataArray),
                                         (true, :AbstractDataArray, :AbstractDataArray),
                                         (true, :AbstractDataArray, :AbstractArray),
                                         (false, :AbstractArray, :AbstractDataArray))
        ]...,
    ))
end

# Unary operators, DataArrays.
@dataarray_unary(+, Any, T)
@dataarray_unary(-, Bool, Int)
@dataarray_unary(-, Any, T)
@dataarray_unary(!, Bool, T)

# Treat ctranspose and * in a special way
for (f, elf) in ((:(Base.ctranspose), :conj), (:(Base.transpose), :identity))
    @eval begin
        function $(f){T}(d::DataMatrix{T})
            # (c)transpose in Base uses a cache-friendly algorithm for
            # numeric arrays, which is faster than our naive algorithm,
            # but chokes on undefined values in the data array.
            # Fortunately, undefined values can only be present in
            # arrays of non-bits types.
            if isbits(T)
                DataArray($(f)(d.data), d.na.')
            else
                data = d.data
                sz = (size(data, 1), size(data, 2))
                res = similar(data, size(data, 2), size(data, 1))
                @bitenumerate d.na i na begin
                    if !na
                        jnew, inew = ind2sub(sz, i)
                        @inbounds res[inew, jnew] = $(elf)(data[i])
                    end
                end
                DataArray(res, d.na.')
            end
        end
    end
end

# Propagates missings
# For a dissenting view,
# http://radfordneal.wordpress.com/2011/05/21/slowing-down-matrix-multiplication-in-r/
# But we're getting 10x R while maintaining missings
for (adata, bdata) in ((true, false), (false, true), (true, true))
    @eval begin
        function (*)(a::$(adata ? :(Union{DataVector, DataMatrix}) : :(Union{Vector, Matrix})),
                           b::$(bdata ? :(Union{DataVector, DataMatrix}) : :(Union{Vector, Matrix})))
            c = $(adata ? :(a.data) : :a) * $(bdata ? :(b.data) : :b)
            res = DataArray(c, falses(size(c)))
            # Propagation can be made more efficient by storing record of corrupt
            # rows and columns, then doing fast edits.
            $(if adata
                quote
                    n1 = size(a, 1)
                    p1 = size(a, 2)
                    corrupt_rows = falses(n1)
                    for j in 1:p1, i in 1:n1
                        # Propagate missings
                        # Corrupt all rows based on i
                        corrupt_rows[i] |= a.na[i, j]
                    end
                    res.na[corrupt_rows, :] = true
                end
            end)
            $(if bdata
                quote
                    n2 = size(b, 1)
                    p2 = size(b, 2)
                    corrupt_cols = falses(p2)
                    for j in 1:p2, i in 1:n2
                        # Propagate missings
                        # Corrupt all columns based on j
                        corrupt_cols[j] |= b.na[i, j]
                    end
                    res.na[:, corrupt_cols] = true
                end
            end)
            res
        end
    end
end

#
# Elementary functions
#
# XXX: The below should be revisited once we have a way to infer what
# the proper return type of an array should be.

# One-argument elementary functions that do something different for
# Complex
for f in (:(Base.abs), :(Base.abs2))
    @eval begin
        @dataarray_unary $(f) Complex T.parameters[1]
    end
end

# One-argument elementary functions that return the same type as their
# inputs
for f in (:(Base.abs), :(Base.abs2), :(Base.conj), :(Base.sign))
    @eval begin
        @dataarray_unary $(f) Number T
    end
end

# One-argument elementary functions that always return floating points
## Base
for f in (:(Base.acos), :(Base.acosh), :(Base.asin), :(Base.asinh), :(Base.atan), :(Base.atanh),
          :(Base.sin), :(Base.sinh), :(Base.cos), :(Base.cosh), :(Base.tan), :(Base.tanh),
          :(Base.exp), :(Base.exp2), :(Base.expm1), :(Base.log), :(Base.log10), :(Base.log1p),
          :(Base.log2), :(Base.exponent), :(Base.sqrt), :(Base.gamma), :(Base.lgamma))
    @eval begin
        @dataarray_unary $(f) AbstractFloat T
        @dataarray_unary $(f) Real Float64
    end
end
## SpecialFunctions (should be a conditional module when supported)
for f in (:(SpecialFunctions.digamma), :(SpecialFunctions.erf), :(SpecialFunctions.erfc))
    @eval begin
        @dataarray_unary $(f) AbstractFloat T
        @dataarray_unary $(f) Real Float64
    end
end

# Elementary functions that take varargs
for f in (:(Base.round), :(Base.ceil), :(Base.floor), :(Base.trunc))
    @eval begin
        # ambiguity
        @dataarray_unary $(f) Real T 1
        @dataarray_unary $(f) Real T 2
        @dataarray_unary $(f) Real T

        function $(f){T<:Real}(d::DataArray{T}, args::Integer...)
            data = similar(d.data)
            @bitenumerate d.na i na begin
                if !na
                    @inbounds data[i] = $(f)(d[i], args...)
                end
            end
            DataArray(data, copy(d.na))
        end
        function $(f){T<:Real}(adv::AbstractDataArray{T}, args::Integer...)
            res = similar(adv)
            for i = 1:length(adv)
                res[i] = ($f)(adv[i], args...)
            end
            res
        end
    end
end

#
# Comparison operators
#

# This is for performance only; the definition in Base is sufficient
# for AbstractDataArrays
function Base.isequal(a::DataArray, b::DataArray)
    if size(a) != size(b) || a.na != b.na
        return false
    end
    @bitenumerate a.na i na begin
        @inbounds if !na && !isequal(a.data[i], b.data[i])
            return false
        end
    end
    return true
end

# ambiguity
@swappable (==)(a::DataArray{Bool}, b::BitArray) =
    invoke(==, Tuple{DataArray,AbstractArray}, a, b)
@swappable (==)(a::DataArray, b::BitArray) =
    invoke(==, Tuple{DataArray,AbstractArray}, a, b)
@swappable (==)(a::AbstractDataArray{Bool}, b::BitArray) =
    invoke(==, Tuple{DataArray,AbstractArray}, a, b)
@swappable (==)(a::AbstractDataArray, b::BitArray) =
    invoke(==, Tuple{DataArray,AbstractArray}, a, b)

function (==)(a::DataArray, b::DataArray)
    size(a) == size(b) || return false
    adata = a.data
    bdata = b.data
    bchunks = b.na.chunks
    has_missing = false
    @bitenumerate a.na i amissing begin
        if amissing || Base.unsafe_bitgetindex(bchunks, i)
            has_missing = true
        else
            @inbounds adata[i] == bdata[i] || return false
        end
    end
    has_missing ? missing : true
end

@swappable function (==)(a::DataArray, b::AbstractArray)
    size(a) == size(b) || return false
    adata = a.data
    has_missing = false
    @bitenumerate a.na i amissing begin
        if amissing
            has_missing = true
        else
            @inbounds adata[i] == b[i] || return false
        end
    end
    has_missing ? missing : true
end

# ambiguity
@swappable (==)(a::DataArray, b::AbstractArray{>:Missing}) =
    invoke(==, Tuple{AbstractDataArray,AbstractArray}, a, b)

#
# Binary operators
#

# Define methods for UniformScaling. Otherwise we get ambiguity
# warnings...
if isdefined(Base, :UniformScaling)

function (+)(A::DataArray{TA,2},J::UniformScaling{TJ}) where {TA,TJ}
    n = LinAlg.checksquare(A)
    B = similar(A,promote_type(TA,TJ))
    copy!(B,A)
    @inbounds for i = 1:n
        if !B.na[i,i]
            B.data[i,i] += J.λ
        end
    end
    B
end
(+)(J::UniformScaling,A::DataArray{TA,2}) where {TA} = A + J

function (-)(A::DataArray{TA,2},J::UniformScaling{TJ}) where {TA,TJ<:Number}
    n = LinAlg.checksquare(A)
    B = similar(A,promote_type(TA,TJ))
    copy!(B,A)
    @inbounds for i = 1:n
        if !B.na[i,i]
            B.data[i,i] -= J.λ
        end
    end
    B
end
function (-)(J::UniformScaling{TJ},A::DataArray{TA,2}) where {TA,TJ<:Number}
    n = LinAlg.checksquare(A)
    B = -A
    @inbounds for i = 1:n
        if !B.na[i,i]
            B.data[i,i] += J.λ
        end
    end
    B
end

(+)(A::DataArray{Bool,2},J::UniformScaling{Bool}) =
    invoke(+, Tuple{AbstractArray{Union{Bool,Missing},2},UniformScaling{Bool}}, A, J)
(+)(J::UniformScaling{Bool},A::DataArray{Bool,2}) =
    invoke(+, Tuple{UniformScaling{Bool},AbstractArray{Union{Bool,Missing},2}}, J, A)
(-)(A::DataArray{Bool,2},J::UniformScaling{Bool}) =
    invoke(-, Tuple{AbstractArray{Union{Bool,Missing},2},UniformScaling{Bool}}, A, J)
(-)(J::UniformScaling{Bool},A::DataArray{Bool,2}) =
    invoke(-, Tuple{UniformScaling{Bool},AbstractArray{Union{Bool,Missing},2}}, J, A)

(+)(A::AbstractDataArray{TA,2},J::UniformScaling{TJ}) where {TA,TJ} =
    invoke(+, Tuple{AbstractArray{Union{TA,Missing},2},UniformScaling{TJ}}, A, J)
(+)(J::UniformScaling,A::AbstractDataArray{TA,2}) where {TA} =
    invoke(+, Tuple{UniformScaling,AbstractArray{Union{TA,Missing},2}}, J, A)
(-)(A::AbstractDataArray{TA,2},J::UniformScaling{TJ}) where {TA,TJ<:Number} =
    invoke(-, Tuple{AbstractArray{Union{TA,Missing},2},UniformScaling{TJ}}, A, J)
(-)(J::UniformScaling{TJ},A::AbstractDataArray{TA,2}) where {TA,TJ<:Number} =
    invoke(-, Tuple{UniformScaling{TJ},AbstractArray{Union{TA,Missing},2}}, J, A)

(+)(A::AbstractDataArray{Bool,2},J::UniformScaling{Bool}) =
    invoke(+, Tuple{AbstractArray{Union{Bool,Missing},2},UniformScaling{Bool}}, A, J)
(+)(J::UniformScaling{Bool},A::AbstractDataArray{Bool,2}) =
    invoke(+, Tuple{UniformScaling{Bool},AbstractArray{Union{Bool,Missing},2}}, J, A)
(-)(A::AbstractDataArray{Bool,2},J::UniformScaling{Bool}) =
    invoke(-, Tuple{AbstractArray{Union{Bool,Missing},2},UniformScaling{Bool}}, A, J)
(-)(J::UniformScaling{Bool},A::AbstractDataArray{Bool,2}) =
    invoke(-, Tuple{UniformScaling{Bool},AbstractArray{Union{Bool,Missing},2}}, J, A)

end # if isdefined(Base, :UniformScaling)

for f in (:(*), :(Base.div), :(Base.mod), :(Base.fld), :(Base.rem))
    @eval begin
        # Array with missing
        @swappable $(f){T,N}(::Missing, b::AbstractArray{T,N}) =
            DataArray(Array{T,N}(size(b)), trues(size(b)))

        # DataArray with scalar
        @dataarray_binary_scalar $f $f nothing true
    end
end

for f in (:(+), :(-))
    # Array with missing
    @eval @swappable $(f){T,N}(::Missing, b::AbstractArray{T,N}) =
        DataArray(Array{T,N}(size(b)), trues(size(b)))
end

for f in (:(+), :(-))
    @eval begin
        # Necessary to avoid ambiguity warnings
        @swappable ($f)(A::BitArray, B::AbstractDataArray{Bool}) = ($f)(Array(A), B)
        @swappable ($f)(A::BitArray, B::DataArray{Bool}) = ($f)(Array(A), B)

        @dataarray_binary_array $f $f
    end
end

# / is defined separately since it is not swappable
(/)(b::AbstractArray{T,N}, ::Missing) where {T,N} =
    DataArray(Array{T,N}(size(b)), trues(size(b)))
@dataarray_binary_scalar(/, /, nothing, false)

function Base.LinAlg.diff(dv::DataVector)
    n = length(dv)
    new_data = diff(dv.data)
    new_na = falses(n - 1)
    new_na[1] = dv.na[1]
    for i = 2:(n - 1)
        if dv.na[i]
            new_na[i - 1] = true
            new_na[i] = true
        end
    end
    new_na[n - 1] = new_na[n - 1] || dv.na[n]
    return DataArray(new_data, new_na)
end

# for f in cumulative_vector_operators
#     @eval function ($f)(dv::DataVector)
#         new_data = ($f)(dv.data)
#         new_na = falses(length(dv))
#         hitna = false
#         @bitenumerate dv.na i na begin
#             hitna |= na
#             if hitna
#                 new_na[i] = true
#             end
#         end
#         return DataArray(new_data, new_na)
#     end
# end
function Base.accumulate(f, dv::DataVector)
    new_data = accumulate(f, dv.data)
    new_na = falses(length(dv))
    hitna = false
    @bitenumerate dv.na i na begin
        hitna |= na
        if hitna
            new_na[i] = true
        end
    end
    return DataArray(new_data, new_na)
end
Base.cumsum(dv::DataVector) = accumulate(+, dv)
Base.cumprod(dv::DataVector)   = accumulate(*, dv)

for f in unary_vector_operators
    @eval ($f)(dv::DataVector) = any(dv.na) ? missing : ($f)(dv.data)
end

for f in binary_vector_operators
    @eval ($f)(dv1::DataVector, dv2::DataVector) =
            any(dv1.na) || any(dv2.na) ? missing : ($f)(dv1.data, dv2.data)
end

for f in (:(Base.minimum), :(Base.maximum), :(Base.prod), :(Base.sum),
          :(Base.mean), :(Base.median), :(Base.std), :(Base.var),
          :(Base.norm))
    colf = Symbol("col$(f)s")
    rowf = Symbol("row$(f)s")
    @eval begin
        function ($colf)(dm::AbstractDataMatrix)
            n, p = nrow(dm), ncol(dm)
            res = datazeros(p)
            for j in 1:p
                res[j] = ($f)(dm[:, j])
            end
            return res
        end
        function ($rowf)(dm::DataMatrix)
            n, p = nrow(dm), ncol(dm)
            res = datazeros(n)
            for i in 1:n
                res[i] = ($f)(DataArray(reshape(dm.data[i, :], p), reshape(dm.na[i, :], p)))
            end
            return res
        end
    end
end

#
# Boolean operators
#

function Base.all(dv::DataArray{Bool})
    data = dv.data
    hasmissings = false
    @bitenumerate dv.na i na begin
        if !na
            data[i] || return false
        else
            hasmissings = true
        end
    end
    hasmissings ? missing : true
end

function Base.all(dv::AbstractDataArray{Bool})
    hasmissings = false
    for i in 1:length(dv)
        x = dv[i]
        if !ismissing(x)
            x || return false
        else
            hasmissings = true
        end
    end
    hasmissings ? missing : true
end

function Base.any(dv::DataArray{Bool})
    hasmissings = false
    @bitenumerate dv.na i na begin
        if !na
            if dv.data[i]
                return true
            end
        else
            hasmissings = true
        end
    end
    hasmissings ? missing : false
end

function Base.any(dv::AbstractDataArray{Bool})
    hasmissings = false
    for i in 1:length(dv)
        if !ismissing(dv[i])
            if dv[i]
                return true
            end
        else
            hasmissings = true
        end
    end
    hasmissings ? missing : false
end

function rle(v::AbstractVector{T}) where T
    n = length(v)
    current_value = v[1]
    current_length = 1
    values = similar(v, n)
    total_values = 1
    lengths = Array(Int16, n)
    total_lengths = 1
    for i in 2:n
        if v[i] == current_value
            current_length += 1
        else
            values[total_values] = current_value
            total_values += 1
            lengths[total_lengths] = current_length
            total_lengths += 1
            current_value = v[i]
            current_length = 1
        end
    end
    values[total_values] = current_value
    lengths[total_lengths] = current_length
    return (values[1:total_values], lengths[1:total_lengths])
end

function rle(v::AbstractDataVector{T}) where T
    n = length(v)
    current_value = v[1]
    current_length = 1
    values = DataArray(T, n)
    total_values = 1
    lengths = Vector{Int16}(n)
    total_lengths = 1
    for i in 2:n
        if ismissing(v[i]) || ismissing(current_value)
            if ismissing(v[i]) && ismissing(current_value)
                current_length += 1
            else
                values[total_values] = current_value
                total_values += 1
                lengths[total_lengths] = current_length
                total_lengths += 1
                current_value = v[i]
                current_length = 1
            end
        else
            if v[i] == current_value
                current_length += 1
            else
                values[total_values] = current_value
                total_values += 1
                lengths[total_lengths] = current_length
                total_lengths += 1
                current_value = v[i]
                current_length = 1
            end
        end
    end
    values[total_values] = current_value
    lengths[total_lengths] = current_length
    return (values[1:total_values], lengths[1:total_lengths])
end

## inverse run-length encoding
function inverse_rle(values::AbstractVector{T},
                     lengths::Vector{I}) where {T, I <: Integer}
    total_n = Int(sum(lengths))
    pos = 0
    res = similar(values, total_n)
    n = length(values)
    for i in 1:n
        v = values[i]
        l = lengths[i]
        for j in 1:l
            pos += 1
            res[pos] = v
        end
    end
    return res
end
