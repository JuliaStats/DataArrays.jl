const unary_operators = [:(Base.(:+)),
                         :(Base.(:-)),
                         :(Base.(:!)),
                         :(Base.(:*))]

const numeric_unary_operators = [:(Base.(:+)),
                                 :(Base.(:-))]

const logical_unary_operators = [:(Base.(:!))]

const elementary_functions = [:(Base.abs),
                              :(Base.sign),
                              :(Base.acos),
                              :(Base.acosh),
                              :(Base.asin),
                              :(Base.asinh),
                              :(Base.atan),
                              :(Base.atanh),
                              :(Base.sin),
                              :(Base.sinh),
                              :(Base.cos),
                              :(Base.cosh),
                              :(Base.tan),
                              :(Base.tanh),
                              :(Base.ceil),
                              :(Base.floor),
                              :(Base.round),
                              :(Base.trunc),
                              :(Base.exp),
                              :(Base.exp2),
                              :(Base.expm1),
                              :(Base.log),
                              :(Base.log10),
                              :(Base.log1p),
                              :(Base.log2),
                              :(Base.exponent),
                              :(Base.sqrt),
                              :(Base.gamma),
                              :(Base.lgamma),
                              :(Base.digamma),
                              :(Base.erf),
                              :(Base.erfc)]

const two_argument_elementary_functions = [:(Base.round),
                                           :(Base.ceil),
                                           :(Base.floor),
                                           :(Base.trunc)]

const special_comparison_operators = [:(Base.isless)]

const comparison_operators = [:(Base.(:(==))),
                              :(Base.(:(.==))),
                              :(Base.(:(!=))),
                              :(Base.(:(.!=))),
                              :(Base.(:(>))),
                              :(Base.(:(.>))),
                              :(Base.(:(>=))),
                              :(Base.(:(.>=))),
                              :(Base.(:(<))),
                              :(Base.(:(.<))),
                              :(Base.(:(<=))),
                              :(Base.(:(.<=)))]

const scalar_comparison_operators = [:(Base.(:(==))),
                                     :(Base.(:(!=))),
                                     :(Base.(:(>))),
                                     :(Base.(:(>=))),
                                     :(Base.(:(<))),
                                     :(Base.(:(<=)))]

const array_comparison_operators = [:(Base.(:(.==))),
                                    :(Base.(:(.!=))),
                                    :(Base.(:(.>))),
                                    :(Base.(:(.>=))),
                                    :(Base.(:(.<))),
                                    :(Base.(:(.<=)))]

const vectorized_comparison_operators = [:(Base.(:(.==))),
                                         :(Base.(:(==))),
                                         :(Base.(:(.!=))),
                                         :(Base.(:(!=))),
                                         :(Base.(:(.>))),
                                         :(Base.(:(>))),
                                         :(Base.(:(.>=))),
                                         :(Base.(:(>=))),
                                         :(Base.(:(.<))),
                                         :(Base.(:(<))),
                                         :(Base.(:(.<=))),
                                         :(Base.(:(<=)))]

const binary_operators = [:(Base.(:+)),
                          :(Base.(:.+)),
                          :(Base.(:-)),
                          :(Base.(:.-)),
                          :(Base.(:*)),
                          :(Base.(:.*)),
                          :(Base.(:/)),
                          :(Base.(:./)),
                          :(Base.(:.^)),
                          :(Base.div),
                          :(Base.mod),
                          :(Base.fld),
                          :(Base.rem)]

const induced_binary_operators = [:(Base.(:^))]

const arithmetic_operators = [:(Base.(:+)),
                              :(Base.(:.+)),
                              :(Base.(:-)),
                              :(Base.(:.-)),
                              :(Base.(:*)),
                              :(Base.(:.*)),
                              :(Base.(:/)),
                              :(Base.(:./)),
                              :(Base.(:.^)),
                              :(Base.div),
                              :(Base.mod),
                              :(Base.fld),
                              :(Base.rem)]

const induced_arithmetic_operators = [:(Base.(:^))]

const biscalar_operators = [:(Base.max),
                            :(Base.min)]

const scalar_arithmetic_operators = [:(Base.(:+)),
                                     :(Base.(:-)),
                                     :(Base.(:*)),
                                     :(Base.(:/)),
                                     :(Base.div),
                                     :(Base.mod),
                                     :(Base.fld),
                                     :(Base.rem)]

const induced_scalar_arithmetic_operators = [:(Base.(:^))]

const array_arithmetic_operators = [:(Base.(:+)),
                                    :(Base.(:.+)),
                                    :(Base.(:-)),
                                    :(Base.(:.-)),
                                    :(Base.(:.*)),
                                    :(Base.(:.^))]

const bit_operators = [:(Base.(:&)),
                       :(Base.(:|)),
                       :(Base.(:$))]

const unary_vector_operators = [:(Base.min),
                                :(Base.max),
                                :(Base.prod),
                                :(Base.sum),
                                :(Base.mean),
                                :(Base.median),
                                :(Base.std),
                                :(Base.var),
                                :(Stats.mad),
                                :(Base.norm),
                                :(Stats.skewness),
                                :(Stats.kurtosis)]

# TODO: dist, iqr, rle, inverse_rle

const pairwise_vector_operators = [:(Base.diff)]
                                   #:(Base.reldiff),
                                   #:(Base.percent_change)]

const cumulative_vector_operators = [:(Base.cumprod),
                                     :(Base.cumsum),
                                     :(Base.cumsum_kbn),
                                     :(Base.cummin),
                                     :(Base.cummax)]

const ffts = [:(Base.fft)]

const binary_vector_operators = [:(Base.dot),
                                 :(Base.cor),
                                 :(Base.cov),
                                 :(Stats.cor_spearman)]

const rowwise_operators = [:rowmins,
                           :rowmaxs,
                           :rowprods,
                           :rowsums,
                           :rowmeans,
                           :rowmedians,
                           :rowstds,
                           :rowvars,
                           :rowffts,
                           :rownorms]

const columnar_operators = [:colmins,
                            :colmaxs,
                            :colprods,
                            :colsums,
                            :colmeans,
                            :colmedians,
                            :colstds,
                            :colvars,
                            :colffts,
                            :colnorms]

const boolean_operators = [:(Base.any),
                           :(Base.all)]

# Swap arguments to fname() anywhere in AST. Returns the number of
# arguments swapped
function swapargs(ast::Expr, fname::Union(Expr, Symbol))
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
function swapargs(ast, fname::Union(Expr, Symbol))
    ast
    0
end

# Return a block consisting of both the given function and a copy of
# the function in which arguments to the function itself and any
# 2-argument calls to a function of the same name are swapped
macro swappable(func, syms...)
    if (func.head != :function && func.head != :(=)) ||
       func.args[1].head != :call || length(func.args[1].args) != 3
        error("@swappable may only be applied to functions of two arguments")
    end
    
    func2 = deepcopy(func)
    fname = func2.args[1].args[1]
    if isa(fname, Expr) && fname.head == :curly
        fname = fname.args[1]
    end

    for s in unique([fname, syms...])
        if swapargs(func2, s) < 1
            error("No argument swapped")
        end
    end
    esc(Expr(:block, func, func2))
end

#
# Unary operator macros for DataArrays
#

# Apply unary operator to non-NA members of a DataArray or
# AbstractDataArray
macro dataarray_unary(f, intype, outtype)
    esc(quote
        function $(f){T<:$(intype)}(d::DataArray{T})
            data = similar(d.data, $(outtype))
            for i = 1:length(data)
                if !d.na[i]
                    data[i] = $(f)(d.data[i])
                end
            end
            DataArray(data, copy(d.na))
        end
        function $(f){T<:$(intype)}(adv::AbstractDataArray{T})
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
macro dataarray_binary_scalar(vectorfunc, scalarfunc, outtype)
    esc(Expr(:block,
        # DataArray and AbstractDataArray with scalar
        # XXX It would be really nice to make this work with arbitrary
        # types, but doing so results in a bunch of method ambiguity
        # warnings
        {
            quote
                @swappable function $(vectorfunc)(a::DataArray, b::$t)
                    res = DataArray(similar(a.data, $outtype), copy(a.na))
                    for i = 1:length(a)
                        if !res.na[i]
                            res.data[i] = $(scalarfunc)(a.data[i], b)
                        end
                    end
                    res
                end $scalarfunc
                @swappable function $(vectorfunc)(a::AbstractDataArray, b::$t)
                    res = similar(a, $outtype)
                    for i = 1:length(a)
                        res[i] = $(scalarfunc)(a[i], b)
                    end
                    res
                end $scalarfunc
            end
            for t in (:String, :Number)
        }...
    ))
end

# Binary operators with two array arguments
macro dataarray_binary_array(vectorfunc, scalarfunc, outtype)
    esc(Expr(:block,
        # DataArray with other array
        {
            quote
                function $(vectorfunc)(a::$(adata ? :DataArray : :AbstractArray),
                                       b::$(bdata ? :DataArray : :AbstractArray))
                    res = DataArray(Array($outtype, promote_shape(size(a), size(b))), $narule)
                    for i = 1:length(res)
                        if !res.na[i]
                            res.data[i] = $(scalarfunc)($(adata ? :(a.data) : :a)[i],
                                                        $(bdata ? :(b.data) : :b)[i])
                        end
                    end
                    res
                end
            end
            for (adata, bdata, narule) in ((true, true, :(a.na | b.na)),
                                           (true, false, :(copy(a.na))),
                                           (false, true, :(copy(b.na))))
        }...,
        # AbstractDataArray with other array
        # Definitinons with DataArray necessary to avoid ambiguity
        {
            quote
                function $(vectorfunc)(a::$atype, b::$btype)
                    res = similar($(asim ? :a : :b), $outtype, promote_shape(size(a), size(b)))
                    for i = 1:length(a)
                        res[i] = $(scalarfunc)(a[i], b[i])
                    end
                    res
                end
            end
            for (asim, atype, btype) in ((true, :DataArray, :AbstractDataArray),
                                         (false, :AbstractDataArray, :DataArray),
                                         (true, :AbstractDataArray, :AbstractDataArray),
                                         (true, :AbstractDataArray, :AbstractArray),
                                         (false, :AbstractArray, :AbstractDataArray))
        }...,
    ))
end

# Unary operators, NA
for f in unary_operators
    @eval $(f)(d::NAtype) = NA
end

# Unary operators, DataArrays. Definitions in base should be adequate
# for AbstractDataArrays. These are just optimizations
Base.(:!)(d::DataArray{Bool}) = DataArray(!d.data, copy(d.na))
Base.(:-)(d::DataArray) = DataArray(-d.data, copy(d.na))

# Treat ctranspose and * in a special way for now
for f in (:(Base.ctranspose), :(Base.transpose))
    @eval $(f)(d::DataArray) = DataArray($(f)(d.data), d.na')
end

# Propagates NA's
# For a dissenting view,
# http://radfordneal.wordpress.com/2011/05/21/slowing-down-matrix-multiplication-in-r/
# But we're getting 10x R while maintaining NA's
for (adata, bdata) in ((true, false), (false, true), (true, true))
    @eval begin
        function Base.(:*)(a::$(adata ? :(Union(DataVector, DataMatrix)) : :(Union(Vector, Matrix))),
                           b::$(bdata ? :(Union(DataVector, DataMatrix)) : :(Union(Vector, Matrix))))
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
                        # Propagate NA's
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
                        # Propagate NA's
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

# One-argument elementary functions that return the same type as their
# inputs
for f in (:(Base.abs), :(Base.sign))
    @eval begin
        $(f)(::NAtype) = NA
        @dataarray_unary $(f) Number T
    end
end

# One-argument elementary functions that always return floating points
for f in (:(Base.acos), :(Base.acosh), :(Base.asin), :(Base.asinh), :(Base.atan), :(Base.atanh),
          :(Base.sin), :(Base.sinh), :(Base.cos), :(Base.cosh), :(Base.tan), :(Base.tanh),
          :(Base.ceil), :(Base.floor), :(Base.round), :(Base.trunc), :(Base.exp), :(Base.exp2),
          :(Base.expm1), :(Base.log), :(Base.log10), :(Base.log1p), :(Base.log2), :(Base.exponent),
          :(Base.sqrt), :(Base.gamma), :(Base.lgamma), :(Base.digamma), :(Base.erf), :(Base.erfc))
    @eval begin
        ($f)(::NAtype) = NA
        @dataarray_unary $(f) FloatingPoint T
        @dataarray_unary $(f) Real Float64
    end
end

# Elementary functions that take varargs
for f in (:(Base.round), :(Base.ceil), :(Base.floor), :(Base.trunc))
    @eval begin
        ($f)(::NAtype, args::Integer...) = NA

        function $(f){T<:Number}(d::DataArray{T}, args::Integer...)
            data = similar(d.data)
            for i = 1:length(data)
                if !d.na[i]
                    data[i] = $(f)(d[i], args...)
                end
            end
            DataArray(data, copy(d.na))
        end
        function $(f){T<:Number}(adv::AbstractDataArray{T}, args::Integer...)
            res = similar(adv)
            for i = 1:length(adv)
                res[i] = ($f)(adv[i], args...)
            end
            res
        end
    end
end

#
# Bit operators
#

@swappable Base.(:&)(a::NAtype, b::Bool) = b ? NA : false
@swappable Base.(:|)(a::NAtype, b::Bool) = b ? true : NA
@swappable Base.(:$)(a::NAtype, b::Bool) = NA

# To avoid ambiguity warning
@swappable Base.(:|)(a::NAtype, b::Function) = NA

for f in (:(Base.(:&)), :(Base.(:|)), :(Base.(:$)))
    @eval begin
        # Scalar with NA
        ($f)(::NAtype, ::NAtype) = NA
        @swappable ($f)(::NAtype, b) = NA
    end
end

# DataArray with DataArray
Base.(:&)(a::DataArray{Bool}, b::DataArray{Bool}) =
    DataArray(a.data & b.data, (a.na & b.data) | (b.na & a.data))
Base.(:|)(a::DataArray{Bool}, b::DataArray{Bool}) =
    DataArray(a.data | b.data, (a.na & !b.data) | (b.na & !a.data))
Base.(:$)(a::DataArray{Bool}, b::DataArray{Bool}) =
    DataArray(a.data $ b.data, a.na | b.na)

# DataArray with non-DataArray
# Need explicit definition for BitArray to avoid ambiguity
for t in (:(BitArray), :(Union(AbstractArray{Bool}, Bool)))
    @eval begin
        @swappable Base.(:&)(a::DataArray{Bool}, b::$t) = DataArray(a.data & b, a.na & b)
        @swappable Base.(:|)(a::DataArray{Bool}, b::$t) = DataArray(a.data | b, a.na & !b)
        @swappable Base.(:$)(a::DataArray{Bool}, b::$t) = DataArray(a.data $ b, a.na)
    end
end

#
# Comparison operators
#

Base.isless(::NAtype, ::NAtype) = false
Base.isless(::NAtype, b) = true
Base.isless(a, ::NAtype) = false

# This is for performance only; the definition in Base is sufficient
# for AbstractDataArrays
function Base.isequal(a::DataArray, b::DataArray)
    if size(a) != size(b) || a.na != b.na
        return false
    end
    for i = 1:length(a)
        if !a.na[i] && a.data[i] != b.data[i]
            return false
        end
    end
    return true
end

for (sf,vf) in zip(scalar_comparison_operators, array_comparison_operators)
    #vf = symbol(".$sf")

    @eval begin
        # Array with NA
        @swappable ($(vf)){T,N}(::NAtype, b::AbstractArray{T,N}) =
            DataArray(Array(Bool, size(b)), trues(size(b)))

        # Scalar with NA
        ($(vf))(::NAtype, ::NAtype) = NA
        ($(sf))(::NAtype, ::NAtype) = NA
        @swappable ($(vf))(::NAtype, b) = NA
        @swappable ($(sf))(::NAtype, b) = NA

        @dataarray_binary_scalar $(vf) $(sf) Bool
        @dataarray_binary_array $(vf) $(sf) Bool

        $(sf)(a::AbstractDataArray, b::AbstractDataArray) =
            error("$sf not defined for AbstractDataArrays. Try $vf")
    end
end

#
# Binary operators
#

# Necessary to avoid ambiguity warnings
Base.(:.^)(::MathConst{:e}, B::DataArray) = exp(B)
Base.(:.^)(::MathConst{:e}, B::AbstractDataArray) = exp(B)

for f in arithmetic_operators
    @eval begin
        # Array with NA
        @swappable $(f){T,N}(::NAtype, b::AbstractArray{T,N}) =
            DataArray(Array(T, size(b)), trues(size(b)))

        # Scalar with NA
        ($f)(::NAtype, ::NAtype) = NA
        @swappable ($f)(d::NAtype, x::Number) = NA

        # DataArray with scalar
        @dataarray_binary_scalar $f $f promote_type(eltype(a), eltype(b))
    end
end

Base.(:^)(::NAtype, ::NAtype) = NA
Base.(:^)(a, ::NAtype) = NA
Base.(:^)(::NAtype, ::Integer) = NA
Base.(:^)(::NAtype, ::Number) = NA

for (vf, sf) in ((:(Base.(:+)), :(Base.(:+))),
                 (:(Base.(:.+)), :(Base.(:+))),
                 (:(Base.(:-)), :(Base.(:-))),
                 (:(Base.(:.-)), :(Base.(:-))),
                 (:(Base.(:.*)), :(Base.(:*))),
                 (:(Base.(:.^)), :(Base.(:^))))
    @eval begin
        # Necessary to avoid ambiguity warnings
        @swappable ($vf)(A::BitArray, B::AbstractDataArray) = ($vf)(bitunpack(A), B)
        @swappable ($vf)(A::BitArray, B::DataArray) = ($vf)(bitunpack(A), B)

        @dataarray_binary_array $vf $sf promote_type(eltype(a), eltype(b))
    end
end

@swappable Base.(:./)(A::BitArray, B::AbstractDataArray) = ./(bitunpack(A), B)
@swappable Base.(:./)(A::BitArray, B::DataArray) = ./(bitunpack(A), B)

@dataarray_binary_array Base.(:./) Base.(:/) isa(a, FloatingPoint) || isa(b, FloatingPoint) ?
                                             promote_type(a, b) : Float64

for f in biscalar_operators
    @eval begin
        ($f)(::NAtype, ::NAtype) = NA
        @swappable $(f)(::Number, ::NAtype) = NA
    end
end

for f in pairwise_vector_operators
    @eval function ($f)(dv::DataVector)
        n = length(dv)
        new_data = ($f)(dv.data)
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
end

for f in cumulative_vector_operators
    @eval function ($f)(dv::DataVector)
        new_data = ($f)(dv.data)
        new_na = falses(length(dv))
        hitna = false
        for i = 1:length(dv)
            if dv.na[i]
                hitna = true
            end
            if hitna
                new_na[i] = true
            end
        end
        return DataArray(new_data, new_na)
    end
end

for f in [unary_vector_operators; ffts]
    @eval ($f)(dv::DataVector) = any(dv.na) ? NA : ($f)(dv.data)
end

for f in binary_vector_operators
    @eval ($f)(dv1::DataVector, dv2::DataVector) =
            any(dv1.na) || any(dv2.na) ? NA : ($f)(dv1.data, dv2.data)
end

for f in (:(Base.min), :(Base.max), :(Base.prod), :(Base.sum), :(Base.mean), :(Base.median),
          :(Base.std), :(Base.var), :(Base.norm))
    colf = symbol("col$(f)s")
    rowf = symbol("row$(f)s")
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

function Base.all(dv::AbstractDataArray{Bool})
    for i in 1:length(dv)
        if dv.na[i]
            return NA
        end
        if !dv.data[i]
            return false
        end
    end
    true
end

function Base.all(dv::AbstractDataArray{Bool})
    for i in 1:length(dv)
        if isna(dv[i])
            return NA
        end
        if !dv[i]
            return false
        end
    end
    true
end

function Base.any(dv::DataArray{Bool})
    has_na = false
    for i in 1:length(dv)
        if !dv.na[i]
            if dv.data[i]
                return true
            end
        else
            has_na = true
        end
    end
    has_na ? NA : false
end

function Base.any(dv::AbstractDataArray{Bool})
    has_na = false
    for i in 1:length(dv)
        if !isna(dv[i])
            if dv[i]
                return true
            end
        else
            has_na = true
        end
    end
    has_na ? NA : false
end

Stats.range{T}(dv::AbstractDataVector{T}) = max(dv) - min(dv)

function rle{T}(v::AbstractVector{T})
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

function rle{T}(v::AbstractDataVector{T})
    n = length(v)
    current_value = v[1]
    current_length = 1
    values = DataArray(T, n)
    total_values = 1
    lengths = Array(Int16, n)
    total_lengths = 1
    for i in 2:n
        if isna(v[i]) || isna(current_value)
            if isna(v[i]) && isna(current_value)
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
function inverse_rle{T, I <: Integer}(values::AbstractVector{T},
                                      lengths::Vector{I})
    total_n = sum(lengths)
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
