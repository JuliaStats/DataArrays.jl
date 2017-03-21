using Base: @deprecate, depwarn

# Deprecate in Julia 0.6 cycle
function Base.isnan(da::DataArray)
    depwarn("vectorized method isnan(da) is deprecated, use isnan.(da) instead", :isnan)
    return isnan.(da)
end

@deprecate isna(x::AbstractArray) isna.(x)
@deprecate anyna(x) any(isna, x)
@deprecate allna(x) all(isna, x)
@deprecate padNA(dv::AbstractDataVector, front::Integer, back::Integer) padna(dv, front, back)
