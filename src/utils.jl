# Default values for unspecified objects
baseval{T <: String}(s::Type{T}) = ""
baseval(x::Any) = zero(x)

# iterate over vals array to find the most generic non-NA type
function typeloop(vals)
    toptype = None
    for i = 1:length(vals)
        if vals[i] != :NA
            toptype = promote_type(toptype, typeof(vals[i]))
        end
    end
    if !method_exists(baseval, (toptype, ))
        error("No baseval exists for type: $(toptype)")
    end
    return toptype
end
