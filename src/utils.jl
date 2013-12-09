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

# Enumerate a BitArray. This is faster than using enumerate()
macro bitenumerate(ba, i, x, code)
    esc(quote
        $i = 1
        for $x in $ba
            $code
            $i += 1
        end
    end)
end
