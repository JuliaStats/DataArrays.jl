# iterate over vals array to find the most generic non-NA type
function typeloop(vals)
    toptype = None
    for i = 1:length(vals)
        if vals[i] != :NA
            toptype = promote_type(toptype, typeof(vals[i]))
        end
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
