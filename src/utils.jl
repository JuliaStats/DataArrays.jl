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
