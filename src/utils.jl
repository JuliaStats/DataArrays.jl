##############################################################################
##
## Default values for unspecified objects
##
## Sometimes needed when dealing with NA's for which some value must exist in
## the underlying data vector
##
##############################################################################

baseval{T <: String}(s::Type{T}) = ""
baseval(x::Any) = zero(x)

function ancestors(t::Type)
    a = {t}
    while t != Any
        t = super(t)
        push!(a, t)
    end
    return a
end

function common_ancestors(s::Type, t::Type)
    return filter(e -> (e in ancestors(s)), ancestors(t))
end

earliest_common_ancestor(s::Type, t::Type) = first(common_ancestors(s, t))

# Need to do type inference
# Like earliest_common_ancestor, but ignores NA
# TODO: Deprecate this code path
function _dv_most_generic_type(vals)
    # iterate over vals tuple to find the most generic non-NA type
    toptype = None
    for i = 1:length(vals)
        if !isna(vals[i])
            toptype = promote_type(toptype, typeof(vals[i]))
        end
    end
    if !method_exists(baseval, (toptype, ))
        error("No baseval exists for type: $(toptype)")
    end
    return toptype
end

function typeloop(vals)
    # iterate over vals tuple to find the most generic non-NA type
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
