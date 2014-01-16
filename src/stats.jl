function Base.mean{T <: Real}(da::DataArray{T};
                              skipna::Bool = false)
    s, n = 0.0, 0
    for i in 1:length(da)
        if da.na[i]
            if !skipna
                throw(NAException())
            end
        else
            s += da.data[i]
            n += 1
        end
    end
    return s / n
end

function Base.median{T <: Real}(da::DataArray{T};
                                skipna::Bool = false)
    if !skipna
        return median(array(da))
    else
        return median(removeNA(da))
    end
end

function Base.var{T <: Real}(da::DataArray{T};
                             skipna::Bool = false)
    s, n = 0.0, 0
    m = mean(da, skipna = skipna)
    for i in 1:length(da)
        if da.na[i]
            if !skipna
                throw(NAException())
            end
        else
            z = (da.data[i] - m)
            s += z * z
            n += 1
        end
    end
    return s / (n - 1)
end

function Base.std{T <: Real}(da::DataArray{T};
                             skipna::Bool = false)
    s, n = 0.0, 0
    m = mean(da, skipna = skipna)
    for i in 1:length(da)
        if da.na[i]
            if !skipna
                throw(NAException())
            end
        else
            z = (da.data[i] - m)
            s += z * z
            n += 1
        end
    end
    return sqrt(s / (n - 1))
end

function Base.minimum{T <: Real}(da::DataArray{T};
                                 skipna::Bool = false)
    m = typemax(T)
    for i in 1:length(da)
        if da.na[i]
            if !skipna
                throw(NAException())
            end
        else
            m = min(m, da.data[i])
        end
    end
    return m
end

function Base.maximum{T <: Real}(da::DataArray{T};
                                 skipna::Bool = false)
    m = typemin(T)
    for i in 1:length(da)
        if da.na[i]
            if !skipna
                throw(NAException())
            end
        else
            m = max(m, da.data[i])
        end
    end
    return m
end

function Base.prod{T <: Real}(da::DataArray{T};
                              skipna::Bool = false)
    r = one(T)
    for i in 1:length(da)
        if da.na[i]
            if !skipna
                throw(NAException())
            end
        else
            r *= da.data[i]
        end
    end
    return r
end

function Base.sum{T <: Real}(da::DataArray{T};
                             skipna::Bool = false)
    r = zero(T)
    for i in 1:length(da)
        if da.na[i]
            if !skipna
                throw(NAException())
            end
        else
            r += da.data[i]
        end
    end
    return r
end

function Stats.skewness{T <: Real}(da::DataArray{T};
                                   skipna::Bool = false,
                                   m::Real = mean(da, skipna = skipna))
    n = 0
    cm2 = 0.0   # empirical 2nd centered moment (variance)
    cm3 = 0.0   # empirical 3rd centered moment
    for i in 1:length(da)
        if da.na[i]
            if !skipna
                throw(NAException())
            end
        else
            x_i = da.data[i]
            z = x_i - m
            z2 = z * z
            cm2 += z2
            cm3 += z2 * z
            n += 1
        end
    end
    cm3 /= n
    cm2 /= n
    return cm3 / (cm2^1.5)
end

function Stats.kurtosis{T <: Real}(da::DataArray{T};
                                   skipna::Bool = false,
                                   m::Real = mean(da, skipna = skipna))
    n = 0
    cm2 = 0.0  # empirical 2nd centered moment (variance)
    cm4 = 0.0  # empirical 4th centered moment
    for i in 1:length(da)
        if da.na[i]
            if !skipna
                throw(NAException())
            end
        else
            x_i = da.data[i]
            z = x_i - m
            z2 = z * z
            cm2 += z2
            cm4 += z2 * z2
            n += 1
        end
    end
    cm4 /= n
    cm2 /= n
    return (cm4 / (cm2^2)) - 3.0
end
