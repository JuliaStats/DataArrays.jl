function fixargs(args::Vector{Any}, stub::Any)
    n = length(args)
    data = Array(Any, n)
    na = BitArray(n)
    for i in 1:n
        if args[i] == :NA
            data[i] = stub
            na[i] = true
        else
            data[i] = args[i]
            na[i] = false
        end
    end
    return data, na
end

# We assume that data has at least one "value" that isn't NA
function findstub_vector(ex::Expr)
    if ex.args[1] != :NA
        return ex.args[1]
    end
    for i in 2:length(ex.args)
        if ex.args[i] != :NA
            return ex.args[i]
        end
    end
    return NA
end

# We assume that data has at least one "value" that isn't NA
function findstub_matrix(ex::Expr)
    if ex.args[1].args[1] != :NA
        return ex.args[1].args[1]
    end
    nrows = length(ex.args)
    for row in 1:nrows
        subex = ex.args[row]
        for i in 1:length(subex.args)
            if subex.args[i] != :NA
                return subex.args[i]
            end
        end
    end
    return NA
end

function parsevector(ex::Expr)
    if ex.head in (:ref, :typed_hcat, :typed_vcat)
        data, na = fixargs(ex.args[2:end], :(zero($(ex.args[1]))))
        return Expr(ex.head, ex.args[1], data...), ex.head == :typed_hcat ? na' : na
    else
        stub = findstub_vector(ex)
        data, na = fixargs(ex.args, stub)
        if ex.head == :hcat
            na = na'
        end

        if isequal(stub, NA)
            return Expr(ex.head == :hcat ? (:typed_hcat) : (:ref), Any, data...), na
        else
            return Expr(ex.head, data...), na
        end
    end
end

function parsematrix(ex::Expr)
    if ex.head == :typed_vcat
        stub = :(zero($(ex.args[1])))
        rows = 2:length(ex.args)
    else
        stub = findstub_matrix(ex)
        rows = 1:length(ex.args)
    end

    nrows = length(rows)
    datarows = Array(Expr, nrows)
    narows = Array(Expr, nrows)
    for irow in 1:nrows
        data, na = fixargs(ex.args[rows[irow]].args, stub)
        datarows[irow] = Expr(:row, data...)
        narows[irow] = Expr(:row, na...)
    end
    if ex.head == :typed_vcat
        return Expr(:typed_vcat, ex.args[1], datarows...), Expr(:vcat, narows...)
    elseif isequal(stub, NA)
        return Expr(:typed_vcat, Any, datarows...), Expr(:vcat, narows...)
    else
        return Expr(:vcat, datarows...), Expr(:vcat, narows...)
    end
end

function parsedata(ex::Expr)
    if length(ex.args) == 0
        return :([]), Expr(:call, :Array, :Bool, 0)
    end
    if ex.head == :typed_vcat || (isa(ex.args[1], Expr) && ex.args[1].head == :row)
        return parsematrix(ex)
    else
        return parsevector(ex)
    end
end

macro data(ex)
    if !(ex.head in (:vect, :vcat, :hcat, :ref, :typed_vcat, :typed_hcat))
        return quote
            tmp = $(esc(ex))
            DataArray(tmp, bitbroadcast(x->isequal(x, NA), tmp))
        end
    end
    dataexpr, naexpr = parsedata(ex)
    return Expr(:call, :DataArray, esc(dataexpr), esc(naexpr))
end

macro pdata(ex)
    if !(ex.head in (:vect, :vcat, :hcat, :ref, :typed_vcat, :typed_hcat))
        return quote
            tmp = $(esc(ex))
            PooledDataArray(tmp, bitbroadcast(x->isequal(x, NA), tmp))
        end
    end
    dataexpr, naexpr = parsedata(ex)
    return Expr(:call, :PooledDataArray, esc(dataexpr), esc(naexpr))
end
