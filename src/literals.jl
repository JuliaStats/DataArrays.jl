function fixargs(args::Vector{Any}, stub::Any)
    n = length(args)
    data = Array(Any, n)
    na = Array(Any, n)
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
    error("Type of literal data cannot be inferred")
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
    error("Type of literal data cannot be inferred")
end

function parsevector(ex::Expr)
    stub = findstub_vector(ex)
    data, na = fixargs(ex.args, stub)
    return Expr(:vcat, data...), Expr(:vcat, na...)
end

function parsematrix(ex::Expr)
    stub = findstub_matrix(ex)
    nrows = length(ex.args)
    datarows = Array(Expr, nrows)
    narows = Array(Expr, nrows)
    for row in 1:nrows
        data, na = fixargs(ex.args[row].args, stub)
        datarows[row] = Expr(:row, data...)
        narows[row] = Expr(:row, na...)
    end
    return Expr(:vcat, datarows...), Expr(:vcat, narows...)
end

function parsedata(ex::Expr)
    if length(ex.args) == 0
        return :([]), Expr(:call, :Array, :Bool, 0)
    end
    if isa(ex.args[1], Expr) && ex.args[1].head == :row
        return parsematrix(ex)
    else
        return parsevector(ex)
    end
end

macro data(ex)
    if ex.head != :vcat && ex.head != :hcat
        return quote
            tmp = $(esc(ex))
            DataArray(tmp, bitbroadcast(x->isequal(x, NA), tmp))
        end
    end
    dataexpr, naexpr = parsedata(ex)
    return Expr(:call, :DataArray, esc(dataexpr), esc(naexpr))
end

macro pdata(ex)
    if ex.head != :vcat && ex.head != :hcat
        return quote
            tmp = $(esc(ex))
            PooledDataArray(tmp, bitbroadcast(x->isequal(x, NA), tmp))
        end
    end
    dataexpr, naexpr = parsedata(ex)
    return Expr(:call, :PooledDataArray, esc(dataexpr), esc(naexpr))
end
