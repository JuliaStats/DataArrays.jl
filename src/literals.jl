# NB: Can't easily make arrays of Expr or arrays of symbols that
# look like NA.

function parsedata(ex::Expr)
	n = length(ex.args)
	if ex.head == :vcat
		if typeof(ex.args[1]) != Expr
			restype = DataArrays.typeloop(ex.args)
			data = Array(restype, n)
			na = BitArray(n)
			for i in 1:n
				if ex.args[i] == :NA
					data[i] = zero(restype)
					na[i] = true
				else
					data[i] = ex.args[i]
					na[i] = false
				end
			end
		else
			p = length(ex.args[1].args)
			restype = typeloop(ex.args[1].args)
			data = Array(restype, n, p)
			na = BitArray(n, p)
			for i in 1:n
				for j in 1:n
					if ex.args[i].args[j] == :NA
						data[i, j] = zero(restype)
						na[i, j] = true
					else
						data[i, j] = ex.args[i].args[j]
						na[i, j] = false
					end
				end
			end
		end
		return data, na
	else
		return nothing, nothing
	end
end

function literaldata(ex::Expr)
	d, n = parsedata(ex)
	if n == nothing
		return DataArray(eval(ex))
	else
		return DataArray(d, n)
	end
end

function literalpdata(ex::Expr)
	d, n = parsedata(ex)
	if n == nothing
		return PooledDataArray(eval(ex))
	else
		return PooledDataArray(d, n)
	end
end

macro data(ex)
	return Expr(:call, :literaldata, Expr(:quote, ex))
end

macro pdata(ex)
	return Expr(:call, :literalpdata, Expr(:quote, ex))
end
