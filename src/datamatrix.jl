# Extract the matrix diagonal
using LinearAlgebra
LinearAlgebra.diag(dm::DataMatrix{T}) where {T} = DataArray(diag(dm.data), diag(dm.na))
