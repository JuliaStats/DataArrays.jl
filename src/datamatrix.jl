# Extract the matrix diagonal
Base.diag(dm::DataMatrix{T}) where {T} = DataArray(diag(dm.data), diag(dm.na))
