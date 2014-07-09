# Extract the matrix diagonal
Base.diag{T}(dm::DataMatrix{T}) = DataArray(diag(dm.data), diag(dm.na))
