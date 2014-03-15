# Note: These methods need a more helpfull error message than a `NoMethodError`,
#       when the deprecation is removed
import Base.@deprecate
@deprecate (+)(A::AbstractDataArray{Bool},x::Bool)              A .+ x
@deprecate (+)(x::Bool,A::AbstractDataArray{Bool})              x .+ A 
@deprecate (-)(A::AbstractDataArray{Bool},x::Bool)              A .- x
@deprecate (-)(x::Bool,A::AbstractDataArray{Bool})              x .- A
@deprecate (+)(A::AbstractDataArray,x::Union(NAtype,Number))    A .+ x
@deprecate (+)(x::Union(NAtype,Number),A::AbstractDataArray)    x .+ A
@deprecate (-)(A::AbstractDataArray,x::Union(NAtype,Number))    A .- x
@deprecate (-)(x::Union(NAtype,Number),A::AbstractDataArray)    x .- A
@deprecate (/)(x::Union(NAtype,Number),A::AbstractDataArray)    x ./ A
