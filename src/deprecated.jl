# Note: These methods need a more helpfull error message than a `NoMethodError`,
#       when the deprecation is removed
import Base.@deprecate
@deprecate (/)(x::Union(NAtype,Number),A::AbstractDataArray)    x ./ A
