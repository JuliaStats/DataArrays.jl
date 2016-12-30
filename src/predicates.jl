if VERSION < v"0.6.0-dev+1781"
  numeric_predicates = (:(Base.iseven), :(Base.ispow2), :(Base.isfinite), :(Base.isprime), :(Base.isinf), :(Base.isodd))
else
  import Primes
  numeric_predicates = (:(Base.iseven), :(Base.ispow2), :(Base.isfinite), :(Primes.isprime), :(Base.isinf), :(Base.isodd))
end
  
type_predicates = (:(Base.isinteger), :(Base.isreal))

container_predicates = (:(Base.isempty),)

for p in numeric_predicates
  @eval begin
    ($p)(v::NAtype) = NA
  end
end

for p in type_predicates
  @eval begin
    ($p)(v::NAtype) = NA
  end
end
