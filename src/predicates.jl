numeric_predicates = [:(Base.iseven), :(Base.ispow2), :(Base.isfinite), :(Base.isinf), :(Base.isodd)]
isdefined(Base, :isprime) && push!(numeric_predicates, :(Base.isprime))
  
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
