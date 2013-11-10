numeric_predicates = (:(Base.iseven), :(Base.ispow2), :(Base.isfinite), :(Base.isprime), :(Base.isinf), :(Base.isodd))

type_predicates = (:(Base.isinteger), :(Base.isreal), :(Base.iscomplex))

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
