using DataArrays, Documenter

makedocs(
    modules = [DataArrays],
    clean = false,
    format = :html,
    sitename = "DataArrays.jl",
    authors = "Simon Kornblith, John Myles White, and other contributors",
    pages = [
        "Home" => "index.md",
        "Missing Data and Arrays" => "da.md",
        "Utilities" => "util.md",
    ],
)

deploydocs(
    repo = "github.com/JuliaStats/DataArrays.jl.git",
    target = "build",
    deps = nothing,
    make = nothing,
)
