using Pkg3
if Base.isdeprecated(Base, :Test)
    using Test
else
    using Base.Test
end
using Pkg3.Types

function temp_pkg_dir(fn::Function)
    local project_path
    try
        project_path = joinpath(tempdir(), randstring())

        withenv("JULIA_ENV" => project_path) do
            fn()
        end
    finally
        rm(project_path, recursive=true, force=true)
    end
end

const TEST_PKG = "Crayons"

temp_pkg_dir() do
    Pkg3.add(TEST_PKG)
    @eval import $(Symbol(TEST_PKG))
    Pkg3.update()
    Pkg3.test(TEST_PKG)
    Pkg3.rm(TEST_PKG)

    try
        Pkg3.API.add([PackageSpec("Example", VersionSpec(v"55"))])
    catch e
        @test contains(sprint(showerror, e), "Example")
    end

    nonexisting_pkg = randstring(14)
    @test_throws CommandError Pkg3.API.add(nonexisting_pkg)
    @test_throws CommandError Pkg3.API.up(nonexisting_pkg)
    @test_warn "not in project" Pkg3.API.rm(nonexisting_pkg)
end

