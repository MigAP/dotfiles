try
    using Revise
    using OhMyREPL
    enable_autocomplete_brackets(false)
catch e
    @warn "Error initializing Revise"
end


function template()
    @eval begin
        using PkgTemplates
        Template(;
                 user="arpaperm",
                 authors="Miguel Arpa Perozo",
                 dir="./",
                 julia=v"1.12",
                 host="gricad-gitlab.univ-grenoble-alpes.fr",
                 plugins=[
                     Git(;
	                 name="Miguel Arpa Perozo",
	                 email="miguel.arpa.p@gmail.com", 
	                 manifest=true,
	                 ssh=true,
                         jl=false),
                     GitLabCI(),
                     Documenter{GitLabCI}(),
                 ],
                 )
    end
    
end
