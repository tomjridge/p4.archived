{ }: 
let 
    pkgs = import <nixpkgs> {};
    stdenv = pkgs.stdenv;
    fetchgit = pkgs.fetchgit;
    p1 = import ./../p1 { }; 
    e3 = import ./../e3 { }; 
    ocaml=pkgs.ocaml; 
    findlib=pkgs.ocamlPackages.findlib;
in stdenv.mkDerivation {
      name = "p4";
    
  #    src = fetchgit {
  #      url = https://github.com/tomjridge/p3.git;
  #      rev = "0e42a29";
  #      sha256 = "795b8bacbea102021ad4aaa819d578e58fd7d2041eba60p46482e04e01f81c32";
  #    };
      src=./.;
    
      buildInputs = [ ocaml findlib e3 p1 ];
    
      configurePhase = "true"; 	# Skip configure

#      patchPhase = "ln -sf ${e3} src_ext/e3";

#      buildPhase="cd build && make && cd ..";
 
      postInstall = "mkdir -p $out && cp -R * $out"; # so we can inspect the result
    
      createFindlibDestdir = true;
    }
