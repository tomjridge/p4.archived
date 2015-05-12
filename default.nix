{ }: 
# nix-build default.nix -A p4.build
# nix-shell default.nix -A p4.build
# nix-shell default.nix -A p4.post_install
let 
    pkgs = import <nixpkgs> {};
    stdenv = pkgs.stdenv;
    fetchgit = pkgs.fetchgit;
   e3 = import ./../e3 { }; 
ocaml=pkgs.ocaml_4_02_1; findlib=pkgs.ocamlPackages_4_02_1.findlib;
in stdenv.mkDerivation {
      name = "p4";
    
  #    src = fetchgit {
  #      url = https://github.com/tomjridge/p3.git;
  #      rev = "0e42a29";
  #      sha256 = "795b8bacbea102021ad4aaa819d578e58fd7d2041eba60p46482e04e01f81c32";
  #    };
      src=./.;
    
      buildInputs = [ ocaml findlib e3 ];
    

      patchPhase = "ln -sf ${e3} src_ext/e3";

      buildPhase="cd build && make";

      configurePhase = "true"; 	# Skip configure
  
      installPhase = "true";
    
      createFindlibDestdir = true;
    }
