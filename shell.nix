with import <nixpkgs> {};

let haskellPackages = pkgs.haskellPackages.override {
      extension = self: super: {
        fynderEmail = self.callPackage ./. { doCheck = false; };
      };
    };
in lib.overrideDerivation haskellPackages.fynderEmail (attrs: {
     buildInputs = [ haskellPackages.cabalInstall_1_18_0_3 ] ++ attrs.buildInputs;
   })