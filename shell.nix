with import <nixpkgs> {};

let haskellPackages = pkgs.haskellPackages.override {
      extension = self: super: {
        fynder = self.callPackage ./fynder {};
        fynderEmail = self.callPackage ./. { doCheck = false; };
        migralo = self.callPackage <migralo> {};
        mimeMail = super.mimeMail.override { sendmail = "${pkgs.postfix}/bin/sendmail"; };
      };
    };
in lib.overrideDerivation haskellPackages.fynderEmail (attrs: {
     buildInputs = [ haskellPackages.cabalInstall_1_18_0_3 ] ++ attrs.buildInputs;
   })
