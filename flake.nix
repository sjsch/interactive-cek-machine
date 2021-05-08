{
  description = "An interactive CEK machine, with visualization.";

  outputs = { self, nixpkgs }:
    let
      easy-ps = nixpkgs.fetchFromGitHub {
        owner = "justinwoo";
        repo = "easy-purescript-nix";
        rev = "a5fd0328827ac46954db08f624c09eba981f1ab2";
        sha256 = "1g3bk2y8hz0y998yixz3jmvh553kjpj2k7j0xrp4al1jrbdcmgjq";
      };

    in
    {
      # packages.x86_64-linux.cekmachine = nixpkgs.legacyPackages.x86_64-linux.cekmachine;
      # defaultPackage.x86_64-linux = self.packages.x86_64-linux.cekmachine;

      devShell = nixpkgs.stdenv.mkDerivation {
        # inputsFrom = with pkgs; [ ];
        buildInputs = [ easy-ps.spago2nix ];
      };
    };
}
