{ pkgs ? import <nixpkgs> { } }:

with pkgs;

let
  easy-ps = import
    (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "fbbb27c1afd51d729939a6a2006e954dbd844846";
      sha256 = "1kw9cqycrq456dipd5mq7c1ij6jl3d9ajlnba152db3qrw5wmrg0";
    })
    {
      inherit pkgs;
    };

  spagoPkgs = import ./nix/spago-packages.nix { inherit pkgs; };

  getGlob = pkg: ''".spago/${pkg.name}/${pkg.version}/src/**/*.purs"'';

  nodeEnv = import ./nix/node-env.nix {
    inherit (pkgs) stdenv lib python2 runCommand writeTextFile;
    inherit pkgs;
    nodejs = pkgs.nodejs-14_x;
    libtool = null;
  };

  nodePkgs = import ./nix/node-packages.nix {
    inherit nodeEnv;
    inherit (pkgs) fetchurl nix-gitignore stdenv lib fetchgit;
  };

in

stdenv.mkDerivation {
  name = "cekmachine";
  version = "0.1.0";

  src = ./.;

  nativeBuildInputs = with easy-ps; [
    easy-ps.zephyr
    easy-ps.purs-0_14_1
  ];

  buildPhase = ''
    ln -s ${nodePkgs.nodeDependencies}/lib/node_modules .
    ${spagoPkgs.installSpagoStyle}/bin/install-spago-style
    purs compile --codegen corefn \
      ${builtins.toString (builtins.map getGlob (builtins.attrValues spagoPkgs.inputs))} \
      src/*.purs src/**/*.purs
    zephyr -f Main.main
    ${nodePkgs.nodeDependencies}/bin/webpack
  '';

  installPhase = ''
    cp dist/main.js $out
  '';
}
