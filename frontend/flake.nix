{
  description = "Storyteller frontend, pre-built static export (bun run build:static)";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      packages.${system}.default = pkgs.stdenvNoCC.mkDerivation {
        pname = "storyteller-frontend-dist";
        version = "0";
        src = ./out;
        dontBuild = true;
        installPhase = "mkdir -p $out && cp -r . $out";
      };
    };
}
