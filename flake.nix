{
  description = "Storyteller";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    runix-flake = {
      url = "github:n3wm1nd/runix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    universal-llm-flake = {
      url = "github:n3wm1nd/universal-llm";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    runix-tools-flake = {
      url = "github:n3wm1nd/runix-tools";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Vendored at the exact commit gitlib-effect/cbits/build-libgit2.sh builds
    # locally (see vendor/libgit2, pinned via the git submodule) -- fetched
    # here instead of read off disk so the flake doesn't depend on the
    # submodule's checkout state.
    libgit2-src = {
      url = "github:libgit2/libgit2/f7164261c9bc0a7e0ebf767c584e5192810a8b24";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, runix-flake, universal-llm-flake, runix-tools-flake, libgit2-src, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};

      # Static libgit2, built with the same flags as cbits/build-libgit2.sh
      # (no SSH/HTTPS transports -- this codebase only touches local repos).
      libgit2 = pkgs.stdenv.mkDerivation {
        pname = "libgit2-vendored";
        version = "f7164261";
        src = libgit2-src;
        nativeBuildInputs = [ pkgs.cmake pkgs.pkg-config ];
        buildInputs = [ pkgs.zlib ];
        cmakeFlags = [
          "-DBUILD_SHARED_LIBS=OFF"
          "-DUSE_SSH=OFF"
          "-DUSE_HTTPS=OFF"
          "-DREGEX_BACKEND=builtin"
          "-DUSE_BUNDLED_ZLIB=OFF"
          "-DUSE_THREADS=ON"
          "-DBUILD_TESTS=OFF"
          "-DBUILD_CLI=OFF"
          "-DBUILD_EXAMPLES=OFF"
          "-DBUILD_FUZZERS=OFF"
          "-DCMAKE_INSTALL_LIBDIR=lib"
        ];
      };

      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: {
          runix = runix-flake.packages.${system}.runix;
          universal-llm = universal-llm-flake.packages.${system}.universal-llm;
          runix-tools = runix-tools-flake.packages.${system}.runix-tools;

          gitlib-effect = pkgs.haskell.lib.compose.addTestToolDepends [ pkgs.git ]
            (pkgs.haskell.lib.compose.overrideCabal (old: {
            # gitlib-effect's Setup.hs (build-type: Custom) shells out to
            # cbits/build-libgit2.sh in preBuild, which builds vendor/libgit2
            # (a git submodule not present in this flake's filtered source)
            # unless its output is already there -- pre-stage the nix-built
            # libgit2 so the script's own "already built" short-circuit fires
            # and vendor/libgit2 is never touched.
            preConfigure = ''
              ${old.preConfigure or ""}
              patchShebangs cbits/build-libgit2.sh
              mkdir -p cbits/build/install
              cp -r ${libgit2}/lib cbits/build/install/lib
              cp -r ${libgit2}/include cbits/build/install/include
            '';
          }) (self.callCabal2nix "gitlib-effect" ./gitlib-effect { git2 = libgit2; }));

          storyteller = pkgs.haskell.lib.compose.addTestToolDepends [ pkgs.git ]
            (self.callCabal2nix "storyteller" ./. { });
        };
      };

      storyteller = haskellPackages.storyteller;
    in
    {
      packages.${system} = {
        default = storyteller;
        storyteller = storyteller;
        gitlib-effect = haskellPackages.gitlib-effect;
      };

      apps.${system} = {
        default = self.apps.${system}.story-server;
        story-server = {
          type = "app";
          program = "${storyteller}/bin/story-server";
        };
      };

      devShells.${system}.default = haskellPackages.shellFor {
        packages = p: [ p.storyteller p.gitlib-effect ];
        buildInputs = [
          haskellPackages.haskell-language-server
          pkgs.cabal-install
          # gitlib-effect's Custom Setup.hs builds vendor/libgit2 for real
          # here (unlike the nix package build, which pre-stages a nix-built
          # copy) since cabal.project lists it as a local, editable package.
          pkgs.cmake
          pkgs.pkg-config
          pkgs.zlib
          pkgs.git
        ];
        withHoogle = true;
      };
    };
}
