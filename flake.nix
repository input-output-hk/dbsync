{
  description = "dbsync release builds — static linux binaries and a native darwin binary";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackageNix";
    };
    hackageNix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    # IOG crypto C libraries (libsodium VRF fork, secp256k1, blst) as overlays,
    # pinned to the same revisions cardano-node builds against.
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs = { self, ... }@inputs:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];
    in
    inputs.flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import inputs.nixpkgs {
          inherit system;
          inherit (inputs.haskellNix) config;
          overlays = builtins.attrValues inputs.iohkNix.overlays ++ [
            inputs.haskellNix.overlay

            # iohk-nix's haskellBuildUtils (rewrite-libs, used for the
            # macOS release tarball) defaults to a GHC too old for this
            # haskell.nix; pin it to ours.
            (final: prev: {
              haskellBuildUtils = prev.haskellBuildUtils.override {
                compiler-nix-name = "ghc9141";
                index-state = "2026-07-22T00:00:00Z";
              };
            })

            # pkgconfig-depends name -> nixpkgs attribute, for the system
            # libraries the cabal plan resolves via pkg-config.
            (final: prev: {
              haskell-nix = prev.haskell-nix // {
                extraPkgconfigMappings = prev.haskell-nix.extraPkgconfigMappings // {
                  "libpq" = [ "libpq" ];
                  "liburing" = [ "liburing" ];
                  "lmdb" = [ "lmdb" ];
                  "snappy" = [ "snappy" ];
                };
              };
            })

            # Static variants for the musl release build; nixpkgs strips .a
            # files from these by default.
            (final: prev: {
              libpq = final.lib.pipe prev.libpq [
                (p: p.override {
                  curlSupport = false;
                  gssSupport = false;
                })
                (p: p.overrideAttrs (old:
                  final.lib.optionalAttrs final.stdenv.hostPlatform.isMusl {
                    dontDisableStatic = true;
                    NIX_LDFLAGS = "--push-state --as-needed -lstdc++ --pop-state";
                    LC_CTYPE = "C";
                    # nixpkgs' postInstall drops either the static or the
                    # shared libs; we need both present.
                    postInstall = "";
                  }))
              ];

              liburing = prev.liburing.overrideAttrs (old:
                final.lib.optionalAttrs final.stdenv.hostPlatform.isMusl {
                  dontDisableStatic = true;
                  # Replaces the nixpkgs postInstall, which removes static
                  # libs; keep its example-binary copying intact.
                  postInstall = ''
                    for file in $(find ./examples -executable -type f); do
                      install -Dm555 -t "$bin/bin" "$file"
                    done
                  '';
                });

              snappy = prev.snappy.override (
                final.lib.optionalAttrs final.stdenv.hostPlatform.isMusl {
                  static = true;
                });
            })
          ];
        };

        inherit (pkgs) lib;

        project = pkgs.haskell-nix.cabalProject' {
          src = ./.;
          name = "dbsync";
          compiler-nix-name = "ghc9141";

          # CHaP resolves from the flake input instead of the network; the
          # cardano-node source-repository-package hash rides as a --sha256
          # comment in cabal.project.
          inputMap = { "https://chap.intersectmbo.org/" = inputs.CHaP; };

          modules = [
            {
              doHaddock = false;

              # protoc for proto-lens code generation.
              packages.proto-lens-protobuf-types.components.library.build-tools =
                [ pkgs.buildPackages.protobuf ];
              packages.cardano-rpc.components.library.build-tools =
                [ pkgs.buildPackages.protobuf ];
              packages.cardano-rpc.components.sublibs.gen.build-tools =
                [ pkgs.buildPackages.protobuf ];
            }

            # Static libpq carries no dependency metadata: pull in pgcommon /
            # pgport and OpenSSL explicitly (-lssl must precede -lcrypto).
            ({ pkgs, lib, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isMusl {
              packages.dbsync.ghcOptions = [
                "-optl-Wl,-lpgcommon"
                "-optl-Wl,-lpgport"
                "-optl-Wl,-lm"
                "-L${pkgs.openssl.out}/lib"
                "-optl-Wl,-lssl"
                "-optl-Wl,-lcrypto"
              ];
            })
          ];
        };

        muslProject =
          if system == "x86_64-linux" then project.projectCross.musl64
          else if system == "aarch64-linux" then project.projectCross.aarch64-multiplatform-musl
          else null;

        packages = {
          dbsync = project.hsPkgs.dbsync.components.exes.dbsync;
          default = packages.dbsync;
        } // lib.optionalAttrs (muslProject != null) {
          # Fully static: this is the release-tarball and docker-image binary.
          dbsync-static = muslProject.hsPkgs.dbsync.components.exes.dbsync;
        } // lib.optionalAttrs (system == "aarch64-darwin") {
          # Native build with its dylib deps copied alongside and
          # install_name_tool-rewritten to @executable_path, then
          # re-signed (required on Apple Silicon after any load-command
          # edit) — runs standalone, no nix or homebrew needed.
          dbsync-macos = pkgs.runCommand "dbsync-macos"
            {
              # rewrite-libs shells out to nix-store itself, hence pkgs.nix;
              # darwin.sigtool provides a sandbox-safe codesign stand-in for
              # the real Apple tool, which the build sandbox can't reach.
              nativeBuildInputs = [
                pkgs.haskellBuildUtils
                pkgs.bintools
                pkgs.nix
                pkgs.darwin.sigtool
              ];
            }
            ''
              mkdir -p $out/bin
              cp ${packages.dbsync}/bin/dbsync $out/bin/dbsync
              chmod +w $out/bin/dbsync
              rewrite-libs $out/bin $out/bin/dbsync
              codesign -f -s - $out/bin/dbsync
            '';
        };
      in
      {
        inherit packages;

        # Contract for IOG Hydra: the buildable jobs plus a `required`
        # aggregate for the jobset's gate.
        hydraJobs =
          let jobs = removeAttrs packages [ "default" ];
          in jobs // pkgs.callPackages inputs.iohkNix.utils.ciJobsAggregates {
            ciJobs = jobs;
            nonRequiredPaths = [ ];
          };
      });

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = true;
  };
}
