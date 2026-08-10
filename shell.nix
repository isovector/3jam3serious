{ ghc }:

let
  pkgs = import <nixpkgs> { };

  SDL2_gfx_symbolic = pkgs.SDL2_gfx.overrideAttrs (old: {
    env = (old.env or { }) // {
      NIX_LDFLAGS =
        (old.env.NIX_LDFLAGS or "")
        + " -Bsymbolic-functions";
    };
  });
in
pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "3jam3serious";

  nativeBuildInputs = [
    pkgs.pkg-config
  ];

  buildInputs = [
    pkgs.SDL2
    SDL2_gfx_symbolic
  ];
}