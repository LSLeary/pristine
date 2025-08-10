{
  inputs = {
    flake-utils.url       = "github:numtide/flake-utils";
    git-ignore-nix.url    = "github:hercules-ci/gitignore.nix/master";
    flake-lib-haskell.url = "github:LSLeary/flake-lib-haskell";
  };
  outputs = { self, flake-utils, nixpkgs, git-ignore-nix, flake-lib-haskell }:
  let
    hlib = flake-lib-haskell;
    defComp = if builtins.pathExists ./comp.nix
      then import ./comp.nix
      else { };
    hoverlay = final: prev: hfinal: hprev: with prev.haskell.lib; {
      poly               = unmarkBroken (dontCheck hprev.poly);
      pristine           = hfinal.callCabal2nix "pristine"
        (git-ignore-nix.lib.gitignoreSource ./core     ) { };
      pristine-analysis  = hfinal.callCabal2nix "pristine-analysis"
        (git-ignore-nix.lib.gitignoreSource ./analysis ) { };
      pristine-vis-Chart = hfinal.callCabal2nix "pristine-vis-Chart"
        (git-ignore-nix.lib.gitignoreSource ./vis-Chart) { };
      pristine-example   = hfinal.callCabal2nix "pristine-example"
        (git-ignore-nix.lib.gitignoreSource ./example  ) { };
    };
    overlay = hlib.fromHOL defComp hoverlay;
    hoverlays = [ hoverlay ];
    overlays  = [  overlay ];
  in flake-utils.lib.eachDefaultSystem (system:
  let pkgs = import nixpkgs { inherit system overlays; };
      hpkg = pkgs.lib.attrsets.getAttrFromPath (hlib.hpath defComp) pkgs;
  in {
    devShell = hpkg.shellFor {
      doBenchmark = true;
      packages = with pkgs.haskell.lib; p:
        [ p.pristine
          p.pristine-analysis
          p.pristine-vis-Chart
          (doBenchmark p.pristine-example)
        ];
    # packages = p: [ ];
    # buildInputs =
    #   [ pkgs.blas
    #     pkgs.lapack
    #     pkgs.expat
    #     pkgs.cairo
    #     pkgs.libxdmcp
    #   ];
    # nativeBuildInputs = [ pkgs.pkg-config ];
    };
    defaultPackage = hpkg.pristine;
  }) // { inherit hoverlay overlay hoverlays overlays; };
}
