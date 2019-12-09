let
  hsPkgs = import ./default.nix {};
in
  hsPkgs.mateamt.components.all
