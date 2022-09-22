{ config, lib, ... }:
with builtins;
with lib;
let

  mergeAttr = a: b:
  if isAttrs a
  then merge a b
  else if isList a
  then a ++ b
  else b;

  merge = l: r:
  let
    f = name:
    if hasAttr name l && hasAttr name r
    then mergeAttr l.${name} r.${name}
    else l.${name} or r.${name};
  in genAttrs (concatMap attrNames [l r]) f;

  paths = name: {
    when = {
      condition = false;
      generated-other-modules = ["Paths_${replaceStrings ["-"] ["_"] name}"];
    };
  };

  meta = {
    version = import ./version.nix;
    license = "BSD-2-Clause-Patent";
    license-file = "LICENSE";
    author = "Torsten Schmits";
    maintainer = "hackage@tryp.io";
    copyright = "2022 Torsten Schmits";
    category = "Neovim";
    build-type = "Simple";
    github = "tek/proteome";
  };

  options.ghc-options = [
    "-Wall"
    "-Wredundant-constraints"
    "-Wincomplete-uni-patterns"
    "-Wmissing-deriving-strategies"
    "-Widentities"
    "-Wunused-packages"
    "-fplugin=Polysemy.Plugin"
  ];

  dependencies = [
      { name = "base"; version = ">= 4.12 && < 5"; mixin = "hiding (Prelude)"; }
      { name = "prelate"; version = ">= 0.1"; mixin = ["(Prelate as Prelude)" "hiding (Prelate)"]; }
      "polysemy"
      "polysemy-plugin"
    ];

  basic = name: merge (meta // options) {
    inherit name;
    default-extensions = config.ghci.extensions;
  };

  project = name: basic name // {
    library = paths name // {
      source-dirs = "lib";
      inherit dependencies;
    };
  };

  exe = name: dir: merge (paths name // {
    main = "Main.hs";
    source-dirs = dir;
    inherit dependencies;
    ghc-options = [
      "-threaded"
      "-rtsopts"
      "-with-rtsopts=-N"
    ];
  });

in {

  proteome = merge (project "proteome") {
    synopsis = "Neovim Project Manager";
    description = "See https://hackage.haskell.org/package/proteome/docs/Proteome.html";
    library.dependencies = [
      "Glob"
      "attoparsec"
      "chiasma"
      "chronos"
      "exon"
      "extra"
      "filepattern"
      "lens"
      "lens-regex-pcre"
      "microlens"
      "nonempty-zipper"
      "parsers"
      "path"
      "path-io"
      "pcre-light"
      "polysemy-chronos"
      "polysemy-process"
      "prettyprinter"
      "raw-strings-qq"
      "ribosome"
      "ribosome-host"
      "ribosome-menu"
      "stm-chans"
      "streamly >= 0.8"
      "streamly-process >= 0.1"
      "transformers"
      "typed-process"
    ];
    executables.proteome = exe "proteome" "app" {
      dependencies = ["proteome"];
    };
  };

  proteome-test = merge (project "proteome-test") {
    synopsis = "Proteome tests";
    description = "See https://hackage.haskell.org/package/proteome/docs/Proteome.html";
    tests.proteome-unit = exe "proteome-test" "test" {
      dependencies = [
        "aeson"
        "exon"
        "lens"
        "path"
        "path-io"
        "pcre-heavy"
        "polysemy-test"
        "proteome"
        "lens-regex-pcre"
        "ribosome"
        "ribosome-host"
        "ribosome-menu"
        "ribosome-test"
        "streamly"
        "tasty"
      ];
    };
  };

}
