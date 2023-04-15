{
  description = "Neovim Project Manager";

  inputs = {
    ribosome.url = "git+https://git.tryp.io/tek/ribosome";
    hls.url = "github:haskell/haskell-language-server?ref=1.9.0.0";
  };

  outputs = {ribosome, hls, ...}: ribosome.lib.pro ({config, lib, ...}: {
    compiler = "ghc925";
    depsFull = [ribosome];
    compat.enable = false;
    hackage.versionFile = "ops/version.nix";

    overrides = {hackage, pkgs, buildInputs, fast, notest, ...}: {
      proteome = fast (buildInputs [pkgs.neovim pkgs.ripgrep]);
      streamly-process = notest (hackage "0.2.0.1" "0sip03na3g7b7avbhiqsg6xri649zizfikd10gd9ar54lpjx93wy");
    };

    cabal = {
      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      author = "Torsten Schmits";
      meta = {
        maintainer = "hackage@tryp.io";
        category = "Neovim";
        github = "tek/proteome";
        extra-source-files = ["readme.md" "changelog.md"];
      };
      ghc-options = ["-fplugin=Polysemy.Plugin"];
      prelude = {
        enable = true;
        package = {
          name = "prelate";
          version = "^>= 0.5.1";
        };
        module = "Prelate";
      };
      dependencies = ["polysemy" "polysemy-plugin"];
    };

    packages.proteome = {
      src = ./packages/proteome;

      cabal.meta.synopsis = "Neovim Project Manager";

      library = {
        enable = true;
        dependencies = [
          "Glob"
          "attoparsec"
          "chiasma"
          "chronos"
          "exon"
          "extra"
          "filepattern"
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
      };

      test = {
        enable = true;
        dependencies = [
          "aeson"
          "exon"
          "path"
          "path-io"
          "pcre-heavy"
          "polysemy-test"
          "proteome"
          "lens-regex-pcre"
          "ribosome"
          "ribosome-menu"
          "ribosome-test"
          "streamly"
          "tasty"
        ];
      };

      executable.enable = true;

    };

    envs.dev.buildInputs = with config.pkgs; [pkgs.neovim pkgs.tmux];
    envs.hls.hls.package = hls.packages.${config.system}.haskell-language-server-925;

    exe = "proteome";
    branch = "main";
    githubOrg = "tek";
    cachixName = "tek";
    cachixKey = "tek.cachix.org-1:+sdc73WFq8aEKnrVv5j/kuhmnW2hQJuqdPJF5SnaCBk=";

  });
}
