{
  description = "Neovim Project Manager";

  inputs = {
    ribosome.url = "git+https://git.tryp.io/tek/ribosome";
  };

  outputs = {ribosome, ...}: ribosome.lib.pro ({config, ...}: {
    depsFull = [ribosome];
    compat.enable = false;
    hackage.versionFile = "ops/version.nix";
    gen-overrides.enable = true;

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
          version = ">= 0.6 && < 0.8";
        };
        module = "Prelate";
      };
      dependencies = ["polysemy" "polysemy-plugin"];
    };

    buildInputs = pkgs: [pkgs.neovim pkgs.ripgrep];

    overrides = {hackage, jailbreak, notest, ...}: {
      streamly-process = jailbreak (notest (hackage "0.2.0.1" "0sip03na3g7b7avbhiqsg6xri649zizfikd10gd9ar54lpjx93wy"));
    };

    packages.proteome = {
      src = ./packages/proteome;

      cabal.meta.synopsis = "Neovim Project Manager";

      buildInputs = pkgs: [pkgs.neovim pkgs.ripgrep];

      override = api: api.fast;

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

    exe = "proteome";
    branch = "main";
    githubOrg = "tek";
    cachixName = "tek";
    cachixKey = "tek.cachix.org-1:+sdc73WFq8aEKnrVv5j/kuhmnW2hQJuqdPJF5SnaCBk=";

  });
}
