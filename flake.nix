{
  description = "Neovim Project Manager";

  inputs = {
    ribosome.url = git+https://git.tryp.io/tek/ribosome;
    streamly-process = {
      flake = false;
      url = github:/tek/streamly-process;
    };
  };

  outputs = { ribosome, streamly-process, ... }:
  let
    overrides = { hackage, source, minimal, pkgs, buildInputs, fast, ... }:
    let
      inputs = buildInputs [pkgs.neovim pkgs.tmux pkgs.xterm pkgs.ripgrep];
    in {
      proteome = fast;
      proteome-test = fast inputs;
      streamly = hackage "0.8.1" "0ywyy7gxjnp32hx8kki0lfn94bnc9mzjh8g6mg65ff3vv28k2vdr";
      streamly-process = minimal (source.root streamly-process);
    };

  in ribosome.lib.flake ({ config, lib, ... }: {
    base = ./.;
    inherit overrides;
    depsFull = [ribosome];
    compat.enable = false;
    packages = {
      proteome = ./packages/proteome;
      proteome-test = ./packages/test;
    };
    main = "proteome";
    exe = "proteome";
    branch = "main";
    githubOrg = "tek";
    cachixName = "tek";
    cachixKey = "tek.cachix.org-1:+sdc73WFq8aEKnrVv5j/kuhmnW2hQJuqdPJF5SnaCBk=";
    hpack = {
      packages = import ./ops/hpack.nix { inherit config lib; };
      defaultApp = "proteome";
    };
    ghcid.shellConfig.buildInputs = with config.devGhc.pkgs; [pkgs.neovim pkgs.tmux];
    ghci = {
      preludePackage = "prelate";
      preludeModule = "Prelate";
      args = ["-fplugin=Polysemy.Plugin"];
      extensions = ["StandaloneKindSignatures" "OverloadedLabels" "ImpredicativeTypes"];
    };
  });
}
