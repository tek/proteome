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
      inputs = buildInputs [pkgs.neovim pkgs.ripgrep];
    in {
      proteome = fast inputs;
      streamly-process = hackage "0.2.0.1" "0sip03na3g7b7avbhiqsg6xri649zizfikd10gd9ar54lpjx93wy";
    };

  in ribosome.lib.pro ({ config, lib, ... }: {
    inherit overrides;
    depsFull = [ribosome];
    compat.enable = false;
    devGhc.compiler = "ghc902";
    packages.proteome = ./packages/proteome;
    exe = "proteome";
    branch = "main";
    githubOrg = "tek";
    cachixName = "tek";
    cachixKey = "tek.cachix.org-1:+sdc73WFq8aEKnrVv5j/kuhmnW2hQJuqdPJF5SnaCBk=";
    hackage.versionFile = "ops/version.nix";
    hpack.packages = import ./ops/hpack.nix { inherit config lib; };
    ghcid.shellConfig.buildInputs = with config.devGhc.pkgs; [pkgs.neovim pkgs.tmux];
    ghci = {
      preludePackage = "prelate";
      preludeModule = "Prelate";
      args = ["-fplugin=Polysemy.Plugin"];
      extensions = ["StandaloneKindSignatures" "OverloadedLabels" "ImpredicativeTypes"];
    };
  });
}
