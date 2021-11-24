{
  description = "Neovim Project Manager";

  # inputs.ribosome.url = github:tek/ribosome;
  inputs.ribosome.url = path:/home/tek/code/tek/haskell/ribosome;
  inputs.streamly-process = {
    flake = false;
    url = github:/tek/streamly-process;
  };

  outputs = { ribosome, streamly-process, ... }:
  let
    inherit (ribosome.inputs) chiasma hix;
    overrides = { hackage, source, minimal, configure, pkgs, transform_, unbreak, jailbreak, ... }: {
      cornea = hackage "0.4.0.0" "1w9rkf6f861kknkskywb8fczlk7az8m56i3hvmg6a5inpvqf6p7i";
      chiasma = source.package chiasma "chiasma";
      proteome-test = transform_ (drv: drv.overrideAttrs (old: {
        buildInputs = old.buildInputs ++ [pkgs.neovim pkgs.tmux pkgs.ripgrep];
      }));
      ribosome = configure "--extra-prog-path=${pkgs.neovim}/bin" (minimal (source.package ribosome "ribosome"));
      ribosome-test = minimal (source.package ribosome "test");
      streamly = hackage "0.8.1" "0ywyy7gxjnp32hx8kki0lfn94bnc9mzjh8g6mg65ff3vv28k2vdr";
      streamly-process = minimal (source.root streamly-process);
    };

  in hix.flake {
    base = ./.;
    inherit overrides;
    deps = [ribosome];
    compat = false;
    packages = {
      proteome = ./packages/proteome;
      proteome-test = ./packages/test;
    };
    main = "proteome";
    versionFile = "ops/hpack/packages/meta.yaml";
    shellConfig = p: { buildInputs = [p.pkgs.neovim p.pkgs.ripgrep p.pkgs.tmux]; };
    modify = _: outputs: rec {
      apps.proteome = {
        type = "app";
        program = "${outputs.packages.proteome}/bin/proteome";
      };
      defaultApp = apps.proteome;
    };
  };
}
