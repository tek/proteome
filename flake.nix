{
  description = "Neovim Project Manager";

  inputs.ribosome.url = github:tek/ribosome;

  outputs = { ribosome, ... }:
  let
    inherit (ribosome.inputs) chiasma hix;
    overrides = { hackage, source, minimal, configure, pkgs, transform_, ... }: {
      cornea = hackage "0.4.0.0" "1w9rkf6f861kknkskywb8fczlk7az8m56i3hvmg6a5inpvqf6p7i";
      chiasma = source.package chiasma "chiasma";
      proteome-test = transform_ (drv: drv.overrideAttrs (old: {
        buildInputs = old.buildInputs ++ [pkgs.neovim pkgs.tmux pkgs.ripgrep];
      }));
      ribosome = configure "--extra-prog-path=${pkgs.neovim}/bin" (minimal (source.package ribosome "ribosome"));
      ribosome-test = minimal (source.package ribosome "test");
    };

  in hix.flake {
    base = ./.;
    inherit overrides;
    compat = false;
    packages = {
      proteome = ./packages/proteome;
      proteome-test = ./packages/test;
    };
    main = "proteome";
    versionFile = "ops/hpack/packages/meta.yaml";
    runConfig = p: { buildInputs = [p.pkgs.neovim]; };
    modify = _: outputs: rec {
      apps.proteome = {
        type = "app";
        program = "${outputs.packages.proteome}/bin/proteome";
      };
      defaultApp = apps.proteome;
    };
  };
}
