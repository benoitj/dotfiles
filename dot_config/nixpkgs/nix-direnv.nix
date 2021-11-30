{ pkgs, ... }:

{
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
  # optional for nix flakes support
  #programs.direnv.nix-direnv.enableFlakes = true;
}
