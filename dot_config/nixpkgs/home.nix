{ config, pkgs, ... }:

{
  imports = [ ./tools.nix ];
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "benoit";
  home.homeDirectory = "/home/benoit";

  home.packages = with pkgs; [
    # cachix??
    irssi
  ];
  #nix = {
  #  binaryCaches = [ "https://nix-community.cachix.org/" ];
  #  binaryCachePublicKeys = [
  #    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
  #  ];
  #};

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;


}
