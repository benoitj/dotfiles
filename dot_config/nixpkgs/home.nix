{ pkgs, ... }:

{
  imports = [
    ./tools.nix
    ./nix-direnv.nix
    ./mail.nix
  ];
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "benoit";
  home.homeDirectory = "/home/benoit";

  home.packages = with pkgs; [
    irssi
    ncspot
    cups
    gh
    keychain
  ];

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
