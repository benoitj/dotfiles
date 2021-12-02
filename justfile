@_default:
  just --list --unsorted

all: format-nix lint-nix update-nix chezmoi hm-switch

# deploy dotfiles to ~
chezmoi:
  yes quit | chezmoi apply

# update nix channels
update-nix:
  nix-channel --update

# Formatting nix files
format-nix:
  @echo "Formatting nix files"
  find . -iname '*.nix' -exec nixpkgs-fmt {} \;
	
# Linting nix files
lint-nix:
  @echo "Linting nix files"
  nix-linter -r .
	
# switching current profile from updates
hm-switch:
  @echo "switching home-manager profile"
  home-manager switch
