@_default:
  just --list --unsorted

all: format-nix lint-nix update-nix chezmoi hm-switch

chezmoi:
  chezmoi apply

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
	
hm-switch:
  @echo "switching home-manager profile"
  home-manager switch
