# DOTS

nixos dotfiles

## Setup

1. install `nixos`
2. install `home-manager`
3. run:

```
git clone git@github.com:alx/dots.git 

cd dots
sudo ln -sf $PWD/configuration.nix /etc/nixos/configuration.nix
ln -sf $PWD/home.nix $HOME/.config/home-manager/home.nix

sudo nixos-rebuild switch
home-manager switch
```

## cheatsheet

- `neofetch`: system status

## resources

-  NixOS: Everything Everywhere All At Once
   https://www.youtube.com/watch?v=CwfKlX3rA6E
