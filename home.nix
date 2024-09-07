{ config, pkgs, ... }:

{
  home.username = "alx";
  home.homeDirectory = "/home/alx";

  home.stateVersion = "24.05";

  home.packages = with pkgs; [
    git
    ripgrep
    fd
    vim
    arduino-ide
    signal-desktop
    keepassxc
    flameshot
    unzip
    owncast
  ];

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      epkgs.nix-mode
      epkgs.magit
    ];
  };

  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  home.sessionVariables = {
    EDITOR = "emacs";
  };

  programs.home-manager.enable = true;

  programs.git = {
    enable = true;
    userName = "Alexandre Girard";
    userEmail = "git@alexgirard.com";
  };

  programs.kitty.enable = true; # required for the default Hyprland config
  programs.waybar.enable = true; # required for the default Hyprland config

  services.glance.enable = true;
  services.glance.settings = {
    pages = [
      {
        columns = [
          {
            size = "small";
            widgets = [
              {
                type = "calendar";
              }
              {
                type = "weather";
                units = "metrics";
                hour-format = "24h";
                location = "Koh Samui Airport";
                hide-location = true;
              }
            ];
          }
        ];
        name = "Home";
      }
    ];
    server = {
      port = 5678;
    };
  };
}
