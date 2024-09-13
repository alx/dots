{ config, pkgs, ... }:

{
  home.username = "alx";
  home.homeDirectory = "/home/alx";

  home.stateVersion = "24.05";

  home.packages = with pkgs; [
    ripgrep
    fd
    file
    wget
    ffmpeg

    git
    vim
    arduino-ide
    unzip
    imagemagick
    jq
    hugo

    neofetch
    flameshot
    krita
    gimp
    vlc
    transmission_4
    owncast
    keepassxc

    signal-desktop
    element-desktop
  ];

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      epkgs.nix-mode
      epkgs.magit
    ];
  };

  # Install firefox.
  programs.firefox.enable = true;

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

  programs.kitty = {
    enable = true;
    font = {
      name = "BitstreamVeraSansMono Nerd Font";
      size = 16;
    };
    theme = "Catppuccin-Macchiato";
    #Also available: Catppuccin-Frappe Catppuccin-Latte Catppuccin-Macchiato Catppuccin-Mocha
    # See all available kitty themes at: https://github.com/kovidgoyal/kitty-themes/blob/46d9dfe230f315a6a0c62f4687f6b3da20fd05e4/themes.json
  };

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
