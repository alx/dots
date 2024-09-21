{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.permittedInsecurePackages = [
    "olm-3.2.16"
  ];

  home.username = "alx";
  home.homeDirectory = "/home/alx";
  home.stateVersion = "24.05";

  home.packages = with pkgs; [
    ripgrep
    fd
    file
    wget
    ffmpeg
    rsync
    python3Full
    python312Packages.flask
    tmux
    monaspace

    git
    vim
    arduino-ide
    unzip
    imagemagick
    jq
    hugo
    uv

    rofi
    networkmanagerapplet
    xss-lock
    pulseaudio
    papirus-icon-theme

    neofetch
    flameshot
    krita
    gimp
    vlc
    transmission_4
    owncast
    keepassxc
    discord
    matrix-commander
    freetube

    signal-desktop
    element-desktop
    calibre
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

    ".config/i3/config".source = dotfiles/i3/config;
    ".config/rofi/config.rasi".source = dotfiles/rofi/config.rasi;
    ".config/kitty/launch.conf".source = dotfiles/kitty/launch.conf;

    ".tmux.conf".source = dotfiles/tmux/tmux.conf;

    ".config/doom/config.el".source = dotfiles/doom/config.el;
    ".config/doom/custom.el".source = dotfiles/doom/custom.el;
    ".config/doom/init.el".source = dotfiles/doom/init.el;
    ".config/doom/packages.el".source = dotfiles/doom/packages.el;

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

}
