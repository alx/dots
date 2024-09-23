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

    # system
    ripgrep
    fd
    file
    wget
    ffmpeg
    rsync
    python3Full
    python312Packages.flask
    tmux
    btop

    # dev
    monaspace
    git
    vim
    arduino-ide
    unzip
    imagemagick
    jq
    hugo
    uv

    # desktop
    rofi
    networkmanagerapplet
    dex
    xss-lock
    pulseaudio
    papirus-icon-theme

    # softwares
    # - utils
    neofetch
    flameshot
    keepassxc
    # - graphics
    krita
    gimp
    vlc
    # - chat
    discord
    matrix-commander
    signal-desktop
    element-desktop
    # - sharing
    transmission_4
    freetube
    owncast
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

    ".tmux.conf".source = dotfiles/tmux/tmux.conf;

    ".config/i3/config".source = dotfiles/i3/config;
    ".config/rofi/config.rasi".source = dotfiles/rofi/config.rasi;
    ".config/kitty/launch.conf".source = dotfiles/kitty/launch.conf;

    ".config/doom/config.el".source = dotfiles/doom/config.el;
    ".config/doom/custom.el".source = dotfiles/doom/custom.el;
    ".config/doom/init.el".source = dotfiles/doom/init.el;
    ".config/doom/packages.el".source = dotfiles/doom/packages.el;

    # How to enable global dark mode using Home manager in NixOS
    # https://discourse.nixos.org/t/how-to-enable-global-dark-mode-using-home-manager-in-nixos/28348/2
    "/etc/xdg/gtk-2.0/gtkrc".source = dotfiles/gtk/gtk2.0;
    "/etc/xdg/gtk-3.0/settings.ini".source = dotfiles/gtk/settings.ini;
    "/etc/xdg/gtk-4.0/settings.ini".source = dotfiles/gtk/settings.ini;

    #
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;
    #
    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
    #

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
      name = "Monaspace Argon";
      size = 18;
    };
    theme = "Catppuccin-Macchiato";
    #Also available: Catppuccin-Frappe Catppuccin-Latte Catppuccin-Macchiato Catppuccin-Mocha
    # See all available kitty themes at: https://github.com/kovidgoyal/kitty-themes/blob/46d9dfe230f315a6a0c62f4687f6b3da20fd05e4/themes.json
  };

}
