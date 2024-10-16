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
    python312Packages.pip
    python312Packages.flask
    python312Packages.pandas
    python312Packages.conda
    tmux
    btop
    polkit
    killall
    xscreensaver
    lxappearance
    systemctl-tui
    xorg.xev
    lazygit
    sox

    # dev
    monaspace
    vim
    arduino-ide
    unzip
    imagemagick
    jq
    hugo
    uv
    mdl # markdown lint

    # desktop
    i3lock-fancy-rapid
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
    # - medias
    transmission_4-qt
    freetube
    owncast
    calibre
    mpv
    yt-dlp
    mcomix
    yacreader

  ];

  home.file = {

    ".Xresources".source = dotfiles/Xresources;
    ".tmux.conf".source = dotfiles/tmux/tmux.conf;

    ".config/i3/config".source = dotfiles/i3/config;
    ".config/i3/tree" = {
      source = dotfiles/i3/tree;
      recursive = true;
    };

    ".config/rofi/config.rasi".source = dotfiles/rofi/config.rasi;
    ".config/kitty/launch.conf".source = dotfiles/kitty/launch.conf;

    ".config/doom" = {
      source = ./dotfiles/doom;
      recursive = true;
    };

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

  home.shellAliases = {
    lg = "lazygit";
  };

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      epkgs.nix-mode
      epkgs.magit
    ];
  };

  programs.firefox.enable = true;

  programs.home-manager = {
    enable = true;
  };

  programs.git = {
    enable = true;
    userName = "Alexandre Girard";
    userEmail = "git@alexgirard.com";
    lfs.enable = true;
  };

  programs.kitty = {
    enable = true;
    font = {
      name = "Monaspace Argon";
      size = 14;
    };
    theme = "Catppuccin-Macchiato";
    # Also available: Catppuccin-Frappe Catppuccin-Latte Catppuccin-Macchiato Catppuccin-Mocha
    # See all available kitty themes at: https://github.com/kovidgoyal/kitty-themes/blob/46d9dfe230f315a6a0c62f4687f6b3da20fd05e4/themes.json
  };

}
