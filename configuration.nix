# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nixos"; # Define your hostname.
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Asia/Bangkok";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.utf-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.utf-8";
    LC_IDENTIFICATION = "en_US.utf-8";
    LC_MEASUREMENT = "en_US.utf-8";
    LC_MONETARY = "en_US.utf-8";
    LC_NAME = "en_US.utf-8";
    LC_NUMERIC = "en_US.utf-8";
    LC_PAPER = "en_US.utf-8";
    LC_TELEPHONE = "en_US.utf-8";
    LC_TIME = "en_US.utf-8";
  };

  # https://nixos.wiki/wiki/I3
  # links /libexec from derivations to /run/current-system/sw
  environment.pathsToLink = [ "/libexec" ];

  # Workaround for GNOME autologin: https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229
  systemd.services."getty@tty1".enable = false;
  systemd.services."autovt@tty1".enable = false;

  services = {

    logind = {
      lidSwitch = "hibernate";
    };

    xserver = {
      enable = true;

      xkb = {
        layout = "fr";
        variant = "";
      };

      windowManager.i3 = {
        enable = true;
        extraPackages = with pkgs; [
          i3status
        ];
      };

      desktopManager = {
        xterm.enable = false;
        xfce = {
          enable = true;
          noDesktop = true;
          enableXfwm = false;
        };
      };

      displayManager = {
        lightdm.enable = true;
      };

    };

    # Inadvertent touchpad double-clicks
    # https://discourse.nixos.org/t/inadvertent-touchpad-double-clicks/21084/1
    libinput.touchpad.tapping = false;

    displayManager = {
      defaultSession = "xfce+i3";
      autoLogin = {
        enable = true;
        user = "alx";
      };
    };

    # Enable CUPS to print documents.
    printing = {
      enable = true;
    };

    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      # If you want to use JACK applications, uncomment this
      #jack.enable = true;

      # use the example session manager (no others are packaged yet so this is enabled by default,
      # no need to redefine it in your config for now)
      #media-session.enable = true;
    };

    emacs = {
      enable = true;
      package = pkgs.emacs;
    };
  };

  # Configure console keymap
  console.keyMap = "fr";

  # Enable sound with pipewire.
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.alx = {
    isNormalUser = true;
    description = "Alex";
    extraGroups = [ "networkmanager" "wheel" "docker" ];
    packages = with pkgs; [
    ];
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
  #  vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
  #  wget
    kitty
    kitty-themes
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
    localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?

  # Auto Hibernate on low battery
  # https://old.reddit.com/r/i3wm/comments/myg9pt/autohibernate_laptop_on_low_battery/
  #
  services.udev.extraRules = ''
    KERNEL=="ttyACM0", MODE:="666"
    SUBSYSTEM=="power_supply", ATTR{status}=="Discharging", ATTR{capacity}=="[0-5]", RUN+="${pkgs.systemd}/bin/systemctl hibernate"
  '';

  fonts.packages = with pkgs; [ (nerdfonts.override { fonts = [ "JetBrainsMono" ]; }) ];

  # enable the tailscale service
  services.tailscale.enable = true;

  # enable docker
  # virtualisation.docker.enable = true;

  environment.etc = {

    # How to enable global dark mode using Home manager in NixOS
    # https://discourse.nixos.org/t/how-to-enable-global-dark-mode-using-home-manager-in-nixos/28348/2
    "xdg/gtk-2.0/gtkrc".source = dotfiles/gtk/gtk2.0;
    "xdg/gtk-3.0/settings.ini".source = dotfiles/gtk/settings.ini;
    "xdg/gtk-4.0/settings.ini".source = dotfiles/gtk/settings.ini;

  };

}
