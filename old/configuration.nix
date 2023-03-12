# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  # Include the results of the hardware scan.
  imports = [ ./hardware-configuration.nix ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

  # Mount /tmp as tmpfs (doesn't use hard drive but RAM instead, will be cleared at every boot)
  boot.tmpOnTmpfs = true;

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  programs.fish.enable = true;

  services.xserver.enable = true;
  services.xserver.layout = "fr";
  services.xserver.xkbVariant = "bepo";
  services.xserver.xkbOptions = "grp:shifts_toggle,ctrl:nocaps,terminate:ctrl_alt_bksp,compose:ralt";
  services.xserver.windowManager.default = "i3";
  services.xserver.windowManager.i3.enable = true;
  services.xserver.displayManager.slim.defaultUser = "quentin";


  time.timeZone = "Europe/Brussels";
  networking.hostName = "nixos";
  # networking.wireless.enable = true;

  # Select internationalisation properties.

  environment.systemPackages = with pkgs; [
    # Console applications
    neovim tmux htop most git python
    # Graphical applications
    rxvt_unicode mpv llpp emacs
    # Web browser
    firefox
    # Other utilities
    unclutter i3lock
  ];

  services.openssh.enable = true;
  services.redshift.enable = true;
  services.redshift.latitude = "50.833";
  services.redshift.longitude = "4.333";

  # TODO: disable (temporary, for tests)
  networking.firewall.enable = false;

  # services.printing.enable = true;

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  # services.xserver.libinput.enable = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?

  users.users.quentin = {
    uid = 1000;
    isNormalUser = true;
    home = "/home/quentin";
    description = "Quentin";
    extraGroups = ["wheel" ];
    shell = pkgs.fish;
  };
}
