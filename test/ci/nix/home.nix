{ pkgs, ... }:
{
  home.username = "runner";
  home.stateVersion = "24.11";

  # One tiny package so activation is observable and cheap. `verify.sh` checks
  # that `hello` landed in the home-manager profile.
  home.packages = [ pkgs.hello ];

  # Puts `home-manager` on PATH via the profile, which the property's checker
  # (`hasCmd' "home-manager"`) looks for on subsequent runs.
  programs.home-manager.enable = true;
}
