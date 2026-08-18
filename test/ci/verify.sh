#!/usr/bin/env bash
# Verify that `fun bootstrap` actually converged the system to test/ci/config.yml.
# Run from the repo root, after the bootstrap.
set -uo pipefail

BASE="$HOME/funstation-ci"
WORKSPACE="${GITHUB_WORKSPACE:-$(pwd)}"
FIXTURES="$WORKSPACE/test/ci/fixtures/dotfiles"

case "$(uname -s)" in
  Darwin) IS_MAC=1 ;;
  *)      IS_MAC=0 ;;
esac

# GitHub run: steps are non-login shells, so nix not on PATH yet
# Source daemon profile so nix stuff can be found
# Relax `-u` since the profile scripts touch unset vars.
set +u
for profile in \
  /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh \
  /etc/profile.d/nix.sh; do
  # shellcheck disable=SC1090
  [ -e "$profile" ] && . "$profile"
done
set -u
[ -d "$HOME/.nix-profile/bin" ] && PATH="$HOME/.nix-profile/bin:$PATH"

FAILS=0
pass() { echo "  ✓ $1"; }
fail() { echo "  ✗ $1"; FAILS=$((FAILS + 1)); }
check() { # check <description> <command...>
  local desc="$1"; shift
  if "$@" >/dev/null 2>&1; then pass "$desc"; else fail "$desc"; fi
}

echo "== HasGit =="
check "git is installed" command -v git

echo "== NixDaemon =="
check "nix is installed" command -v nix
check "nix daemon responds (nix store ping)" nix store ping
check "/etc/nix/nix.conf has the funstation-ci marker" \
  grep -q "funstation-ci-marker" /etc/nix/nix.conf
check "/etc/nix/nix.conf enables flakes" \
  grep -q "experimental-features = nix-command flakes" /etc/nix/nix.conf

echo "== GitHomeDir =="
check "bare git dir exists" test -d "$BASE/git-dir"
if command -v git >/dev/null 2>&1; then
  origin="$(git --git-dir "$BASE/git-dir" remote get-url origin 2>/dev/null || true)"
  if [ "$origin" = "$BASE/src" ]; then
    pass "origin remote points at the source repo ($origin)"
  else
    fail "origin remote is '$origin', expected '$BASE/src'"
  fi
fi
check "tracked file .ci-tracked-a checked out into home" \
  test -f "$BASE/home/.ci-tracked-a"
check "nested tracked file sub/.ci-tracked-b checked out into home" \
  test -f "$BASE/home/sub/.ci-tracked-b"

echo "== Dotfiles =="
DEST="$BASE/dotfiles-dest"
# .bashrc is managed as a symlink
if [ -L "$DEST/.bashrc" ] && [ -r "$DEST/.bashrc" ]; then
  if grep -q "FUNSTATION_CI=1" "$DEST/.bashrc"; then
    pass ".bashrc is a symlink resolving to the fixture"
  else
    fail ".bashrc symlink does not resolve to expected content"
  fi
else
  fail ".bashrc is not a readable symlink"
fi
# .vimrc is managed as a copy (regular file, not a symlink)
if [ -f "$DEST/.vimrc" ] && [ ! -L "$DEST/.vimrc" ]; then
  if diff -q "$FIXTURES/vimrc" "$DEST/.vimrc" >/dev/null 2>&1; then
    pass ".vimrc is a copy matching the fixture"
  else
    fail ".vimrc content does not match the fixture"
  fi
else
  fail ".vimrc is not a regular (non-symlink) file"
fi

echo "== HomeManager =="
hm_found=0
for p in \
  "$HOME/.nix-profile/bin/hello" \
  "$HOME/.local/state/nix/profiles/home-manager/home-path/bin/hello" \
  "$HOME/.local/state/home-manager/gcroots/current-home/home-path/bin/hello"; do
  if [ -e "$p" ]; then hm_found=1; hm_path="$p"; break; fi
done
if [ "$hm_found" -eq 1 ]; then
  pass "home-manager activated; hello present ($hm_path)"
else
  fail "home-manager package 'hello' not found in any known profile path"
fi
check "home-manager profile generation exists" \
  bash -c 'ls "$HOME"/.local/state/nix/profiles/home-manager* >/dev/null 2>&1 || ls "$HOME"/.local/state/home-manager/gcroots/current-home >/dev/null 2>&1'

if [ "$IS_MAC" -eq 1 ]; then
  echo "== HomebrewBundle (macOS) =="
  check "brew formula 'hello' is installed" brew list hello
fi

echo
if [ "$FAILS" -eq 0 ]; then
  echo "ALL STATE CHECKS PASSED"
  exit 0
else
  echo "STATE VERIFICATION FAILED: $FAILS check(s) did not pass"
  exit 1
fi
