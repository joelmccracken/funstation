#!/usr/bin/env bash
# Prepare the hermetic fixtures the CI bootstrap consumes, entirely under
# ~/funstation-ci (no network, no external repo, no credentials):
#
#   ~/funstation-ci/src   a normal git repo GitHomeDir fetches from (its remoteUrl)
#   ~/funstation-ci/home  a dedicated EMPTY work tree GitHomeDir checks out into
#   ~/funstation-ci/Brewfile  copy of the Brewfile at a literal path brew can read
#
# Run from the repo root before `fun bootstrap`.
set -euo pipefail

BASE="$HOME/funstation-ci"
SRC="$BASE/src"
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

rm -rf "$BASE"
mkdir -p "$BASE/home"

# --- source repo for GitHomeDir ------------------------------------------------
git init -q -b main "$SRC"
git -C "$SRC" config user.email "ci@funstation.example"
git -C "$SRC" config user.name  "Funstation CI"

printf '# funstation ci tracked dotfile A\n' > "$SRC/.ci-tracked-a"
mkdir -p "$SRC/sub"
printf '# funstation ci tracked dotfile B (nested)\n' > "$SRC/sub/.ci-tracked-b"

git -C "$SRC" add -A
git -C "$SRC" commit -q -m "funstation ci fixtures"
# So a plain local-path/file:// remote can be fetched from.
git -C "$SRC" update-server-info

# --- Brewfile at a literal path (HomebrewBundle.brewfile isn't expandPath'd) ---
cp "$HERE/fixtures/Brewfile" "$BASE/Brewfile"

echo "Prepared fixtures under $BASE:"
ls -la "$BASE"
echo "Tracked files in $SRC:"
git -C "$SRC" ls-tree -r --name-only HEAD
