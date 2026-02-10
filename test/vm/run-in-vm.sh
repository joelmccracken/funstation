#!/usr/bin/env sh

STARTING_SNAPSHOT='Snapshot 4'

nix build


export VMRUN="/Applications/VMware Fusion.app/Contents/Library/vmrun";
export VMX="$HOME/Virtual Machines.localized/macOS 14.vmwarevm/macOS 14.vmx";
export DIR="vmwareshared/run-in-vm-dir"
export HOSTDIR="$HOME/DIR"
export GUESTDIR="/Volumes/VMWare\\\ Shared\\\ Folders/$DIR"
mkdir -p "$HOSTDIR"
for f in $HOSTDIR/*; do rm -f $f; done
cp ./result/bin/wshs test/sample_configs/just_nix.yml "$DIR"

"$VMRUN" -T fusion revertToSnapshot "$VMX" "$STARTING_SNAPSHOT";
"$VMRUN" -T fusion start "$VMX" gui;
"$VMRUN" -T fusion -gu joel -gp password runScriptInGuest "$VMX" -interactive /bin/bash \
    "osascript -e 'tell app \"Terminal\" to do script \"mkdir -p ~/testing; cd ~/testing; cp '$GUESTDIR'/* .; ./wshs --nopasswd bootstrap just_nix.yml aeglos\"'"
