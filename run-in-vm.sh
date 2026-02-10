#!/usr/bin/env sh

export VMRUN="/Applications/VMware Fusion.app/Contents/Library/vmrun"; export VMX="$HOME/Virtual Machines.localized/macOS 14.vmwarevm/macOS 14.vmx"
"$VMRUN" -T fusion revertToSnapshot "$VMX" 'Snapshot 4'
"$VMRUN" -T fusion start "$VMX" gui
"$VMRUN" -T fusion -gu joel -gp password runScriptInGuest "$VMX" -interactive /bin/bash "osascript -e 'tell app \"Terminal\" to do script \"mkdir -p ~/testing; cd ~/testing; cp /Volumes/VMWare\\\ Shared\\\ Folders/vmwareshared/* .; ./wshs bootstrap just_nix.yml aeglos\"'"
