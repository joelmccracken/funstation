#!/usr/bin/env bash


set -x

# STARTING_SNAPSHOT='Snapshot 4'
# STARTING_SNAPSHOT='post-vmwaretools-setup-sharing-works'
# STARTING_SNAPSHOT='post-side-channel-mitigations-disabled'
# STARTING_SNAPSHOT='after-allow-vmware-tools-permissions'
STARTING_SNAPSHOT='after-terminal-full-disk-access'
# nix build


export VMRUN="/Applications/VMware Fusion.app/Contents/Library/vmrun";
export VMX="$HOME/Virtual Machines.localized/macOS_14.vmwarevm/macOS_14.vmx";
export DIR="vmwareshared/run-in-vm-dir"
export HOSTDIR="$HOME/$DIR"
mkdir -p "$HOSTDIR"
for f in $HOSTDIR/*; do rm -f $f; done # bypassing that wshs ends up being write protected, there must be a better way to do this
cp ./result/bin/wshs test/sample_configs/just_nix.yml "$HOSTDIR"

echo password > "$HOSTDIR"/sudo-pass

"$VMRUN" -T fusion revertToSnapshot "$VMX" "$STARTING_SNAPSHOT";
"$VMRUN" -T fusion start "$VMX" gui;
"$VMRUN" -T fusion -gu user -gp password runScriptInGuest "$VMX" -interactive /bin/bash \
    "set -x; osascript -e 'tell app \"Terminal\" to do script \"mkdir -p ~/testing; cd ~/testing; cp /Volumes/VMWare\\\ Shared\\\ Folders/vmwareshared/run-in-vm-dir/* .; (./wshs --sudo-cache --sudo-pass-file ./sudo-pass bootstrap just_nix.yml aeglos 2>&1 | tee /Volumes/VMWare\\\ Shared\\\ Folders/vmwareshared/run-in-vm-dir/output.log); touch /Volumes/VMWare\\\ Shared\\\ Folders/vmwareshared/run-in-vm-dir/done\"'"

LOGFILE="$HOSTDIR/output.log"
DONEFILE="$HOSTDIR/done"

set +x
# wait for guest to start writing
while [ ! -f "$LOGFILE" ]; do sleep 1; done
set -x

tail -f "$LOGFILE" &
TAIL_PID=$!

set +x
# wait for guest to signal completion
while [ ! -f "$DONEFILE" ]; do sleep 2; done
set -x
kill $TAIL_PID
echo "--- VM run complete ---"

