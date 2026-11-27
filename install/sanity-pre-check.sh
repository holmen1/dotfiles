#!/bin/sh

set -e

SSH_DIR=$HOME/.ssh
PRIVATE_KEY=$SSH_DIR/id_ed25519
PUBLIC_KEY=$PRIVATE_KEY.pub
OS=$(uname -s)

fail() {
    echo "Error: $1" >&2
    exit 1
}

file_mode() {
    case "$OS" in
        FreeBSD) stat -f '%Lp' "$1" ;;
        Linux) stat -c '%a' "$1" ;;
        *) fail "unsupported OS '$OS'" ;;
    esac
}

[ -d "$SSH_DIR" ] || fail "$SSH_DIR does not exist"
[ -f "$PRIVATE_KEY" ] || fail "$PRIVATE_KEY does not exist"
[ -f "$PUBLIC_KEY" ] || fail "$PUBLIC_KEY does not exist"

ssh_dir_mode=$(file_mode "$SSH_DIR") || fail "could not read $SSH_DIR permissions"
[ "$ssh_dir_mode" = 700 ] || fail "$SSH_DIR permissions are $ssh_dir_mode; run chmod 700 '$SSH_DIR'"

key_mode=$(file_mode "$PRIVATE_KEY") || fail "could not read private-key permissions"
case "$key_mode" in
    400|600) ;;
    *) fail "$PRIVATE_KEY permissions are $key_mode; run chmod 600 '$PRIVATE_KEY'" ;;
esac

if ! command -v ssh >/dev/null 2>&1; then
    fail "ssh is not installed"
fi

ssh_status=0
github_response=$(ssh -T -o BatchMode=yes -o ConnectTimeout=10 \
    -o IdentitiesOnly=yes -i "$PRIVATE_KEY" git@github.com 2>&1) || ssh_status=$?

case "$github_response" in
    *"successfully authenticated"*)
        echo "[ok] SSH key is recognized by GitHub"
        ;;
    *)
        echo "Error: GitHub did not authenticate $PRIVATE_KEY" >&2
        echo "SSH exited with status $ssh_status." >&2
        if [ -n "$github_response" ]; then
            printf '%s\n' "$github_response" >&2
        fi
        echo "If GitHub's host key is not trusted yet, run 'ssh -T git@github.com' and verify it." >&2
        echo "Confirm that $PUBLIC_KEY has been added to your GitHub account." >&2
        exit 1
        ;;
esac
