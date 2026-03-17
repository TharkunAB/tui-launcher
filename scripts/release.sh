#!/bin/bash
set -euo pipefail

PACKAGE="tui-launcher"

usage() {
  echo "Usage: $0 [--publish]"
  echo ""
  echo "Without --publish, uploads as a package candidate (dry run)."
  echo "With --publish, uploads as a published release."
  exit 1
}

publish_flag=""
for arg in "$@"; do
  case "$arg" in
    --publish) publish_flag="--publish" ;;
    -h|--help) usage ;;
    *) echo "Error: unknown argument '$arg'" ; usage ;;
  esac
done

rm -f dist-newstyle/"$PACKAGE"-[0-9]*-docs.tar.gz
rm -f dist-newstyle/sdist/"$PACKAGE"-[0-9]*.tar.gz

cabal haddock --haddock-for-hackage --haddock-executables
cabal sdist

read -p "Username: " username
read -sp "Password: " password
echo ""

echo ""
echo "=== Releasing $PACKAGE ==="

cabal upload $publish_flag -u "$username" -p "$password" dist-newstyle/sdist/"$PACKAGE"-[0-9]*.tar.gz
cabal upload $publish_flag -d -u "$username" -p "$password" dist-newstyle/"$PACKAGE"-[0-9]*-docs.tar.gz

echo "=== Done: $PACKAGE ==="
