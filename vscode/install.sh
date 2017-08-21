set -eu

target="$1/.vscode/extensions/deep-sister/"

npm install
tsc
rm -rf $target
cp -r . $target
