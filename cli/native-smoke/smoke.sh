#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "usage: smoke.sh <binary> <expected-version>" >&2
  exit 2
fi

bin=$1
expected=$2
root=$(cd "$(dirname "$0")/../.." && pwd)
fixtures="$root/cli/native-smoke/fixtures"

if [[ ! -f "$bin" ]]; then
  echo "binary not found: $bin" >&2
  exit 1
fi

if [[ -z "$expected" || "$expected" == "(dev)" ]]; then
  echo "expected version must be non-empty and not (dev), got '$expected'" >&2
  exit 1
fi

workdir=$(mktemp -d)
trap 'rm -rf "$workdir"' EXIT
export XDG_CONFIG_HOME="$workdir/xdg"
mkdir -p "$XDG_CONFIG_HOME/atbp"
printf '%s\n' 'atbp {' '}' > "$XDG_CONFIG_HOME/atbp/application.conf"

actual=$("$bin" --version)
if [[ "$actual" != "$expected" ]]; then
  echo "--version printed '$actual', expected '$expected'" >&2
  exit 1
fi

help_one() {
  if ! "$bin" "$@" --help >/dev/null; then
    echo "help failed: $bin $* --help" >&2
    exit 1
  fi
}

help_one
help_one hubad
help_one liga
help_one liga leaderboard
help_one liga handicap
help_one liga serve
help_one md2c
help_one mdmerge
help_one plate
help_one plate label
help_one plate check
help_one plate radar
help_one plate sort
help_one retext
help_one stmt2csv
help_one traceviz

retext_out="$workdir/retext"
mkdir -p "$retext_out"
"$bin" retext --target "$retext_out" "$fixtures/note.md"
if [[ ! -f "$retext_out/note.md" ]] || ! grep -q 'native-smoke note' "$retext_out/note.md"; then
  echo "retext did not write the fixture text" >&2
  exit 1
fi

text_out="$workdir/hello.txt"
"$bin" stmt2csv --text --target "$text_out" "$fixtures/hello.pdf"
if [[ ! -f "$text_out" ]] || ! grep -q 'native-smoke' "$text_out"; then
  echo "stmt2csv --text did not write native-smoke" >&2
  exit 1
fi

diagram_out="$workdir/diagrams"
mkdir -p "$diagram_out"
log="$workdir/md2c.log"
set +e
"$bin" md2c --render-only --target "$diagram_out" "$fixtures/diagrams" >"$log" 2>&1
status=$?
set -e
cat "$log" >&2
if [[ $status -ne 0 ]]; then
  echo "md2c --render-only failed" >&2
  exit 1
fi

for bad in "Mermaid rendering failed" "D2 rendering failed" "d2 not on PATH"; do
  if grep -q "$bad" "$log"; then
    echo "render log contains '$bad'" >&2
    exit 1
  fi
done

for img in plantuml.png mermaid.png d2.png; do
  if [[ ! -s "$diagram_out/$img" ]]; then
    echo "missing or empty $diagram_out/$img" >&2
    exit 1
  fi
done
