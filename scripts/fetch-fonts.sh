#!/usr/bin/env bash
#
# Downloads self-hosted webfonts into static/fonts/ (gitignored, regenerated
# on demand). Pinned to specific upstream releases for reproducibility; bump
# the tags below deliberately when upgrading a font.
#
# Both fonts are SIL Open Font License 1.1; each destination directory gets
# its own license file alongside the font files so the license travels with
# the redistributed copies, same as upstream requires.

set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.."

SOURCE_SERIF_TAG="4.005R"
JETBRAINS_MONO_TAG="v2.304"

DEST_SOURCE_SERIF="static/fonts/source-serif"
DEST_JETBRAINS_MONO="static/fonts/jetbrains-mono"

if [[ -f "$DEST_SOURCE_SERIF/SourceSerif4Variable-Roman.woff2" && -f "$DEST_JETBRAINS_MONO/JetBrainsMono-Regular.woff2" ]]; then
  echo "fetch-fonts: fonts already present, skipping (delete static/fonts/ to force a re-fetch)"
  exit 0
fi

WORKDIR="$(mktemp -d)"
trap 'rm -rf "$WORKDIR"' EXIT

echo "fetch-fonts: fetching Source Serif 4 ($SOURCE_SERIF_TAG)..."
mkdir -p "$DEST_SOURCE_SERIF"
curl -sL -o "$WORKDIR/source-serif.zip" \
  "https://github.com/adobe-fonts/source-serif/releases/download/${SOURCE_SERIF_TAG}/source-serif-4.005_WOFF2.zip"
unzip -qq -j -o "$WORKDIR/source-serif.zip" \
  "source-serif-4.005_WOFF2/VAR/SourceSerif4Variable-Roman.otf.woff2" \
  "source-serif-4.005_WOFF2/VAR/SourceSerif4Variable-Italic.otf.woff2" \
  -d "$DEST_SOURCE_SERIF"
mv "$DEST_SOURCE_SERIF/SourceSerif4Variable-Roman.otf.woff2" "$DEST_SOURCE_SERIF/SourceSerif4Variable-Roman.woff2"
mv "$DEST_SOURCE_SERIF/SourceSerif4Variable-Italic.otf.woff2" "$DEST_SOURCE_SERIF/SourceSerif4Variable-Italic.woff2"
curl -sL -o "$DEST_SOURCE_SERIF/LICENSE.md" \
  "https://raw.githubusercontent.com/adobe-fonts/source-serif/${SOURCE_SERIF_TAG}/LICENSE.md"

echo "fetch-fonts: fetching JetBrains Mono ($JETBRAINS_MONO_TAG)..."
mkdir -p "$DEST_JETBRAINS_MONO"
curl -sL -o "$WORKDIR/jetbrains-mono.zip" \
  "https://github.com/JetBrains/JetBrainsMono/releases/download/${JETBRAINS_MONO_TAG}/JetBrainsMono-2.304.zip"
unzip -qq -j -o "$WORKDIR/jetbrains-mono.zip" \
  "fonts/webfonts/JetBrainsMono-Regular.woff2" \
  "fonts/webfonts/JetBrainsMono-Bold.woff2" \
  "fonts/webfonts/JetBrainsMono-Italic.woff2" \
  "fonts/webfonts/JetBrainsMono-BoldItalic.woff2" \
  -d "$DEST_JETBRAINS_MONO"
curl -sL -o "$DEST_JETBRAINS_MONO/OFL.txt" \
  "https://raw.githubusercontent.com/JetBrains/JetBrainsMono/${JETBRAINS_MONO_TAG}/OFL.txt"

echo "fetch-fonts: done"
