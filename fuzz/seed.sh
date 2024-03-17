#!/bin/sh
# From @simdjson/simdjson.
#
# Builds a corpus from all small .paw files in the source directory.
# The files are renamed to the sha1 of their content, and suffixed
# .paw. Results are zipped into a flat file named corpus.zip

set -eu

tmp=$(mktemp -d)

root=$(readlink -f "$(dirname "$0")/..")

find $root -type f -size -4k -name "*.paw" | while read -r paw; do
 cp "$paw" "$tmp"/$(sha1sum < "$paw" |cut -f1 -d' ').paw
done

zip --quiet --junk-paths -r corpus.zip "$tmp"

