#!/bin/sh
# Download the test data of web-platform-tests and the Public Suffix List,
# and the files of the Unicode Character Database that the tests and the
# tables are made from, at the versions that the library follows, into
# dev-resources/.
set -eu

wpt=b1a3daad996272add733ff1c420bf48049a5b698
unicode=18.0.0
psl=3929462652695bad04f0a27afb600974014a3c8b

cd "$(dirname "$0")/.."
mkdir -p dev-resources/wpt dev-resources/unicode dev-resources/psl

for f in urltestdata.json urltestdata-javascript-only.json setters_tests.json \
         toascii.json IdnaTestV2.json IdnaTestV2-removed.json percent-encoding.json; do
  curl -sSfL -o "dev-resources/wpt/$f" \
    "https://raw.githubusercontent.com/web-platform-tests/wpt/$wpt/url/resources/$f"
done

for f in idna/IdnaMappingTable.txt idna/IdnaTestV2.txt ucd/UnicodeData.txt \
         ucd/extracted/DerivedBidiClass.txt ucd/extracted/DerivedJoiningType.txt \
         ucd/extracted/DerivedGeneralCategory.txt ucd/extracted/DerivedCombiningClass.txt; do
  curl -sSfL -o "dev-resources/unicode/$(basename "$f")" \
    "https://www.unicode.org/Public/$unicode/$f"
done

for f in public_suffix_list.dat tests/test_psl.txt; do
  curl -sSfL -o "dev-resources/psl/$(basename "$f")" \
    "https://raw.githubusercontent.com/publicsuffix/list/$psl/$f"
done
