#!/bin/sh

function regenerateSeries()
{
  for f in `ls $1/*.hbt`; do
    DEST=`basename -s .hbt $f`.gld
    echo "Rebuilding $1/${DEST}"
    ./dist/build/hshabit/hshabit $2 $f > $1/${DEST}
  done
}

regenerateSeries "tests/scanner"    "lex"
regenerateSeries "tests/indentinfo" "lex -i"
regenerateSeries "tests/lexer"      "lex -e"
regenerateSeries "tests/parser"     "parse"
