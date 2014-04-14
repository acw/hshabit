#!/bin/sh

for f in `ls tests/scanner/*.hbt`; do
  DEST=`basename -s .hbt $f`
  echo "Rebuilding tests/scanner/${DEST}"
  ./dist/build/hshabit/hshabit lex $f > ${DEST}
done

for f in `ls tests/indentinfo/*.hbt`; do
  DEST=`basename -s .hbt $f`
  echo "Rebuilding tests/indentinfo/${DEST}"
  ./dist/build/hshabit/hshabit lex -i $f > ${DEST}
done

for f in `ls tests/lexer/*.hbt`; do
  DEST=`basename -s .hbt $f`
  echo "Rebuilding tests/lexer/${DEST}"
  ./dist/build/hshabit/hshabit lex -e $f > ${DEST}
done
