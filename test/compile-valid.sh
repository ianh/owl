#! /bin/bash

: "${CC:=$(which cc)}"

test_dir="$(cd "$(dirname "$0")" && pwd -P)"

cd $test_dir

for i in *.owl; do
  ../owl -c "$i" -o "results/$i.h" 2> /dev/null
  if [ -e "results/$i.h" ]; then
    echo -e "#define OWL_PARSER_IMPLEMENTATION\n#include \"$i.h\"\n" > "results/$i.c"
    pushd results &> /dev/null
    $CC -c "$i.c" 2> "$i.cc.stderr"
    rm "$i."{c,h,o} &> /dev/null
    popd &> /dev/null
  fi
done
