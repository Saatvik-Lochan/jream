#!/usr/bin/env bash
# Copy this file into the directory which has the JREAM executeables
bench_dir=$1

set -o pipefail
set -e

if [ -z "$bench_dir" ]; then
    echo "Must pass in benchmark directory. Usage './run_mem.sh <bench_dir>'"
    exit 1
fi

ln -s -f "$bench_dir/merge.erl" ./
erlc ./merge.erl

beam="erl -noshell -eval 'merge:sort(), halt().' > out"
beam_single="erl +S 1 -noshell -eval 'merge:sort(), halt().' > out"
jream="./jream 'merge:sort()' > out"
jream_multi="./jream_multi 'merge:sort()' > out"

commands=("$beam" "$beam_single" "$jream" "$jream_multi")

: > mem_out

for n in 1 10 100 1000 10000 100000; do
  echo "Executing benchmarks for n = $n"
  python "$bench_dir/scripts/generate_reversed.py" list.txt $n
  echo "n = $n" >> mem_out

  for command in "${commands[@]}"; do
    echo "Executing command $command"
    echo "$command" >> mem_out
    for i in {1..10}; do
      echo -n "$i "
      (/usr/bin/time -v bash -c "$command") 2>&1 \
        | grep "Maximum resident set size" \
        | awk '{print $6}' >> mem_out
    done

    python "$bench_dir/scripts/check_sorted.py" out $n

    echo ""
  done
done
