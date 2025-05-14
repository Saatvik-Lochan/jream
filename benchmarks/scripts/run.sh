bench_dir=$1

if [ -z "$bench_dir" ]; then
    echo "Must pass in benchmark directory. Usage './run.sh <bench_dir>'"
    exit 1
fi

# gray bench
echo "Executing gray benchmark"
ln -s -f "$bench_dir/gray.erl" ./
erlc ./gray.erl
hyperfine --runs 10 --warmup 1 \
    -L n 100,1_000,10_000,100_000,1_000_000,10_000_000 \
    --export-json="gray_timing.json" \
    "./jream 'gray:main({n})'" \
    "./jream_multi 'gray:main({n})'" \
    "erl -noshell -eval 'gray:main({n}), halt().'" \
    "erl +S 1 -noshell -eval 'gray:main({n}), halt().'"

# fib bench
echo "Executing fib benchmark"
ln -s -f "$bench_dir/fib.erl" ./
erlc ./fib.erl
hyperfine --runs 10 --warmup 1 \
    -L n 15,20,25,30,35,40 \
    --export-json="fib_timing.json" \
    "./jream 'fib:fib({n})'" \
    "./jream_multi 'fib:fib({n})'" \
    "erl -noshell -eval 'fib:fib({n}), halt().'" \
    "erl +S 1 -noshell -eval 'fib:fib({n}), halt().'"

# merge sort bench
echo "Executing merge sort benchmark"
ln -s -f "$bench_dir/merge.erl" ./
erlc merge.erl
hyperfine --runs 15 --warmup 1 \
    -L n 10,100,1000,10000,100000,1000000 \
    --export-json="merge_sort_timing.json" \
    --setup "python $bench_dir/scripts/generate_reversed.py list.txt {n}" \
    "./jream 'merge:sort()'" \
    "./jream_multi 'merge:sort()'" \
    "./jream_no_generational 'merge:sort()'" \
    "erl -noshell -eval 'merge:sort(), halt().'" \
    "erl +S 1 -noshell -eval 'merge:sort(), halt().'"

# ring bench
echo "Executing ring benchmark"
ln -s -f "$bench_dir/ring.erl" ./
erlc ring.erl
hyperfine --runs 10 --warmup 1 \
    -L num_procs 1,10,100,1000,10000 -L num_rounds 100 \
    --export-json="ring_timing.json" \
    "./jream 'ring:run({num_rounds}, {num_procs})'" \
    "./jream_multi 'ring:run({num_rounds}, {num_procs})'" \
    "erl -noshell -eval 'ring:run({num_rounds}, {num_procs}), halt().'" \
    "erl +S 1 -noshell -eval 'ring:run({num_rounds}, {num_procs}), halt().'" \

# long parallel bench
echo "Executing long parallel benchmark"
ln -s -f "$bench_dir/parallel.erl" ./
erlc parallel.erl
hyperfine --runs 15 --warmup 1 \
    --export-json="parallel_timing.json" \
    -L n 15,20,25,30,35 \
    "./jream 'parallel:fibp(8, {n})'" \
    "./jream_multi 'parallel:fibp(8, {n})'" \
    "erl -noshell -eval 'parallel:fibp(8, {n}), halt().'" \
    "erl +S 1 -noshell -eval 'parallel:fibp(8, {n}), halt().'" \
