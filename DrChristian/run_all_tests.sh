#!/bin/zsh

BOUND=2000000000
OUTDIR="results_${BOUND}"

mkdir -p "$OUTDIR"

echo "Running all tests with bound = $BOUND"
echo "Results will be saved in $OUTDIR"
echo

echo "1. PushGen sequential"
/usr/bin/time -l scala-cli shifts09.scala --main-class measureS -- "$BOUND" \
  2>&1 | tee "$OUTDIR/testallS_pushgen_sequential.txt"

echo
echo "2. Decode sequential"
/usr/bin/time -l scala-cli shifts09.scala --main-class measureDS -- "$BOUND" \
  2>&1 | tee "$OUTDIR/testallDS_decode_sequential.txt"

echo
echo "3. Decode parallel"
/usr/bin/time -l scala-cli shifts09.scala --main-class measureDP -- "$BOUND" \
  2>&1 | tee "$OUTDIR/testallDP_decode_parallel.txt"

echo
echo "All tests finished."