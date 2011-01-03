#!/bin/bash
PIN=../../../../pin
OBJDIR=obj-ia32

echo == STARTING ==
$PIN -t $OBJDIR/trace.so -- ./test &
PID=$!
wait
echo == FINISHED ==
echo See output in file "trace.out"
