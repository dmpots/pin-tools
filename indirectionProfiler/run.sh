#!/bin/bash
PIN=../../../../pin
OBJDIR=obj-ia32

$PIN -t $OBJDIR/indirectionProfiler.so -- ./test.haskell &
PID=$!
wait
#ln -f $!.CallProfile.sql out.sql
cp -f $PID.CallProfile.log CallProfile.log
cp -f $PID.AddressMap.log AddressMap.log
./addressMapReader AddressMap.log CallProfile.log > AnnotatedCallProfile.log
