#!/bin/bash

pin -t obj-intel64/indirectionProfiler.so -- ./test &
PID=$!
ln -fs $!.CallProfile.sql out.sql
ln -fs $!.CallProfile.log CallProfile.log
ln -fs $!.AddressMap.log AddressMap.log
wait
