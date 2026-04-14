#!/bin/sh
#./usr/local/bin/X :1 vt8 &
X :1 vt8 &
_pid=$!
sleep 10 && kill $_pid

