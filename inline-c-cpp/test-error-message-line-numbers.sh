#!/usr/bin/env bash
set -x
sed -i -e 's/.*uncomment this line.*//g' inline-c-cpp/test/tests.hs
stack test $@ inline-c-cpp >& error-log
cat error-log
grep -n 'Test this line' inline-c-cpp/test/tests.hs  | awk -F ':' '{print $1}' > exp
cat exp
grep 'tests.hs:[0-9]*:.*error' error-log | awk -F ':' '{print $2}' > out
cat out
set -xe
diff exp out
