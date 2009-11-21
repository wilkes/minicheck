#!/bin/sh
lein clean && lein compile && lein install
cd lein-minicheck
lein clean && lein deps && lein compile && lein install
cd ..
lein deps
