#!/bin/bash
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

cd $SCRIPT_DIR
zig build && \
../writing-a-c-compiler-tests/test_compiler $SCRIPT_DIR/zig-out/bin/jcc-driver --verbose --failfast --bitwise --compound --increment $@
