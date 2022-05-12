#!/bin/bash
set -e
mkdir -p build
rm -rf build/*
mill rocketChip.runMain \
	freechips.rocketchip.system.Generator \
	-td build \
	-C freechips.rocketchip.system.DefaultSmallConfig \
	-T freechips.rocketchip.system.TestHarness
mill meowv64.runMain firrtl.stage.FirrtlMain \
	-td build \
	-i build/freechips.rocketchip.system.DefaultSmallConfig.fir \
	-o freechips.rocketchip.system.DefaultSmallConfig.v \
	-X verilog \
	-faf build/freechips.rocketchip.system.DefaultSmallConfig.anno.json