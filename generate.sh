#!/bin/bash
set -e
mkdir -p build
git clean -fdx build
cp ./submodules/rocket-chip/src/main/resources/vsrc/EICG_wrapper.v build/
mill meowv64.runMain \
	freechips.rocketchip.system.Generator \
	-td build \
	-C meowv64.rocket.MeowV64Config \
	-T meowv64.rocket.RiscVSystem
mill meowv64.runMain firrtl.stage.FirrtlMain \
	-td build \
	-i build/meowv64.rocket.MeowV64Config.fir \
	-o meowv64.rocket.MeowV64Config.v \
	-X verilog \
	-faf build/meowv64.rocket.MeowV64Config.anno.json
