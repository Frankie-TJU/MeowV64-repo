#!/bin/bash
set -e
mkdir -p build
cp ./submodules/rocket-chip/src/main/resources/vsrc/EICG_wrapper.v build/
CONFIG=${CONFIG:-meowv64.rocket.MeowV64SingleCoreConfig}
mill meowv64.runMain \
	freechips.rocketchip.system.Generator \
	-td build \
	-C ${CONFIG} \
	-T meowv64.rocket.RiscVSystem
mill meowv64.runMain firrtl.stage.FirrtlMain \
	-td build \
	-i build/${CONFIG}.fir \
	-o ${CONFIG}.v \
	-X verilog \
	-faf build/${CONFIG}.anno.json \
	-firw -gmv full
