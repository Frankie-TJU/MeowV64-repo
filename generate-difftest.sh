#!/bin/bash
set -e
CONFIG=${CONFIG:-meowv64.rocket.MeowV64DifftestConfig}
DEST=build/${CONFIG}
mkdir -p ${DEST}
cp ./submodules/rocket-chip/src/main/resources/vsrc/EICG_wrapper.v ${DEST}
mill meowv64.runMain \
	freechips.rocketchip.system.Generator \
	-td ${DEST} \
	-C ${CONFIG} \
	-T meowv64.rocket.RiscVSystem
mill meowv64.runMain firrtl.stage.FirrtlMain \
	-td ${DEST} \
	-i ${DEST}/${CONFIG}.fir \
	-o ${CONFIG}.v \
	-X verilog \
	-faf ${DEST}/${CONFIG}.anno.json \
	-firw -gmv full
