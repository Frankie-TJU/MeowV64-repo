#!/bin/bash
set -e
CONFIG=${CONFIG:-meowv64.rocket.MeowV64SingleCoreConfig}
DEST=build/${CONFIG}
mkdir -p ${DEST}
cp ./submodules/rocket-chip/src/main/resources/vsrc/EICG_wrapper.v ${DEST}
cp ./submodules/rocket-chip/src/main/resources/vsrc/plusarg_reader.v ${DEST}
rm -f ${DEST}/RiscVSystem.fir ${DEST}/RiscVSystem.anno.json
mill meowv64.runMain \
	freechips.rocketchip.diplomacy.Main \
	--dir $PWD/${DEST} \
	--config ${CONFIG} \
	--top meowv64.rocket.RiscVSystem
firtool --lowering-options=disallowLocalVariables,mitigateVivadoArrayIndexConstPropBug,disallowPackedStructAssignments --lower-memories --disable-all-randomization ${DEST}/RiscVSystem.fir -o ${DEST}/${CONFIG}.v
