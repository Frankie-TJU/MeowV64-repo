	.text
entry:
	// save a0 & a1
	csrw dscratch0, a0
	csrw dscratch1, a1

	// read command from dm
	li a0, 0x1000
loop:
	ld a1, 0(a0)
	beq a1, zero, loop

check_action:
	li a0, 1
	bne a0, a1, check_resume
	// if a1 == 1
	// received abstract action
	// clear icache
	fence.i
	// jump to 0x20000
	li a0, 0x1000
	li a1, 0x20000
	jr 0(a1)

check_resume:
	// finish & exit
	li a0, 0x1000
	sw zero, 4(a0)
	j exit

exit:
	csrr a0, dscratch0
	csrr a1, dscratch1
	dret


