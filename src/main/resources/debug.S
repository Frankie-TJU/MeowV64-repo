	.text
entry:
	# save a0 & a1
	csrw dscratch0, a0
	csrw dscratch1, a1

	# read command from dm
	li a0, 0x1000
loop:
	ld a1, 0(a0)
	beq a1, zero, loop

exit:
	csrr a0, dscratch0
	csrr a1, dscratch1
	dret


