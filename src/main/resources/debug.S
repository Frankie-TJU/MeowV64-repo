	.text
	// exception vectors
	.org 0x4
ebreak_trap:
	j park_loop

	.org 0xc
haltreq_trap:
	j enter_debug

	.org 0x10
step_trap:
	j enter_debug

enter_debug:
	// enter debug mode
	// save a0 & a1
	csrw dscratch0, a0
	csrw dscratch1, a1

park_loop:
	// read command from dm
	li a0, 0x1000
loop:
	ld a1, 0(a0)
	beq a1, zero, loop

check_action:
	li a0, 1
	blt a0, a1, check_resume
	// if a1 == 1
	// received abstract action
	// clear icache
	fence.i
	// jump to 0x20000
	li a0, 0x1000
	li a1, 0x20000
	jr 0(a1)

check_resume:
	li a0, 2
	blt a0, a1, check_memory
	// resume if a1 == 2
	// finish & exit
	li a0, 0x1000
	sw zero, 4(a0)
	j exit

check_memory:
	// memory access if a1 == 3
	// read write
	li a0, 0x1000
	lw a1, 8(a0)
	// extract write bit
	bne a1, zero, memory_write

	// arg0 @ 0x0-0x8, arg1 @ 0x8-0x10
memory_read:
	// arg0 = mem[arg1]
	// read aamsize
	lw a1, 0xc(a0)
	li a0, 0
	beq a1, a0, memory_read_b
	li a0, 1
	beq a1, a0, memory_read_h
	li a0, 2
	beq a1, a0, memory_read_w
	li a0, 3
	beq a1, a0, memory_read_d

memory_read_b:
	ld a0, 8(zero)
	lb a0, 0(a0)
	sd a0, 0(zero)
	j memory_postincrement
memory_read_h:
	ld a0, 8(zero)
	lh a0, 0(a0)
	sd a0, 0(zero)
	j memory_postincrement
memory_read_w:
	ld a0, 8(zero)
	lw a0, 0(a0)
	sd a0, 0(zero)
	j memory_postincrement
memory_read_d:
	ld a0, 8(zero)
	ld a0, 0(a0)
	sd a0, 0(zero)
	j memory_postincrement

memory_write:
	// mem[arg1] = arg0
	// read aamsize
	lw a1, 0xc(a0)
	li a0, 0
	beq a1, a0, memory_write_b
	li a0, 1
	beq a1, a0, memory_write_h
	li a0, 2
	beq a1, a0, memory_write_w
	li a0, 3
	beq a1, a0, memory_write_d

memory_write_b:
	ld a0, 0(zero)
	ld a1, 8(zero)
	sb a0, 0(a1)
	j memory_postincrement
memory_write_h:
	ld a0, 0(zero)
	ld a1, 8(zero)
	sh a0, 0(a1)
	j memory_postincrement
memory_write_w:
	ld a0, 0(zero)
	ld a1, 8(zero)
	sw a0, 0(a1)
	j memory_postincrement
memory_write_d:
	ld a0, 0(zero)
	ld a1, 8(zero)
	sd a0, 0(a1)
	j memory_postincrement

memory_postincrement:
	// read aampostincrement
	li a0, 0x1000
	lw a1, 0x10(a0)
	beq a1, zero, memory_done

	// arg1 += (1 << aamsize)
	// read aamsize
	lw a1, 0xc(a0)
	li a0, 1
	sll a0, a0, a1

	// read arg1 & update
	ld a1, 0x8(zero)
	add a1, a1, a0
	sd a1, 0x8(zero)

memory_done:
	// complete
	li a0, 0x1000
	sw zero, 4(a0)
	ebreak

exit:
	csrr a0, dscratch0
	csrr a1, dscratch1
	dret


