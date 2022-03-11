	.text
	// exception vectors
	.org 0x0
	// from non-debug mode to debug mode
enter_debug_mode_trap:
	j enter_debug
	
	.org 0x4
	// ebreak in debug mode
ebreak_trap:
	j success
	
	.org 0x8
	// unexpected exception in debug mode
exception_trap:
	j handle_exception

handle_exception:
	// write 1 to 0x104
	// signals exception in abstract command
	li a1, 1
	sw a1, 0x104(zero)
	j park_loop

enter_debug:
	// enter debug mode
	// save a0 & a1
	csrw dscratch0, a0
	csrw dscratch1, a1

park_loop:
	// read command from dm
loop:
	ld a1, 0x100(zero)
	beq a1, zero, loop

check_action:
	li a0, 1
	blt a0, a1, check_resume
	// if a1 == 1
	// received abstract action

	// check program ram dirty
	ld a1, 0x114(zero)
	beqz a1, skip_clear_icache
	
	// clear icache
	fence.i
	// no more dirty
	sd zero, 0x114(zero)

skip_clear_icache:
	// jump to 0x20000
	j 0x20000

check_resume:
	li a0, 2
	blt a0, a1, check_memory
	// resume if a1 == 2
	// finish & exit
	sw zero, 0x104(zero)
	j exit

check_memory:
	// memory access if a1 == 3
	// read write
	lw a1, 0x108(zero)
	// extract write bit
	bne a1, zero, memory_write

	// arg0 @ 0x0-0x8, arg1 @ 0x8-0x10
memory_read:
	// arg0 = mem[arg1]
	// read aamsize
	lw a1, 0x10c(zero)
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
	lw a1, 0x10c(zero)
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
	lw a1, 0x110(zero)
	beq a1, zero, memory_done

	// arg1 += (1 << aamsize)
	// read aamsize
	lw a1, 0x10c(zero)
	li a0, 1
	sll a0, a0, a1

	// read arg1 & update
	ld a1, 0x8(zero)
	add a1, a1, a0
	sd a1, 0x8(zero)

memory_done:
	// complete
	ebreak

success:
	// signal command complete
	sw zero, 0x104(zero)
	j park_loop

exit:
	csrr a0, dscratch0
	csrr a1, dscratch1
	dret


