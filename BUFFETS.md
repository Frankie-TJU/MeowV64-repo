# Buffets

## Buffets

Register Address Map:

1. 0x00: HEAD
2. 0x20: TAIL
3. 0x40: SIZE
4. 0x60: EMPTY
5. 0x80: SHRINK

How to use:

1. initialize address generation
2. read from Buffets
3. shrink data
4. loop

## Address Generation

Address Map:

- 0x00: STATUS
- 0x20: CONTROL
- 0x40: ITERATIONS
- 0x60: INSTS

How to use:

1. setup INSTS
2. set ITERATIONS
3. write to CONTROL
4. wait for STATUS to complete

Instructions:

1. each instruction is 32 bit wide
2. CONFIG + ARGUMENTS
3. CONFIG: \[31:27\] is opcode
4. ARGUMENTS:
	1. strided: baseHigh, baseLow
	2. indexed: baseHigh, baseLow, indexedBaseHigh, indexedBaseLow
5. END LOOP: all zeros

4 32-bit registers from 0 to 3: i, j, k, zero

fields:
1. \[31:27\]: opcode
2. \[26:25\]: rs1
3. \[24:23\]: rd
4. \[19:13\]: bytes
5. \[22:21\]: rs2
6. \[12:10\]: indexedShift
7. \[9:0\]: stride
8. \[24:0\]: addr
9. \[22:0\]: imm

instructions:

1. loop: `regs[rs1]++`, if `regs[rs1] == iterations`, then stop and set `regs[rs1]` as `0`; otherwise, goto address `addr`
	1. \[31:27\]: opcode, 0b00000
	3. \[26:25\]: rs1
	3. \[24:0\]: addr
2. strided: read `bytes` bytes from `base + regs[rs1] * stride` as data, send to buffets
	1. \[31:27\]: opcode, 0b00001
	2. \[26:25\]: rs1
	3. \[19:13\]: bytes
	4. \[9:0\]: stride
	5. two 32-bit arguments: baseHigh, baseLow
3. indexed: read `bytes` bytes from `base + regs[rs1] * stride` as index, read `bytes` bytes from `indexedBase + (index << indexedShift)` as data, send to buffets
	1. \[31:27\]: opcode, 0b00010
	2. \[26:25\]: rs1
	3. \[19:13\]: bytes
	4. \[12:10\]: indexedShift
	5. \[9:0\]: stride
	6. four 32-bit arguments: baseHigh, baseLow, indexedBaseHigh, indexedBaseLow
4. load: read `bytes` bytes from `base + regs[rs1] * stride` into `regs[rd]`
	1. \[31:27\]: opcode, 0b00011
	2. \[26:25\]: rs1
	3. \[24:23\]: rd
	4. \[19:13\]: bytes
	5. \[9:0\]: stride
	6. two 32-bit arguments: baseHigh, baseLow
5. add: compute sum of `regs[rs1]` and `regs[rs2]` and write to `regs[rd]`
	1. \[31:27\]: opcode, 0b00100
	2. \[26:25\]: rs1
	3. \[24:23\]: rd
	4. \[22:21\]: rs2
6. addi: compute sum of signed `immediate` and `regs[rs1]` and write to `regs[rd]`
	1. \[31:27\]: opcode, 0b00101
	2. \[26:25\]: rs1
	3. \[24:23\]: rd
	4. \[22:0\]: simm

## L2 Buffets

Connects to all L1 Buffets via TileLink

Maintain head/tail pointers for each L1 Buffets, automatic fast path without explicit pop

Only free space when all L1 Buffets bumped through the pointer

L1 Buffets send a long unanswered TileLink request until data is ready

Allow user to push data to L2 Buffets directly, or through Address Generation

Allow large portion of data to be pushed to L2 Buffets atomically: 2 stage commit & fastpath

By default, it is a broadcast interface: each L1 Buffets needs to read & acknowledge the data before L2 Buffets release the data

Add additional capability to mask data out for specific L1 Buffets to allow multicast & unicast operation

### Usage

1. TLB Shootdown: One core send data to L2 Buffets with broadcast
2. Distributed Data Structure: Send data to specified remote core via masking
