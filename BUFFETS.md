# Buffets

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

# Address Generation

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

1. strided: read `bytes` bytes from `base + regs[rs1] * stride` as data
	1. \[31:27\]: opcode, 0b00000
	2. \[26:25\]: rs1
	3. \[19:13\]: bytes
	4. \[9:0\]: stride
	5. two 32-bit arguments: baseHigh, baseLow
2. indexed: read `bytes` bytes from `base + regs[rs1] * stride` as index, read `bytes` bytes from `indexedBase + (index << indexedShift)` as data
	1. \[31:27\]: opcode, 0b00001
	2. \[26:25\]: rs1
	3. \[19:13\]: bytes
	4. \[12:10\]: indexedShift
	5. \[9:0\]: stride
	6. four 32-bit arguments: baseHigh, baseLow, indexedBaseHigh, indexedBaseLow
3. loop: `regs[rs1]++`, if `regs[rs1] == iterations`, then stop; otherwise, goto address `addr`
	1. \[31:27\]: opcode, 0b00010
	3. \[26:25\]: rs1
	3. \[24:0\]: addr
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
6. addi: compute sum of `immediate` and `regs[rs1]` and write to `regs[rd]`
	1. \[31:27\]: opcode, 0b00101
	2. \[26:25\]: rs1
	3. \[24:23\]: rd
	4. \[22:0\]: imm
