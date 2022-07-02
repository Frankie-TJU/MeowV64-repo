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
3. CONFIG:
	1. [31]: opcode, 0=strided, 1=indexed
	2. [30:20]: bytes
	3. [19:0]: stride
4. ARGUMENTS:
	1. strided: baseHigh, baseLow
	2. indexed: baseHigh, baseLow, indexedBaseHigh, indexedBaseLow
5. END LOOP: all zeros