SECTIONS {
    . = 0x100000;
    .text 0x100000 : {
	*(.text)
    }
}