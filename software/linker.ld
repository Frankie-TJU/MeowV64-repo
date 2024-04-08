SECTIONS
{
    ROM_BASE = 0x81000000; /* ... but actually position independent */

    . = ROM_BASE;
    .text.start : { *(.text.start) }
    .text : { *(.text) }
}
