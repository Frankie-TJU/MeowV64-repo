SECTIONS {
    /* move tohost/fromhost to uncached memory region */
    . = 0x60000000;
    .tohost : { *(.tohost) }
    
    . = 0x80000000;
    .text 0x80000000 : {
        *(.text.init)
        *(.text.*)
    }
    .rodata : {
        __srodata_begin = .;
        *(.srodata)
        *(.srodata.*)
        __rodata_begin = .;
        *(.rodata)
        *(.rodata.*)
    }
    .data : {
        *(.sdata)
        *(.data)
    }
    .bss : {
        *(.sbss)
        *(.bss)
    }

    /DISCARD/ : {
        *(.comment)
        *(.eh_frame)
        *(.riscv.attributes)
    }
}
