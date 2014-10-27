MEMORY
{
  flash (rx) : ORIGIN = 0x08004000, LENGTH = 1008K
  sram (rwx) : ORIGIN = 0x20000000, LENGTH = 128K
  ccsram (rwx) : ORIGIN = 0x10000000, LENGTH = 64K
}
_estack = 0x20020000;
_min_heap_size = 0;
_min_stack_size = 0x400;
OUTPUT_ARCH(arm)
ENTRY(Reset_Handler)
SECTIONS
{
  .isr_vector : {
    . = ALIGN(4);
    KEEP(*(.isr_vector))
    . = ALIGN(4);
  } > flash
  .text : {
    *(.text .text.*)
    *(.fixup)
    *(.gnu.warning)
    *(.rodata .rodata.*)
    *(.glue_7)
    *(.glue_7t)
    *(.eh_frame)
    *(.gnu.linkonce.t.*)
    *(.got)
    *(.gcc_except_table)
    *(.gnu.linkonce.r.*)
    KEEP(*(.init))
    KEEP(*(.fini))
    . = ALIGN(4);
    _etext = ABSOLUTE(.);
  } > flash
  .ARM.extab : {
    *(.ARM.extab*)
  } > flash
  __exidx_start = ABSOLUTE(.);
  .ARM.exidx : {
    *(.ARM.exidx*)
  } > flash
  __exidx_end = ABSOLUTE(.);
  .preinit_array : {
    PROVIDE_HIDDEN(__preinit_array_start = .);
    KEEP(*(.preinit_array))
    PROVIDE_HIDDEN(__preinit_array_end = .);
  } > flash
  .init_array : {
    PROVIDE_HIDDEN(__init_array_start = .);
    KEEP(*(.init_array))
    PROVIDE_HIDDEN(__init_array_end = .);
  } > flash
  .ctors : {
    KEEP(*crtbegin.o(.ctors))
    KEEP(*crtbegin?.o(.ctors))
    KEEP(*(EXCLUDE_FILE(*crtend.o *crtend?.o) .ctors))
    KEEP(*(SORT(.ctors.*)))
    KEEP(*(.ctors))
  } > flash
  .data : {
    _sidata = LOADADDR(.data);
    _sdata = ABSOLUTE(.);
    *(.data .data.*)
    *(.gnu.linkonce.d.*)
    CONSTRUCTORS
    _edata = ABSOLUTE(.);
  } > ccsram AT > flash
  . = ALIGN(4);
  .bss : {
    _sbss = ABSOLUTE(.);
    __bss_start__ = ABSOLUTE(_sbss);
    *(.bss .bss.*)
    *(.gnu.linkonce.b.*)
    *(COMMON)
    . = ALIGN(4);
    _ebss = ABSOLUTE(.);
    __bss_end__ = ABSOLUTE(_ebss);
  } > sram
  ._user_heap_stack : {
    . = ALIGN(4);
    PROVIDE(end = .);
    PROVIDE(_end = .);
    . = . + _min_heap_size;
    . = . + _min_stack_size;
    . = ALIGN(4);
  } > ccsram
  .stab 0 : { *(.stab) }
  .stabstr 0 : { *(.stabstr) }
  .stab.excl 0 : { *(.stab.excl) }
  .stab.exclstr 0 : { *(.stab.exclstr) }
  .stab.index 0 : { *(.stab.index) }
  .stab.indexstr 0 : { *(.stab.indexstr) }
  .comment 0 : { *(.comment) }
  .debug_abbrev 0 : { *(.debug_abbrev) }
  .debug_info 0 : { *(.debug_info) }
  .debug_line 0 : { *(.debug_line) }
  .debug_pubnames 0 : { *(.debug_pubnames) }
  .debug_aranges 0 : { *(.debug_aranges) }
  /DISCARD/ : {
    libc.a (*)
    libm.a (*)
    libgcc.a (*)
  }
}
