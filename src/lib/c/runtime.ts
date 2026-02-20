// Minimal C runtime in ARM64 assembly
// Provides _start, printf, puts, putchar, exit, strlen, strcmp, memcpy, memset
// Uses Linux AArch64 syscall conventions (SVC #0)

export const C_RUNTIME = `
.data
__printf_buf:
\t.skip 1024
__printf_numbuf:
\t.skip 32
__heap_ptr:
\t.quad 0x550000

.text
.global _start

_start:
\tBL main
\tMOV X8, #93
\tSVC #0

// exit(int status)
// X0 = exit code
exit:
\tMOV X8, #93
\tSVC #0

// putchar(int c)
// X0 = character
// Returns: character written
putchar:
\tSTP X29, X30, [SP, #-32]!
\tMOV X29, SP
\tSTRB W0, [SP, #16]
\tMOV X2, #1
\tADD X1, SP, #16
\tMOV X0, #1
\tMOV X8, #64
\tSVC #0
\tLDRB W0, [SP, #16]
\tLDP X29, X30, [SP], #32
\tRET

// puts(const char *s)
// X0 = string pointer
// Returns: non-negative on success
puts:
\tSTP X29, X30, [SP, #-32]!
\tMOV X29, SP
\tSTR X0, [SP, #16]
\tBL strlen
\tMOV X2, X0
\tLDR X1, [SP, #16]
\tMOV X0, #1
\tMOV X8, #64
\tSVC #0
\t// Print newline
\tMOV X0, #10
\tSTRB W0, [SP, #16]
\tMOV X2, #1
\tADD X1, SP, #16
\tMOV X0, #1
\tMOV X8, #64
\tSVC #0
\tMOV X0, #0
\tLDP X29, X30, [SP], #32
\tRET

// strlen(const char *s)
// X0 = string pointer
// Returns: length in X0
strlen:
\tMOV X1, X0
strlen_loop:
\tLDRB W2, [X1]
\tCBZ W2, strlen_done
\tADD X1, X1, #1
\tB strlen_loop
strlen_done:
\tSUB X0, X1, X0
\tRET

// strcmp(const char *s1, const char *s2)
// X0 = s1, X1 = s2
// Returns: 0 if equal, <0 if s1<s2, >0 if s1>s2
strcmp:
strcmp_loop:
\tLDRB W2, [X0]
\tLDRB W3, [X1]
\tCMP W2, W3
\tB.NE strcmp_diff
\tCBZ W2, strcmp_equal
\tADD X0, X0, #1
\tADD X1, X1, #1
\tB strcmp_loop
strcmp_equal:
\tMOV X0, #0
\tRET
strcmp_diff:
\tSUB X0, X2, X3
\tRET

// memcpy(void *dest, const void *src, size_t n)
// X0 = dest, X1 = src, X2 = n
// Returns: dest in X0
memcpy:
\tMOV X3, X0
\tCBZ X2, memcpy_done
memcpy_loop:
\tLDRB W4, [X1]
\tSTRB W4, [X3]
\tADD X1, X1, #1
\tADD X3, X3, #1
\tSUB X2, X2, #1
\tCBNZ X2, memcpy_loop
memcpy_done:
\tRET

// memset(void *s, int c, size_t n)
// X0 = s, X1 = c (byte), X2 = n
// Returns: s in X0
memset:
\tMOV X3, X0
\tCBZ X2, memset_done
memset_loop:
\tSTRB W1, [X3]
\tADD X3, X3, #1
\tSUB X2, X2, #1
\tCBNZ X2, memset_loop
memset_done:
\tRET

// malloc(size_t size)
// X0 = size
// Returns: pointer in X0
malloc:
\tLDR X1, =__heap_ptr
\tLDR X2, [X1]
\tADD X0, X0, #7
\tLSR X0, X0, #3
\tLSL X0, X0, #3
\tADD X3, X2, X0
\tSTR X3, [X1]
\tMOV X0, X2
\tRET

// free(void *ptr) - no-op bump allocator
free:
\tRET

// calloc(size_t nmemb, size_t size)
// X0 = nmemb, X1 = size
// Returns: pointer in X0 (zeroed)
calloc:
\tSTP X29, X30, [SP, #-32]!
\tMOV X29, SP
\tSTR X19, [SP, #16]
\tMUL X0, X0, X1
\tMOV X19, X0
\tBL malloc
\tMOV X2, X19
\tMOV X1, #0
\tBL memset
\tLDR X19, [SP, #16]
\tLDP X29, X30, [SP], #32
\tRET

// realloc(void *ptr, size_t new_size)
// X0 = ptr, X1 = new_size
// Returns: pointer in X0
realloc:
\tSTP X29, X30, [SP, #-48]!
\tMOV X29, SP
\tSTR X19, [SP, #16]
\tSTR X20, [SP, #24]
\tSTR X21, [SP, #32]
\tMOV X19, X0
\tMOV X20, X1
\tMOV X0, X1
\tBL malloc
\tMOV X21, X0
\tCBZ X19, realloc_done
\tMOV X0, X21
\tMOV X1, X19
\tMOV X2, X20
\tBL memcpy
realloc_done:
\tMOV X0, X21
\tLDR X19, [SP, #16]
\tLDR X20, [SP, #24]
\tLDR X21, [SP, #32]
\tLDP X29, X30, [SP], #48
\tRET

// printf(const char *fmt, ...)
// Simplified printf supporting: %d, %s, %c, %x, %%, %ld, %u, %lu
// X0 = format string, X1-X7 = arguments
// Returns: number of characters written
printf:
\tSTP X29, X30, [SP, #-112]!
\tMOV X29, SP
\t// Save all argument registers
\tSTR X0, [SP, #16]
\tSTR X1, [SP, #24]
\tSTR X2, [SP, #32]
\tSTR X3, [SP, #40]
\tSTR X4, [SP, #48]
\tSTR X5, [SP, #56]
\tSTR X6, [SP, #64]
\tSTR X7, [SP, #72]
\t// X19 = fmt pointer, X20 = output buffer pos
\t// X21 = arg index (next arg register offset)
\tSTR X19, [SP, #80]
\tSTR X20, [SP, #88]
\tSTR X21, [SP, #96]
\tMOV X19, X0
\tLDR X20, =__printf_buf
\tMOV X21, #24

printf_loop:
\tLDRB W0, [X19]
\tCBZ W0, printf_flush
\tCMP W0, #37
\tB.EQ printf_format
\t// Regular character
\tSTRB W0, [X20]
\tADD X20, X20, #1
\tADD X19, X19, #1
\tB printf_loop

printf_format:
\tADD X19, X19, #1
\tLDRB W0, [X19]
\t// Check for 'l' prefix (long)
\tCMP W0, #108
\tB.NE printf_check_fmt
\tADD X19, X19, #1
\tLDRB W0, [X19]

printf_check_fmt:
\tCMP W0, #100
\tB.EQ printf_int
\tCMP W0, #117
\tB.EQ printf_uint
\tCMP W0, #115
\tB.EQ printf_str
\tCMP W0, #99
\tB.EQ printf_char
\tCMP W0, #120
\tB.EQ printf_hex
\tCMP W0, #37
\tB.EQ printf_percent
\tCMP W0, #112
\tB.EQ printf_hex
\t// Unknown format, just output the character
\tSTRB W0, [X20]
\tADD X20, X20, #1
\tADD X19, X19, #1
\tB printf_loop

printf_int:
\t// Load next arg
\tADD X1, SP, X21
\tLDR X0, [X1]
\tADD X21, X21, #8
\tADD X19, X19, #1
\t// Sign-extend 32-bit int to 64-bit (TCC zero-extends W regs on stack)
\tSXTW X0, W0
\t// Convert signed int to decimal string
\t// Check if negative
\tCMP X0, #0
\tB.GE printf_int_pos
\t// Negative: output '-' and negate
\tMOV W1, #45
\tSTRB W1, [X20]
\tADD X20, X20, #1
\tNEG X0, X0
printf_int_pos:
\t// X0 = absolute value, convert to decimal
\tLDR X1, =__printf_numbuf
\tADD X1, X1, #20
\tMOV X3, #0
printf_int_digitloop:
\tMOV X4, #10
\tUDIV X2, X0, X4
\tMUL X5, X2, X4
\tSUB X5, X0, X5
\tADD W5, W5, #48
\tSUB X1, X1, #1
\tSTRB W5, [X1]
\tADD X3, X3, #1
\tMOV X0, X2
\tCBNZ X0, printf_int_digitloop
\t// Copy digits to output buffer
printf_int_copy:
\tLDRB W0, [X1]
\tSTRB W0, [X20]
\tADD X1, X1, #1
\tADD X20, X20, #1
\tSUB X3, X3, #1
\tCBNZ X3, printf_int_copy
\tB printf_loop

printf_uint:
\t// Load next arg (unsigned)
\tADD X1, SP, X21
\tLDR X0, [X1]
\tADD X21, X21, #8
\tADD X19, X19, #1
\t// Convert unsigned to decimal string
\tLDR X1, =__printf_numbuf
\tADD X1, X1, #20
\tMOV X3, #0
printf_uint_digitloop:
\tMOV X4, #10
\tUDIV X2, X0, X4
\tMUL X5, X2, X4
\tSUB X5, X0, X5
\tADD W5, W5, #48
\tSUB X1, X1, #1
\tSTRB W5, [X1]
\tADD X3, X3, #1
\tMOV X0, X2
\tCBNZ X0, printf_uint_digitloop
\t// Copy digits to output buffer
printf_uint_copy:
\tLDRB W0, [X1]
\tSTRB W0, [X20]
\tADD X1, X1, #1
\tADD X20, X20, #1
\tSUB X3, X3, #1
\tCBNZ X3, printf_uint_copy
\tB printf_loop

printf_str:
\t// Load string pointer from next arg
\tADD X1, SP, X21
\tLDR X1, [X1]
\tADD X21, X21, #8
\tADD X19, X19, #1
printf_str_loop:
\tLDRB W0, [X1]
\tCBZ W0, printf_loop
\tSTRB W0, [X20]
\tADD X1, X1, #1
\tADD X20, X20, #1
\tB printf_str_loop

printf_char:
\t// Load char from next arg
\tADD X1, SP, X21
\tLDR X0, [X1]
\tADD X21, X21, #8
\tADD X19, X19, #1
\tSTRB W0, [X20]
\tADD X20, X20, #1
\tB printf_loop

printf_hex:
\t// Load value from next arg
\tADD X1, SP, X21
\tLDR X0, [X1]
\tADD X21, X21, #8
\tADD X19, X19, #1
\t// Convert to hex string
\tLDR X1, =__printf_numbuf
\tADD X1, X1, #20
\tMOV X3, #0
printf_hex_digitloop:
\tAND X5, X0, #0xf
\tCMP X5, #10
\tB.GE printf_hex_letter
\tADD W5, W5, #48
\tB printf_hex_store
printf_hex_letter:
\tADD W5, W5, #87
printf_hex_store:
\tSUB X1, X1, #1
\tSTRB W5, [X1]
\tADD X3, X3, #1
\tLSR X0, X0, #4
\tCBNZ X0, printf_hex_digitloop
\t// If no digits, output at least "0"
\tCBNZ X3, printf_hex_copy
\tMOV W5, #48
\tSUB X1, X1, #1
\tSTRB W5, [X1]
\tMOV X3, #1
printf_hex_copy:
\tLDRB W0, [X1]
\tSTRB W0, [X20]
\tADD X1, X1, #1
\tADD X20, X20, #1
\tSUB X3, X3, #1
\tCBNZ X3, printf_hex_copy
\tB printf_loop

printf_percent:
\tMOV W0, #37
\tSTRB W0, [X20]
\tADD X20, X20, #1
\tADD X19, X19, #1
\tB printf_loop

printf_flush:
\t// Write all buffered output
\tLDR X1, =__printf_buf
\tSUB X2, X20, X1
\tCBZ X2, printf_done
\tMOV X0, #1
\tMOV X8, #64
\tSVC #0
printf_done:
\tLDR X1, =__printf_buf
\tSUB X0, X20, X1
\tLDR X19, [SP, #80]
\tLDR X20, [SP, #88]
\tLDR X21, [SP, #96]
\tLDP X29, X30, [SP], #112
\tRET
`;
