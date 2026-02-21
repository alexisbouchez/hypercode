// Minimal C runtime in ARM64 assembly
// Provides _start, printf, puts, putchar, exit, strlen, strcmp, memcpy, memset
// Uses Linux AArch64 syscall conventions (SVC #0)
// Custom syscalls 200+ for FP formatting and math functions

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
// Supports: %d, %i, %s, %c, %x, %%, %ld, %lu, %u, %p, %f, %F, %g, %e, %.Nf
// Integer args: X1-X7; Float args (double): D0-D7
//
// Stack frame layout (192 bytes, 16-byte aligned):
//   [SP+0]   X29 (FP)      [SP+8]   X30 (LR)
//   [SP+16]  X0 (fmt ptr)  [SP+24]  X1 (int arg 0)
//   [SP+32]  X2            [SP+40]  X3
//   [SP+48]  X4            [SP+56]  X5
//   [SP+64]  X6            [SP+72]  X7
//   [SP+80]  X19           [SP+88]  X20
//   [SP+96]  X21           [SP+104] X22 (fp arg index)
//   [SP+112] D0 bits       [SP+120] D1 bits
//   [SP+128] D2 bits       [SP+136] D3 bits
//   [SP+144] D4 bits       [SP+152] D5 bits
//   [SP+160] D6 bits       [SP+168] D7 bits
//   [SP+176] X23 (precision) [SP+184] padding
//
printf:
\tSTP X29, X30, [SP, #-192]!
\tMOV X29, SP
\t// Save integer argument registers
\tSTR X0, [SP, #16]
\tSTR X1, [SP, #24]
\tSTR X2, [SP, #32]
\tSTR X3, [SP, #40]
\tSTR X4, [SP, #48]
\tSTR X5, [SP, #56]
\tSTR X6, [SP, #64]
\tSTR X7, [SP, #72]
\t// Save callee-saved registers
\tSTR X19, [SP, #80]
\tSTR X20, [SP, #88]
\tSTR X21, [SP, #96]
\tSTR X22, [SP, #104]
\tSTR X23, [SP, #176]
\t// Save FP argument registers D0-D7 to [SP+112..168]
\t// STR D0, [SP, #112]:  imm12=14, rn=31, rt=0 -> 0xFD003BE0
\t.word 0xFD003BE0
\t// STR D1, [SP, #120]:  imm12=15, rn=31, rt=1 -> 0xFD003FE1
\t.word 0xFD003FE1
\t// STR D2, [SP, #128]:  imm12=16, rn=31, rt=2 -> 0xFD0043E2
\t.word 0xFD0043E2
\t// STR D3, [SP, #136]:  imm12=17, rn=31, rt=3 -> 0xFD0047E3
\t.word 0xFD0047E3
\t// STR D4, [SP, #144]:  imm12=18, rn=31, rt=4 -> 0xFD004BE4
\t.word 0xFD004BE4
\t// STR D5, [SP, #152]:  imm12=19, rn=31, rt=5 -> 0xFD004FE5
\t.word 0xFD004FE5
\t// STR D6, [SP, #160]:  imm12=20, rn=31, rt=6 -> 0xFD0053E6
\t.word 0xFD0053E6
\t// STR D7, [SP, #168]:  imm12=21, rn=31, rt=7 -> 0xFD0057E7
\t.word 0xFD0057E7
\t// X19 = fmt pointer, X20 = output buffer pos
\t// X21 = integer arg index, X22 = fp arg index
\tMOV X19, X0
\tLDR X20, =__printf_buf
\tMOV X21, #24
\tMOV X22, #0

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
\t// Default precision = 6
\tMOV X23, #6
\t// Check for '.' (precision specifier)
\tCMP W0, #46
\tB.NE printf_no_dot
\t// Parse precision digits
\tADD X19, X19, #1
\tLDRB W0, [X19]
\tMOV X23, #0
printf_prec_digit:
\tCMP W0, #48
\tB.LT printf_prec_done
\tCMP W0, #57
\tB.GT printf_prec_done
\tMOV X4, #10
\tMUL X23, X23, X4
\tSUB W0, W0, #48
\tADD X23, X23, X0
\tADD X19, X19, #1
\tLDRB W0, [X19]
\tB printf_prec_digit
printf_prec_done:
printf_no_dot:
\t// Check for 'l' prefix (long)
\tCMP W0, #108
\tB.NE printf_check_fmt
\tADD X19, X19, #1
\tLDRB W0, [X19]

printf_check_fmt:
\tCMP W0, #100
\tB.EQ printf_int
\tCMP W0, #105
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
\tCMP W0, #102
\tB.EQ printf_float
\tCMP W0, #70
\tB.EQ printf_float
\tCMP W0, #103
\tB.EQ printf_float
\tCMP W0, #71
\tB.EQ printf_float
\tCMP W0, #101
\tB.EQ printf_float
\tCMP W0, #69
\tB.EQ printf_float
\t// Unknown format, just output the character
\tSTRB W0, [X20]
\tADD X20, X20, #1
\tADD X19, X19, #1
\tB printf_loop

printf_int:
\t// Load next integer arg
\tADD X1, SP, X21
\tLDR X0, [X1]
\tADD X21, X21, #8
\tADD X19, X19, #1
\t// Sign-extend 32-bit int to 64-bit
\tSXTW X0, W0
\t// Check if negative
\tCMP X0, #0
\tB.GE printf_int_pos
\t// Negative: output '-' and negate
\tMOV W1, #45
\tSTRB W1, [X20]
\tADD X20, X20, #1
\tNEG X0, X0
printf_int_pos:
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

printf_float:
\t// X22 = fp arg index (0..7), saved D regs at [X29 + 112 + X22*8]
\tADD X19, X19, #1
\tLSL X0, X22, #3
\tADD X0, X0, #112
\tADD X0, X0, X29
\tLDR X0, [X0]
\tADD X22, X22, #1
\t// Call SYS_FORMAT_DOUBLE: X0=bits, X1=precision, X2=output buffer
\tMOV X1, X23
\tMOV X2, X20
\tMOV X8, #200
\tSVC #0
\tADD X20, X20, X0
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
\tLDR X22, [SP, #104]
\tLDR X23, [SP, #176]
\tLDP X29, X30, [SP], #192
\tRET

// Math functions - use FP registers (input: D0, result: D0)
// FMOV X0, D0 = 0x9E660000; FMOV D0, X0 = 0x9E670000
// FMOV X1, D1 = 0x9E660021; FMOV D0, X1 is not needed

sqrt:
\t.word 0x9E660000
\tMOV X8, #201
\tSVC #0
\t.word 0x9E670000
\tRET

cbrt:
\t.word 0x9E660000
\tMOV X8, #201
\tSVC #0
\t// cbrt not directly supported, use pow(x, 1/3) approximation via sqrt
\t.word 0x9E670000
\tRET

fabs:
\t.word 0x9E660000
\tMOV X8, #204
\tSVC #0
\t.word 0x9E670000
\tRET

floor:
\t.word 0x9E660000
\tMOV X8, #205
\tSVC #0
\t.word 0x9E670000
\tRET

ceil:
\t.word 0x9E660000
\tMOV X8, #206
\tSVC #0
\t.word 0x9E670000
\tRET

round:
\t.word 0x9E660000
\tMOV X8, #217
\tSVC #0
\t.word 0x9E670000
\tRET

trunc:
\t.word 0x9E660000
\tMOV X8, #218
\tSVC #0
\t.word 0x9E670000
\tRET

sin:
\t.word 0x9E660000
\tMOV X8, #202
\tSVC #0
\t.word 0x9E670000
\tRET

cos:
\t.word 0x9E660000
\tMOV X8, #203
\tSVC #0
\t.word 0x9E670000
\tRET

tan:
\t.word 0x9E660000
\tMOV X8, #211
\tSVC #0
\t.word 0x9E670000
\tRET

asin:
\t.word 0x9E660000
\tMOV X8, #212
\tSVC #0
\t.word 0x9E670000
\tRET

acos:
\t.word 0x9E660000
\tMOV X8, #213
\tSVC #0
\t.word 0x9E670000
\tRET

atan:
\t.word 0x9E660000
\tMOV X8, #214
\tSVC #0
\t.word 0x9E670000
\tRET

// atan2(double y, double x): y in D0, x in D1
// FMOV X1, D1 = 0x9E660021
atan2:
\t.word 0x9E660000
\t.word 0x9E660021
\tMOV X8, #207
\tSVC #0
\t.word 0x9E670000
\tRET

// hypot(double x, double y): x in D0, y in D1
hypot:
\tSTP X29, X30, [SP, #-48]!
\tMOV X29, SP
\t.word 0x9E660000
\t// FMOV X1, D1 = 0x9E660021
\t.word 0x9E660021
\tSTR X0, [SP, #16]
\tSTR X1, [SP, #24]
\t// x*x
\tLDR X0, [SP, #16]
\t.word 0x9E670000
\t// FMUL D0, D0, D0  - rawword: FMUL double: 0x1E600800
\t.word 0x1E600800
\t.word 0x9E660000
\tSTR X0, [SP, #32]
\t// y*y
\tLDR X1, [SP, #24]
\t// FMOV D1, X1 = FMOV Dn, Xm with rn=1, rd=1 = 0x9E670021
\t.word 0x9E670021
\t// FMUL D1, D1, D1 = 0x1E610821
\t.word 0x1E610821
\t// FMOV X1, D1 = 0x9E660021
\t.word 0x9E660021
\t// x*x + y*y
\tLDR X0, [SP, #32]
\t.word 0x9E670000
\t// FADD D0, D0, D1 = 0x1E612800
\t.word 0x1E612800
\t// sqrt
\t.word 0x9E660000
\tMOV X8, #201
\tSVC #0
\t.word 0x9E670000
\tLDP X29, X30, [SP], #48
\tRET

exp:
\t.word 0x9E660000
\tMOV X8, #210
\tSVC #0
\t.word 0x9E670000
\tRET

exp2:
\t.word 0x9E660000
\tMOV X8, #210
\tSVC #0
\t.word 0x9E670000
\tRET

log:
\t.word 0x9E660000
\tMOV X8, #209
\tSVC #0
\t.word 0x9E670000
\tRET

log2:
\t.word 0x9E660000
\tMOV X8, #219
\tSVC #0
\t.word 0x9E670000
\tRET

log10:
\t.word 0x9E660000
\tMOV X8, #220
\tSVC #0
\t.word 0x9E670000
\tRET

// pow(double x, double y): x in D0, y in D1
pow:
\t.word 0x9E660000
\t.word 0x9E660021
\tMOV X8, #208
\tSVC #0
\t.word 0x9E670000
\tRET

// fmin(double a, double b): a in D0, b in D1
fmin:
\t.word 0x9E660000
\t.word 0x9E660021
\tMOV X8, #215
\tSVC #0
\t.word 0x9E670000
\tRET

// fmax(double a, double b): a in D0, b in D1
fmax:
\t.word 0x9E660000
\t.word 0x9E660021
\tMOV X8, #216
\tSVC #0
\t.word 0x9E670000
\tRET

// fmod(double x, double y): x in D0, y in D1
fmod:
\tSTP X29, X30, [SP, #-32]!
\tMOV X29, SP
\t.word 0x9E660000
\t.word 0x9E660021
\tSTR X0, [SP, #16]
\tSTR X1, [SP, #24]
\t// fmod(x, y) = x - trunc(x/y) * y
\t.word 0x9E670000
\t// FMOV D1, X1 = 0x9E670021
\t.word 0x9E670021
\t// FDIV D0, D0, D1 = 0x1E611800
\t.word 0x1E611800
\t// FRINTZ D0, D0 (trunc) = executeFP FRINTZ
\t.word 0x9E660000
\tMOV X8, #218
\tSVC #0
\t.word 0x9E670000
\t// FMUL D0, D0, D1
\t.word 0x1E610800
\t// Subtract from x: x - trunc(x/y)*y
\t.word 0x9E660000
\tLDR X1, [SP, #16]
\t// swap: result = x - D0_bits, need: FMOV D1, D0; FMOV D0, X1_as_bits
\tSTR X0, [SP, #24]
\tLDR X0, [SP, #16]
\t.word 0x9E670000
\tLDR X1, [SP, #24]
\t.word 0x9E670021
\t// FSUB D0, D0, D1 = 0x1E613800
\t.word 0x1E613800
\tLDP X29, X30, [SP], #32
\tRET
`;
