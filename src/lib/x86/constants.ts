// x86_64 register name -> index mapping
// 64-bit general purpose registers
export const REG_NAMES_64: Record<string, number> = {
  rax: 0, rcx: 1, rdx: 2, rbx: 3, rsp: 4, rbp: 5, rsi: 6, rdi: 7,
  r8: 8, r9: 9, r10: 10, r11: 11, r12: 12, r13: 13, r14: 14, r15: 15,
};

// 32-bit sub-registers
export const REG_NAMES_32: Record<string, number> = {
  eax: 0, ecx: 1, edx: 2, ebx: 3, esp: 4, ebp: 5, esi: 6, edi: 7,
  r8d: 8, r9d: 9, r10d: 10, r11d: 11, r12d: 12, r13d: 13, r14d: 14, r15d: 15,
};

// 16-bit sub-registers
export const REG_NAMES_16: Record<string, number> = {
  ax: 0, cx: 1, dx: 2, bx: 3, sp: 4, bp: 5, si: 6, di: 7,
  r8w: 8, r9w: 9, r10w: 10, r11w: 11, r12w: 12, r13w: 13, r14w: 14, r15w: 15,
};

// 8-bit sub-registers (low byte)
export const REG_NAMES_8: Record<string, number> = {
  al: 0, cl: 1, dl: 2, bl: 3, spl: 4, bpl: 5, sil: 6, dil: 7,
  r8b: 8, r9b: 9, r10b: 10, r11b: 11, r12b: 12, r13b: 13, r14b: 14, r15b: 15,
  ah: 16, ch: 17, dh: 18, bh: 19, // high byte registers (special indexing)
};

// Linux x86_64 syscall numbers
export const SYS_WRITE = 1;
export const SYS_EXIT = 60;

// Memory layout
export const CODE_BASE = 0x400000;
export const DATA_BASE = 0x500000;
export const STACK_TOP = 0x5FFF00; // Within the 2MB region from CODE_BASE
export const MEMORY_SIZE = 2 * 1024 * 1024; // 2MB

export const MAX_INSTRUCTIONS = 5000000;
