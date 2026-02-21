// ARM64 register indices
export const REG_NAMES_64: Record<string, number> = {
  x0: 0, x1: 1, x2: 2, x3: 3, x4: 4, x5: 5, x6: 6, x7: 7,
  x8: 8, x9: 9, x10: 10, x11: 11, x12: 12, x13: 13, x14: 14, x15: 15,
  x16: 16, x17: 17, x18: 18, x19: 19, x20: 20, x21: 21, x22: 22, x23: 23,
  x24: 24, x25: 25, x26: 26, x27: 27, x28: 28, x29: 29, x30: 30,
  fp: 29, lr: 30, xzr: 31, sp: 31,
};

export const REG_NAMES_32: Record<string, number> = {
  w0: 0, w1: 1, w2: 2, w3: 3, w4: 4, w5: 5, w6: 6, w7: 7,
  w8: 8, w9: 9, w10: 10, w11: 11, w12: 12, w13: 13, w14: 14, w15: 15,
  w16: 16, w17: 17, w18: 18, w19: 19, w20: 20, w21: 21, w22: 22, w23: 23,
  w24: 24, w25: 25, w26: 26, w27: 27, w28: 28, w29: 29, w30: 30,
  wzr: 31,
};

// Linux AArch64 syscall numbers
export const SYS_WRITE = 64;
export const SYS_EXIT = 93;

// Custom syscalls for FP/math operations
export const SYS_FORMAT_DOUBLE = 200;
export const SYS_SQRT = 201;
export const SYS_SIN = 202;
export const SYS_COS = 203;
export const SYS_FABS = 204;
export const SYS_FLOOR = 205;
export const SYS_CEIL = 206;
export const SYS_ATAN2 = 207;
export const SYS_POW = 208;
export const SYS_LOG = 209;
export const SYS_EXP = 210;
export const SYS_TAN = 211;
export const SYS_ASIN = 212;
export const SYS_ACOS = 213;
export const SYS_ATAN = 214;
export const SYS_FMIN = 215;
export const SYS_FMAX = 216;
export const SYS_ROUND = 217;
export const SYS_TRUNC = 218;
export const SYS_LOG2 = 219;
export const SYS_LOG10 = 220;

// Memory layout
export const CODE_BASE = 0x400000;
export const DATA_BASE = 0x500000;
export const STACK_TOP = 0x5FFF00; // Within the 2MB region from CODE_BASE
export const MEMORY_SIZE = 2 * 1024 * 1024; // 2MB

// Condition codes for B.cond
export const COND_CODES: Record<string, number> = {
  eq: 0, ne: 1, cs: 2, hs: 2, cc: 3, lo: 3,
  mi: 4, pl: 5, vs: 6, vc: 7,
  hi: 8, ls: 9, ge: 10, lt: 11, gt: 12, le: 13,
  al: 14,
};

export const MAX_INSTRUCTIONS = 5000000;
