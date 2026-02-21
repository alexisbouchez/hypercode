import type { AssembledProgram, Operand } from "./assembler";
import { MAX_INSTRUCTIONS } from "./constants";
import { RegisterFile } from "./registers";
import { Memory } from "./memory";
import { handleSyscall } from "./syscalls";

export interface ExecutionResult {
  stdout: string;
  error: string;
  exitCode: number;
}

function getReg(op: Operand): number {
  if (op.kind === "reg64" || op.kind === "reg32") return op.reg;
  throw new Error(`Expected register operand, got ${op.kind}`);
}

function isSP(op: Operand): boolean {
  return op.kind === "reg64" && op.sp === true;
}

function readReg(op: Operand, regs: RegisterFile): bigint {
  const reg = getReg(op);
  if (isSP(op)) return regs.getSP();
  if (op.kind === "reg32") return BigInt(regs.getW(reg));
  return regs.getX(reg);
}

function writeReg(op: Operand, regs: RegisterFile, value: bigint) {
  const reg = getReg(op);
  if (isSP(op)) { regs.setSP(value); return; }
  if (op.kind === "reg32") { regs.setW(reg, Number(BigInt.asUintN(32, value))); return; }
  regs.setX(reg, value);
}

function getImm(op: Operand): bigint {
  if (op.kind === "imm") return op.value;
  throw new Error(`Expected immediate operand, got ${op.kind}`);
}

function getMem(op: Operand): { base: number; offset: number; preIndex: boolean; postIndex: boolean; useSP: boolean } {
  if (op.kind === "mem") return op;
  throw new Error(`Expected memory operand, got ${op.kind}`);
}

function isMemReg(op: Operand): op is { kind: "mem_reg"; base: number; offsetReg: number; useSP: boolean } {
  return op.kind === "mem_reg";
}

function resolveAddr(op: Operand, regs: RegisterFile): { addr: number; preWrite: (() => void) | null; postWrite: (() => void) | null } {
  if (op.kind === "mem_reg") {
    const baseVal = Number(BigInt.asUintN(64, op.useSP ? regs.getSP() : regs.getXOrSP(op.base)));
    const offVal = Number(BigInt.asIntN(64, regs.getX(op.offsetReg)));
    return { addr: baseVal + offVal, preWrite: null, postWrite: null };
  }
  const m = getMem(op);
  let baseAddr = Number(BigInt.asUintN(64, m.useSP ? regs.getSP() : regs.getXOrSP(m.base)));
  if (m.preIndex) {
    baseAddr += m.offset;
    const writeBack = () => {
      if (m.useSP) regs.setSP(BigInt(baseAddr));
      else regs.setXOrSP(m.base, BigInt(baseAddr));
    };
    return { addr: baseAddr, preWrite: writeBack, postWrite: null };
  }
  const accessAddr = baseAddr + (m.postIndex ? 0 : m.offset);
  if (m.postIndex) {
    const newBase = baseAddr + m.offset;
    const writeBack = () => {
      if (m.useSP) regs.setSP(BigInt(newBase));
      else regs.setXOrSP(m.base, BigInt(newBase));
    };
    return { addr: accessAddr, preWrite: null, postWrite: writeBack };
  }
  return { addr: accessAddr, preWrite: null, postWrite: null };
}

function executeFP(opcode: number, fpRegs: Float64Array, regs: RegisterFile, mem: Memory): void {
  const flags = regs; // RegisterFile has .n .z .c .v directly
  // FP data-processing 2-register: 0 0 0 11110 01 1 Rm opcode 10 Rn Rd
  // Mask: 0xFF200C00, Value: 0x1E200800 (double, type=01)
  if ((opcode & 0xFF200C00) === 0x1E200800) {
    const rd = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    const rm = (opcode >> 16) & 0x1F;
    const op = (opcode >> 12) & 0xF;
    switch (op) {
      case 0x0: fpRegs[rd] = fpRegs[rn] * fpRegs[rm]; break; // FMUL
      case 0x1: fpRegs[rd] = fpRegs[rn] / fpRegs[rm]; break; // FDIV
      case 0x2: fpRegs[rd] = fpRegs[rn] + fpRegs[rm]; break; // FADD
      case 0x3: fpRegs[rd] = fpRegs[rn] - fpRegs[rm]; break; // FSUB
      case 0x4: fpRegs[rd] = Math.max(fpRegs[rn], fpRegs[rm]); break; // FMAX
      case 0x5: fpRegs[rd] = Math.min(fpRegs[rn], fpRegs[rm]); break; // FMIN
    }
    return;
  }

  // FP data-processing 1-register: 0 0 0 11110 type 1 opcode 10000 Rn Rd
  if ((opcode & 0xFF207C00) === 0x1E204000) {
    const rd = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    const op = (opcode >> 15) & 0x3F;
    switch (op) {
      case 0x0: fpRegs[rd] = fpRegs[rn]; break;            // FMOV Dd, Dn (register copy)
      case 0x1: fpRegs[rd] = Math.abs(fpRegs[rn]); break;  // FABS
      case 0x2: fpRegs[rd] = -fpRegs[rn]; break;           // FNEG
      case 0x3: fpRegs[rd] = Math.sqrt(fpRegs[rn]); break; // FSQRT
      case 0x8: fpRegs[rd] = Math.round(fpRegs[rn]); break; // FRINTN
      case 0x9: fpRegs[rd] = Math.ceil(fpRegs[rn]); break;  // FRINTP
      case 0xA: fpRegs[rd] = Math.floor(fpRegs[rn]); break; // FRINTM
      case 0xB: fpRegs[rd] = Math.trunc(fpRegs[rn]); break; // FRINTZ
      case 0xC: fpRegs[rd] = Math.round(fpRegs[rn]); break; // FRINTA
    }
    return;
  }

  // FCMP / FCMPE: set flags from FP comparison
  // Encoding: 0 0 0 11110 01 1 Rm 00 1000 Rn 0 0000 (FCMP Dn, Dm)
  if ((opcode & 0xFF20FC07) === 0x1E202000) {
    const rn = (opcode >> 5) & 0x1F;
    const rm = (opcode >> 16) & 0x1F;
    const a = fpRegs[rn];
    const b = fpRegs[rm];
    flags.n = a < b;
    flags.z = a === b;
    flags.c = a >= b;
    flags.v = isNaN(a) || isNaN(b);
    return;
  }
  // FCMP Dn, #0.0 (opcode bit4=0, opc=0)
  if ((opcode & 0xFF20FC1F) === 0x1E202008) {
    const rn = (opcode >> 5) & 0x1F;
    const a = fpRegs[rn];
    flags.n = a < 0;
    flags.z = a === 0;
    flags.c = a >= 0;
    flags.v = isNaN(a);
    return;
  }

  // FCVTZS Xd, Dn (FP -> signed 64-bit int): sf=1 S=0 type=01 rmode=11 opcode=000
  if (((opcode & 0xFFFFFC00) >>> 0) === 0x9E780000) {
    const rd = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    const val = Math.trunc(fpRegs[rn]);
    regs.setX(rd, isNaN(val) ? 0n : BigInt(val));
    return;
  }
  // FCVTZS Wd, Dn (32-bit): sf=0
  if ((opcode & 0xFFFFFC00) === 0x1E780000) {
    const rd = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    regs.setW(rd, Math.trunc(fpRegs[rn]) | 0);
    return;
  }

  // SCVTF Dd, Xn (signed 64-bit int -> FP): sf=1 S=0 type=01 rmode=00 opcode=010
  if (((opcode & 0xFFFFFC00) >>> 0) === 0x9E620000) {
    const rd = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    fpRegs[rd] = Number(BigInt.asIntN(64, regs.getX(rn)));
    return;
  }
  // SCVTF Dd, Wn (signed 32-bit int -> FP): sf=0
  if ((opcode & 0xFFFFFC00) === 0x1E620000) {
    const rd = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    fpRegs[rd] = regs.getW(rn) | 0; // sign extend 32-bit
    return;
  }
  // UCVTF Dd, Xn (unsigned 64-bit int -> FP)
  if (((opcode & 0xFFFFFC00) >>> 0) === 0x9E630000) {
    const rd = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    fpRegs[rd] = Number(BigInt.asUintN(64, regs.getX(rn)));
    return;
  }

  // FMOV Xd, Dn (64-bit GP <- FP): sf=1 type=01 rmode=11 opcode=110
  if (((opcode & 0xFFFFFC00) >>> 0) === 0x9E660000) {
    const rd = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    const buf = new ArrayBuffer(8);
    new DataView(buf).setFloat64(0, fpRegs[rn], true);
    regs.setX(rd, new DataView(buf).getBigUint64(0, true));
    return;
  }
  // FMOV Dn, Xm (64-bit FP <- GP): sf=1 type=01 rmode=11 opcode=111
  if (((opcode & 0xFFFFFC00) >>> 0) === 0x9E670000) {
    const rd = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    const buf = new ArrayBuffer(8);
    new DataView(buf).setBigUint64(0, BigInt.asUintN(64, regs.getX(rn)), true);
    fpRegs[rd] = new DataView(buf).getFloat64(0, true);
    return;
  }
  // FMOV Dd, #imm8 (immediate): 0 0 0 11110 01 1 imm8 1 0 0 0 0 0 0 0 Rd
  if ((opcode & 0xFFE0FC00) === 0x1E201000) {
    const rd = opcode & 0x1F;
    const imm8 = (opcode >> 13) & 0xFF;
    // Decode VFPExpandImm
    const sign = (imm8 >> 7) & 1;
    const exp = (((imm8 >> 6) & 1) ? 0x3FC : 0x400) | ((imm8 >> 4) & 3);
    const mant = (imm8 & 0xF) << 48;
    const bits64 = (BigInt(sign) << 63n) | (BigInt(exp) << 52n) | BigInt(mant);
    const buf = new ArrayBuffer(8);
    new DataView(buf).setBigUint64(0, bits64, true);
    fpRegs[rd] = new DataView(buf).getFloat64(0, true);
    return;
  }

  // LDR Dn, [Xn, #imm12] (FP load, 64-bit): size=11 V=1 opc=01 imm12 Rn Rt
  // 1 1 1 1 1 1 0 1 0 1 imm12 Rn Rt  (0xFD400000 family)
  if (((opcode & 0xFFC00000) >>> 0) === 0xFD400000) {
    const rt = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    const imm12 = (opcode >> 10) & 0xFFF;
    const offset = imm12 * 8;
    const base = rn === 31 ? Number(regs.getSP()) : Number(regs.getX(rn));
    const addr = base + offset;
    const buf = mem.readBytes(addr, 8);
    fpRegs[rt] = new DataView(buf.buffer, buf.byteOffset, 8).getFloat64(0, true);
    return;
  }
  // STR Dn, [Xn, #imm12] (FP store, 64-bit): 0xFD000000 family
  if (((opcode & 0xFFC00000) >>> 0) === 0xFD000000) {
    const rt = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    const imm12 = (opcode >> 10) & 0xFFF;
    const offset = imm12 * 8;
    const base = rn === 31 ? Number(regs.getSP()) : Number(regs.getX(rn));
    const addr = base + offset;
    const buf = new ArrayBuffer(8);
    new DataView(buf).setFloat64(0, fpRegs[rt], true);
    mem.writeBytes(addr, new Uint8Array(buf));
    return;
  }

  // STR Sn, [Xn, #imm12] (FP store, 32-bit): size=10 V=1 opc=00 (0xBD000000)
  if (((opcode & 0xFFC00000) >>> 0) === 0xBD000000) {
    const rt = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    const imm12 = (opcode >> 10) & 0xFFF;
    const offset = imm12 * 4;
    const base = rn === 31 ? Number(regs.getSP()) : Number(regs.getX(rn));
    const addr = base + offset;
    const buf = new ArrayBuffer(4);
    new DataView(buf).setFloat32(0, fpRegs[rt], true);
    mem.writeBytes(addr, new Uint8Array(buf));
    return;
  }
  // LDR Sn, [Xn, #imm12] (FP load, 32-bit): 0xBD400000
  if (((opcode & 0xFFC00000) >>> 0) === 0xBD400000) {
    const rt = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    const imm12 = (opcode >> 10) & 0xFFF;
    const offset = imm12 * 4;
    const base = rn === 31 ? Number(regs.getSP()) : Number(regs.getX(rn));
    const addr = base + offset;
    const buf = mem.readBytes(addr, 4);
    fpRegs[rt] = new DataView(buf.buffer, buf.byteOffset, 4).getFloat32(0, true);
    return;
  }

  // STUR Dn, [Xn, #simm9] (unscaled FP store, 64-bit): 0xFC000000 family
  if (((opcode & 0xFFE00C00) >>> 0) === 0xFC000000) {
    const rt = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    let simm9 = (opcode >> 12) & 0x1FF;
    if (simm9 & 0x100) simm9 |= ~0x1FF; // sign extend
    const base = rn === 31 ? Number(regs.getSP()) : Number(regs.getX(rn));
    const addr = base + simm9;
    const buf = new ArrayBuffer(8);
    new DataView(buf).setFloat64(0, fpRegs[rt], true);
    mem.writeBytes(addr, new Uint8Array(buf));
    return;
  }
  // LDUR Dn, [Xn, #simm9] (unscaled FP load, 64-bit): 0xFC400000 family
  if (((opcode & 0xFFE00C00) >>> 0) === 0xFC400000) {
    const rt = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    let simm9 = (opcode >> 12) & 0x1FF;
    if (simm9 & 0x100) simm9 |= ~0x1FF;
    const base = rn === 31 ? Number(regs.getSP()) : Number(regs.getX(rn));
    const addr = base + simm9;
    const buf = mem.readBytes(addr, 8);
    fpRegs[rt] = new DataView(buf.buffer, buf.byteOffset, 8).getFloat64(0, true);
    return;
  }

  // STP Dn, Dm, [Xn, #imm7] (FP store pair, 64-bit): opc=01 V=1 (0x6D000000..0x6D7FFFFF)
  if ((opcode & 0xFFC00000) === 0x6D000000) {
    const rt1 = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    const rt2 = (opcode >> 10) & 0x1F;
    let imm7 = (opcode >> 15) & 0x7F;
    if (imm7 & 0x40) imm7 |= ~0x7F;
    const offset = imm7 * 8;
    const base = rn === 31 ? Number(regs.getSP()) : Number(regs.getX(rn));
    const addr = base + offset;
    const buf1 = new ArrayBuffer(8); new DataView(buf1).setFloat64(0, fpRegs[rt1], true); mem.writeBytes(addr, new Uint8Array(buf1));
    const buf2 = new ArrayBuffer(8); new DataView(buf2).setFloat64(0, fpRegs[rt2], true); mem.writeBytes(addr + 8, new Uint8Array(buf2));
    return;
  }
  // LDP Dn, Dm, [Xn, #imm7] (FP load pair, 64-bit): 0x6D400000
  if ((opcode & 0xFFC00000) === 0x6D400000) {
    const rt1 = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    const rt2 = (opcode >> 10) & 0x1F;
    let imm7 = (opcode >> 15) & 0x7F;
    if (imm7 & 0x40) imm7 |= ~0x7F;
    const offset = imm7 * 8;
    const base = rn === 31 ? Number(regs.getSP()) : Number(regs.getX(rn));
    const addr = base + offset;
    const b1 = mem.readBytes(addr, 8); fpRegs[rt1] = new DataView(b1.buffer, b1.byteOffset, 8).getFloat64(0, true);
    const b2 = mem.readBytes(addr + 8, 8); fpRegs[rt2] = new DataView(b2.buffer, b2.byteOffset, 8).getFloat64(0, true);
    return;
  }

  // STP Qn, Qm, [Xn, #imm7] (128-bit pair): opc=10 V=1  (0xAD000000)
  if (((opcode & 0xFFC00000) >>> 0) === 0xAD000000) {
    const rt1 = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    const rt2 = (opcode >> 10) & 0x1F;
    let imm7 = (opcode >> 15) & 0x7F;
    if (imm7 & 0x40) imm7 |= ~0x7F;
    const offset = imm7 * 16;
    const base = rn === 31 ? Number(regs.getSP()) : Number(regs.getX(rn));
    const addr = base + offset;
    // Save lower 64 bits (D register) of each Q register
    const buf1 = new ArrayBuffer(16); new DataView(buf1).setFloat64(0, fpRegs[rt1], true); mem.writeBytes(addr, new Uint8Array(buf1));
    const buf2 = new ArrayBuffer(16); new DataView(buf2).setFloat64(0, fpRegs[rt2], true); mem.writeBytes(addr + 16, new Uint8Array(buf2));
    return;
  }
  // LDP Qn, Qm, [Xn, #imm7] (128-bit pair): 0xAD400000
  if (((opcode & 0xFFC00000) >>> 0) === 0xAD400000) {
    const rt1 = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    const rt2 = (opcode >> 10) & 0x1F;
    let imm7 = (opcode >> 15) & 0x7F;
    if (imm7 & 0x40) imm7 |= ~0x7F;
    const offset = imm7 * 16;
    const base = rn === 31 ? Number(regs.getSP()) : Number(regs.getX(rn));
    const addr = base + offset;
    const b1 = mem.readBytes(addr, 8); fpRegs[rt1] = new DataView(b1.buffer, b1.byteOffset, 8).getFloat64(0, true);
    const b2 = mem.readBytes(addr + 16, 8); fpRegs[rt2] = new DataView(b2.buffer, b2.byteOffset, 8).getFloat64(0, true);
    return;
  }

  // STP Dn, Dm, [Xn, #imm7]! (pre-index): 0x6D800000
  if ((opcode & 0xFFC00000) === 0x6D800000) {
    const rt1 = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    const rt2 = (opcode >> 10) & 0x1F;
    let imm7 = (opcode >> 15) & 0x7F;
    if (imm7 & 0x40) imm7 |= ~0x7F;
    const offset = imm7 * 8;
    const base = rn === 31 ? Number(regs.getSP()) : Number(regs.getX(rn));
    const addr = base + offset;
    const buf1 = new ArrayBuffer(8); new DataView(buf1).setFloat64(0, fpRegs[rt1], true); mem.writeBytes(addr, new Uint8Array(buf1));
    const buf2 = new ArrayBuffer(8); new DataView(buf2).setFloat64(0, fpRegs[rt2], true); mem.writeBytes(addr + 8, new Uint8Array(buf2));
    if (rn === 31) regs.setSP(BigInt(addr)); else regs.setX(rn, BigInt(addr));
    return;
  }
  // LDP Dn, Dm, [Xn], #imm7 (post-index): 0x6CC00000
  if ((opcode & 0xFFC00000) === 0x6CC00000) {
    const rt1 = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    const rt2 = (opcode >> 10) & 0x1F;
    let imm7 = (opcode >> 15) & 0x7F;
    if (imm7 & 0x40) imm7 |= ~0x7F;
    const base = rn === 31 ? Number(regs.getSP()) : Number(regs.getX(rn));
    const b1 = mem.readBytes(base, 8); fpRegs[rt1] = new DataView(b1.buffer, b1.byteOffset, 8).getFloat64(0, true);
    const b2 = mem.readBytes(base + 8, 8); fpRegs[rt2] = new DataView(b2.buffer, b2.byteOffset, 8).getFloat64(0, true);
    if (rn === 31) regs.setSP(BigInt(base + imm7 * 8)); else regs.setX(rn, BigInt(base + imm7 * 8));
    return;
  }
  // LDP Dn, Dm, [Xn, #imm7]! (pre-index load pair): 0x6DC00000
  if ((opcode & 0xFFC00000) === 0x6DC00000) {
    const rt1 = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    const rt2 = (opcode >> 10) & 0x1F;
    let imm7 = (opcode >> 15) & 0x7F;
    if (imm7 & 0x40) imm7 |= ~0x7F;
    const offset = imm7 * 8;
    const base = rn === 31 ? Number(regs.getSP()) : Number(regs.getX(rn));
    const addr = base + offset;
    const b1 = mem.readBytes(addr, 8); fpRegs[rt1] = new DataView(b1.buffer, b1.byteOffset, 8).getFloat64(0, true);
    const b2 = mem.readBytes(addr + 8, 8); fpRegs[rt2] = new DataView(b2.buffer, b2.byteOffset, 8).getFloat64(0, true);
    if (rn === 31) regs.setSP(BigInt(addr)); else regs.setX(rn, BigInt(addr));
    return;
  }
  // STP Qn, Qm, [Xn, #imm7]! (128-bit pre-index): 0xAD800000
  if (((opcode & 0xFFC00000) >>> 0) === 0xAD800000) {
    const rt1 = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    const rt2 = (opcode >> 10) & 0x1F;
    let imm7 = (opcode >> 15) & 0x7F;
    if (imm7 & 0x40) imm7 |= ~0x7F;
    const offset = imm7 * 16;
    const base = rn === 31 ? Number(regs.getSP()) : Number(regs.getX(rn));
    const addr = base + offset;
    const buf1 = new ArrayBuffer(16); new DataView(buf1).setFloat64(0, fpRegs[rt1], true); mem.writeBytes(addr, new Uint8Array(buf1));
    const buf2 = new ArrayBuffer(16); new DataView(buf2).setFloat64(0, fpRegs[rt2], true); mem.writeBytes(addr + 16, new Uint8Array(buf2));
    if (rn === 31) regs.setSP(BigInt(addr)); else regs.setX(rn, BigInt(addr));
    return;
  }
  // LDP Qn, Qm, [Xn], #imm7 (128-bit post-index): 0xACC00000
  if (((opcode & 0xFFC00000) >>> 0) === 0xACC00000) {
    const rt1 = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    const rt2 = (opcode >> 10) & 0x1F;
    let imm7 = (opcode >> 15) & 0x7F;
    if (imm7 & 0x40) imm7 |= ~0x7F;
    const base = rn === 31 ? Number(regs.getSP()) : Number(regs.getX(rn));
    const b1 = mem.readBytes(base, 8); fpRegs[rt1] = new DataView(b1.buffer, b1.byteOffset, 8).getFloat64(0, true);
    const b2 = mem.readBytes(base + 16, 8); fpRegs[rt2] = new DataView(b2.buffer, b2.byteOffset, 8).getFloat64(0, true);
    if (rn === 31) regs.setSP(BigInt(base + imm7 * 16)); else regs.setX(rn, BigInt(base + imm7 * 16));
    return;
  }
  // LDP Qn, Qm, [Xn, #imm7]! (128-bit pre-index load): 0xADC00000
  if (((opcode & 0xFFC00000) >>> 0) === 0xADC00000) {
    const rt1 = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    const rt2 = (opcode >> 10) & 0x1F;
    let imm7 = (opcode >> 15) & 0x7F;
    if (imm7 & 0x40) imm7 |= ~0x7F;
    const offset = imm7 * 16;
    const base = rn === 31 ? Number(regs.getSP()) : Number(regs.getX(rn));
    const addr = base + offset;
    const b1 = mem.readBytes(addr, 8); fpRegs[rt1] = new DataView(b1.buffer, b1.byteOffset, 8).getFloat64(0, true);
    const b2 = mem.readBytes(addr + 16, 8); fpRegs[rt2] = new DataView(b2.buffer, b2.byteOffset, 8).getFloat64(0, true);
    if (rn === 31) regs.setSP(BigInt(addr)); else regs.setX(rn, BigInt(addr));
    return;
  }
  // FCSEL Dd, Dn, Dm, cond
  if ((opcode & 0xFF200C00) === 0x1E200C00) {
    const rd = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    const cond = (opcode >> 12) & 0xF;
    const rm = (opcode >> 16) & 0x1F;
    // evaluate condition using flags
    let taken = false;
    switch (cond >> 1) {
      case 0: taken = flags.z; break;          // EQ/NE
      case 1: taken = flags.c; break;          // CS/CC
      case 2: taken = flags.n; break;          // MI/PL
      case 3: taken = flags.v; break;          // VS/VC
      case 4: taken = flags.c && !flags.z; break; // HI/LS
      case 5: taken = flags.n === flags.v; break;  // GE/LT
      case 6: taken = flags.n === flags.v && !flags.z; break; // GT/LE
      case 7: taken = true; break;             // AL
    }
    if (cond & 1) taken = !taken; // negate for odd conditions (NE, CC, etc.)
    fpRegs[rd] = taken ? fpRegs[rn] : fpRegs[rm];
    return;
  }

  // FP data-processing 3-source (double): FMADD/FMSUB/FNMADD/FNMSUB
  // 0 0 0 11111 01 o1 Rm o0 Ra Rn Rd  (type=01 for double)
  if ((opcode & 0xFF200000) === 0x1F400000) {
    const rd = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    const ra = (opcode >> 10) & 0x1F;
    const o0 = (opcode >> 15) & 1;
    const rm = (opcode >> 16) & 0x1F;
    const o1 = (opcode >> 21) & 1;
    if (o1 === 0 && o0 === 0) fpRegs[rd] = fpRegs[rn] * fpRegs[rm] + fpRegs[ra];       // FMADD
    else if (o1 === 0 && o0 === 1) fpRegs[rd] = -(fpRegs[rn] * fpRegs[rm]) + fpRegs[ra]; // FMSUB
    else if (o1 === 1 && o0 === 0) fpRegs[rd] = -(fpRegs[rn] * fpRegs[rm]) - fpRegs[ra]; // FNMADD
    else fpRegs[rd] = fpRegs[rn] * fpRegs[rm] - fpRegs[ra];                               // FNMSUB
    return;
  }

  // FCVTZU Xd, Dn (unsigned FP->int 64-bit): sf=1 type=01 rmode=11 opcode=001
  if (((opcode & 0xFFFFFC00) >>> 0) === 0x9E790000) {
    const rd = opcode & 0x1F;
    const rn = (opcode >> 5) & 0x1F;
    regs.setX(rd, BigInt(Math.trunc(Math.max(0, fpRegs[rn]))));
    return;
  }
}

export function execute(program: AssembledProgram): ExecutionResult {
  const regs = new RegisterFile();
  const mem = new Memory();
  const fpRegs = new Float64Array(32); // D0-D31
  let stdout = "";

  // Load data segment into memory
  if (program.dataSegment.length > 0) {
    mem.writeBytes(program.dataBase, program.dataSegment);
  }

  // PC starts at _start label if it exists, otherwise instruction 0
  regs.pc = program.labels.get("_start") ?? 0;
  let instrCount = 0;

  while (regs.pc >= 0 && regs.pc < program.instructions.length) {
    if (instrCount++ > MAX_INSTRUCTIONS) {
      return { stdout, error: "Execution limit exceeded (infinite loop?)", exitCode: 1 };
    }

    const instr = program.instructions[regs.pc];
    const ops = instr.operands;
    let nextPC = regs.pc + 1;

    try {
      switch (instr.op) {
        case "mov": {
          if (ops[1].kind === "imm") {
            const val = ops[1].value;
            writeReg(ops[0], regs, ops[0].kind === "reg32" ? BigInt.asUintN(32, val) : val);
          } else {
            const val = readReg(ops[1], regs);
            writeReg(ops[0], regs, ops[0].kind === "reg32" ? BigInt.asUintN(32, val) : val);
          }
          break;
        }

        case "movz": {
          const dst = getReg(ops[0]);
          let val = getImm(ops[1]);
          if (ops[2]?.kind === "shift" && ops[2].type === "lsl") {
            val = val << BigInt(ops[2].amount);
          }
          if (ops[0].kind === "reg32") {
            regs.setW(dst, Number(BigInt.asUintN(32, val)));
          } else {
            regs.setX(dst, BigInt.asUintN(64, val));
          }
          break;
        }

        case "movn": {
          const dst = getReg(ops[0]);
          let val = getImm(ops[1]);
          if (ops[2]?.kind === "shift" && ops[2].type === "lsl") {
            val = val << BigInt(ops[2].amount);
          }
          const inverted = BigInt.asUintN(64, ~val);
          if (ops[0].kind === "reg32") {
            regs.setW(dst, Number(BigInt.asUintN(32, inverted)));
          } else {
            regs.setX(dst, inverted);
          }
          break;
        }

        case "movk": {
          const dst = getReg(ops[0]);
          let imm = BigInt.asUintN(16, getImm(ops[1]));
          let shift = 0;
          if (ops[2]?.kind === "shift" && ops[2].type === "lsl") {
            shift = ops[2].amount;
          }
          const shiftBig = BigInt(shift);
          const mask = ~(0xFFFFn << shiftBig);
          const current = BigInt.asUintN(64, regs.getX(dst));
          const result = (current & BigInt.asUintN(64, mask)) | (imm << shiftBig);
          regs.setX(dst, BigInt.asUintN(64, result));
          break;
        }

        case "add": {
          const a = readReg(ops[1], regs);
          let b: bigint;
          if (ops[2].kind === "imm") {
            b = ops[2].value;
          } else {
            b = readReg(ops[2], regs);
          }
          writeReg(ops[0], regs, BigInt.asUintN(64, a + b));
          break;
        }

        case "adds": {
          const dst = getReg(ops[0]);
          const rn = getReg(ops[1]);
          const a = regs.getX(rn);
          let b: bigint;
          if (ops[2].kind === "imm") {
            b = ops[2].value;
          } else {
            b = regs.getX(getReg(ops[2]));
          }
          const result = BigInt.asUintN(64, a + b);
          regs.setX(dst, result);
          regs.setFlagsAddition(a, b);
          break;
        }

        case "sub": {
          const a = readReg(ops[1], regs);
          let b: bigint;
          if (ops[2].kind === "imm") {
            b = ops[2].value;
          } else {
            b = readReg(ops[2], regs);
          }
          writeReg(ops[0], regs, BigInt.asUintN(64, a - b));
          break;
        }

        case "subs": {
          const dst = getReg(ops[0]);
          const rn = getReg(ops[1]);
          const a = regs.getX(rn);
          let b: bigint;
          if (ops[2].kind === "imm") {
            b = ops[2].value;
          } else {
            b = regs.getX(getReg(ops[2]));
          }
          const result = BigInt.asUintN(64, a - b);
          regs.setX(dst, result);
          regs.setFlagsSubtraction(a, b);
          break;
        }

        case "mul": {
          const dst = getReg(ops[0]);
          const a = BigInt.asIntN(64, regs.getX(getReg(ops[1])));
          const b = BigInt.asIntN(64, regs.getX(getReg(ops[2])));
          regs.setX(dst, BigInt.asUintN(64, a * b));
          break;
        }

        case "sdiv": {
          const dst = getReg(ops[0]);
          const a = BigInt.asIntN(64, regs.getX(getReg(ops[1])));
          const b = BigInt.asIntN(64, regs.getX(getReg(ops[2])));
          if (b === 0n) {
            regs.setX(dst, 0n);
          } else {
            // Signed division truncates toward zero
            const result = a / b;
            regs.setX(dst, BigInt.asUintN(64, result));
          }
          break;
        }

        case "udiv": {
          const dst = getReg(ops[0]);
          const a = BigInt.asUintN(64, regs.getX(getReg(ops[1])));
          const b = BigInt.asUintN(64, regs.getX(getReg(ops[2])));
          if (b === 0n) {
            regs.setX(dst, 0n);
          } else {
            regs.setX(dst, a / b);
          }
          break;
        }

        case "neg": {
          const dst = getReg(ops[0]);
          const src = regs.getX(getReg(ops[1]));
          regs.setX(dst, BigInt.asUintN(64, -src));
          break;
        }

        case "and": {
          const dst = getReg(ops[0]);
          const a = regs.getX(getReg(ops[1]));
          let b: bigint;
          if (ops[2].kind === "imm") {
            b = BigInt.asUintN(64, ops[2].value);
          } else {
            b = regs.getX(getReg(ops[2]));
          }
          regs.setX(dst, BigInt.asUintN(64, a & b));
          break;
        }

        case "ands": {
          const dst = getReg(ops[0]);
          const a = regs.getX(getReg(ops[1]));
          let b: bigint;
          if (ops[2].kind === "imm") {
            b = BigInt.asUintN(64, ops[2].value);
          } else {
            b = regs.getX(getReg(ops[2]));
          }
          const result = BigInt.asUintN(64, a & b);
          regs.setX(dst, result);
          regs.setFlags64(result);
          break;
        }

        case "orr": {
          const dst = getReg(ops[0]);
          const a = regs.getX(getReg(ops[1]));
          let b: bigint;
          if (ops[2].kind === "imm") {
            b = BigInt.asUintN(64, ops[2].value);
          } else {
            b = regs.getX(getReg(ops[2]));
          }
          regs.setX(dst, BigInt.asUintN(64, a | b));
          break;
        }

        case "eor": {
          const dst = getReg(ops[0]);
          const a = regs.getX(getReg(ops[1]));
          let b: bigint;
          if (ops[2].kind === "imm") {
            b = BigInt.asUintN(64, ops[2].value);
          } else {
            b = regs.getX(getReg(ops[2]));
          }
          regs.setX(dst, BigInt.asUintN(64, a ^ b));
          break;
        }

        case "mvn": {
          const dst = getReg(ops[0]);
          const src = regs.getX(getReg(ops[1]));
          regs.setX(dst, BigInt.asUintN(64, ~src));
          break;
        }

        case "lsl": {
          const dst = getReg(ops[0]);
          const src = regs.getX(getReg(ops[1]));
          let amount: bigint;
          if (ops[2].kind === "imm") {
            amount = ops[2].value;
          } else {
            amount = BigInt.asUintN(6, regs.getX(getReg(ops[2])));
          }
          regs.setX(dst, BigInt.asUintN(64, src << amount));
          break;
        }

        case "lsr": {
          const dst = getReg(ops[0]);
          const src = BigInt.asUintN(64, regs.getX(getReg(ops[1])));
          let amount: bigint;
          if (ops[2].kind === "imm") {
            amount = ops[2].value;
          } else {
            amount = BigInt.asUintN(6, regs.getX(getReg(ops[2])));
          }
          regs.setX(dst, src >> amount);
          break;
        }

        case "asr": {
          const dst = getReg(ops[0]);
          const src = BigInt.asIntN(64, regs.getX(getReg(ops[1])));
          let amount: bigint;
          if (ops[2].kind === "imm") {
            amount = ops[2].value;
          } else {
            amount = BigInt.asUintN(6, regs.getX(getReg(ops[2])));
          }
          regs.setX(dst, BigInt.asUintN(64, src >> amount));
          break;
        }

        case "cmp": {
          const is32 = ops[0].kind === "reg32";
          const a = is32 ? BigInt(regs.getW(getReg(ops[0]))) : regs.getX(getReg(ops[0]));
          let b: bigint;
          if (ops[1].kind === "imm") {
            b = is32 ? BigInt.asUintN(32, ops[1].value) : ops[1].value;
          } else {
            b = is32 ? BigInt(regs.getW(getReg(ops[1]))) : regs.getX(getReg(ops[1]));
          }
          if (is32) {
            regs.setFlagsSubtraction32(a, b);
          } else {
            regs.setFlagsSubtraction(a, b);
          }
          break;
        }

        case "cmn": {
          const is32 = ops[0].kind === "reg32";
          const a = is32 ? BigInt(regs.getW(getReg(ops[0]))) : regs.getX(getReg(ops[0]));
          let b: bigint;
          if (ops[1].kind === "imm") {
            b = is32 ? BigInt.asUintN(32, ops[1].value) : ops[1].value;
          } else {
            b = is32 ? BigInt(regs.getW(getReg(ops[1]))) : regs.getX(getReg(ops[1]));
          }
          if (is32) {
            regs.setFlagsAddition32(a, b);
          } else {
            regs.setFlagsAddition(a, b);
          }
          break;
        }

        // Memory operations
        case "ldr": {
          const rt = getReg(ops[0]);
          const { addr, preWrite, postWrite } = resolveAddr(ops[1], regs);
          if (preWrite) preWrite();
          if (ops[0].kind === "reg32") {
            regs.setW(rt, mem.readU32(addr));
          } else {
            regs.setX(rt, mem.read64(addr));
          }
          if (postWrite) postWrite();
          break;
        }

        case "ldr_label": {
          const rt = getReg(ops[0]);
          const labelName = (ops[1] as { kind: "label"; name: string }).name;
          const addr = program.labels.get(labelName);
          if (addr === undefined) throw new Error(`Line ${instr.line}: Undefined label: ${labelName}`);
          regs.setX(rt, BigInt(addr));
          break;
        }

        case "ldrb": {
          const rt = getReg(ops[0]);
          const { addr, preWrite, postWrite } = resolveAddr(ops[1], regs);
          if (preWrite) preWrite();
          regs.setX(rt, BigInt(mem.readByte(addr)));
          if (postWrite) postWrite();
          break;
        }

        case "ldrb_label": {
          const rt = getReg(ops[0]);
          const labelName = (ops[1] as { kind: "label"; name: string }).name;
          const addr = program.labels.get(labelName);
          if (addr === undefined) throw new Error(`Line ${instr.line}: Undefined label: ${labelName}`);
          regs.setX(rt, BigInt(addr));
          break;
        }

        case "str": {
          const rt = getReg(ops[0]);
          const { addr, preWrite, postWrite } = resolveAddr(ops[1], regs);
          if (preWrite) preWrite();
          if (ops[0].kind === "reg32") {
            mem.writeU32(addr, regs.getW(rt));
          } else {
            mem.write64(addr, regs.getX(rt));
          }
          if (postWrite) postWrite();
          break;
        }

        case "strb": {
          const rt = getReg(ops[0]);
          const { addr, preWrite, postWrite } = resolveAddr(ops[1], regs);
          if (preWrite) preWrite();
          mem.writeByte(addr, Number(BigInt.asUintN(8, regs.getX(rt))));
          if (postWrite) postWrite();
          break;
        }

        case "ldrh": {
          const rt = getReg(ops[0]);
          const { addr, preWrite, postWrite } = resolveAddr(ops[1], regs);
          if (preWrite) preWrite();
          regs.setX(rt, BigInt(mem.readU16(addr)));
          if (postWrite) postWrite();
          break;
        }

        case "ldrh_label": {
          const rt = getReg(ops[0]);
          const labelName = (ops[1] as { kind: "label"; name: string }).name;
          const addr = program.labels.get(labelName);
          if (addr === undefined) throw new Error(`Line ${instr.line}: Undefined label: ${labelName}`);
          regs.setX(rt, BigInt(addr));
          break;
        }

        case "strh": {
          const rt = getReg(ops[0]);
          const { addr, preWrite, postWrite } = resolveAddr(ops[1], regs);
          if (preWrite) preWrite();
          mem.writeU16(addr, Number(BigInt.asUintN(16, regs.getX(rt))));
          if (postWrite) postWrite();
          break;
        }

        case "ldrsw": {
          const rt = getReg(ops[0]);
          const { addr, preWrite, postWrite } = resolveAddr(ops[1], regs);
          if (preWrite) preWrite();
          const val = mem.read32(addr); // sign-extended 32-bit read
          regs.setX(rt, BigInt(val)); // BigInt preserves sign
          if (postWrite) postWrite();
          break;
        }

        case "ldrsw_label": {
          const rt = getReg(ops[0]);
          const labelName = (ops[1] as { kind: "label"; name: string }).name;
          const addr = program.labels.get(labelName);
          if (addr === undefined) throw new Error(`Line ${instr.line}: Undefined label: ${labelName}`);
          regs.setX(rt, BigInt(addr));
          break;
        }

        case "sxtw": {
          const dst = getReg(ops[0]);
          const src = getReg(ops[1]);
          // Sign-extend 32-bit value to 64-bit
          const val32 = regs.getW(src);
          const signed = (val32 << 0) >> 0; // sign-extend via 32-bit shift
          regs.setX(dst, BigInt(signed));
          break;
        }

        case "uxtb": {
          const dst = getReg(ops[0]);
          const src = getReg(ops[1]);
          const val = regs.getW(src) & 0xff;
          regs.setW(dst, val);
          break;
        }

        case "uxth": {
          const dst = getReg(ops[0]);
          const src = getReg(ops[1]);
          const val = regs.getW(src) & 0xffff;
          regs.setW(dst, val);
          break;
        }

        case "tst": {
          const a = regs.getX(getReg(ops[0]));
          let b: bigint;
          if (ops[1].kind === "imm") {
            b = BigInt.asUintN(64, ops[1].value);
          } else {
            b = regs.getX(getReg(ops[1]));
          }
          const result = BigInt.asUintN(64, a & b);
          regs.setFlags64(result);
          break;
        }

        case "madd": {
          // MADD Xd, Xn, Xm, Xa — Xd = Xa + (Xn * Xm)
          const dst = getReg(ops[0]);
          const n = BigInt.asIntN(64, regs.getX(getReg(ops[1])));
          const m = BigInt.asIntN(64, regs.getX(getReg(ops[2])));
          const a = BigInt.asIntN(64, regs.getX(getReg(ops[3])));
          regs.setX(dst, BigInt.asUintN(64, a + n * m));
          break;
        }

        case "msub": {
          // MSUB Xd, Xn, Xm, Xa — Xd = Xa - (Xn * Xm)
          const dst = getReg(ops[0]);
          const n = BigInt.asIntN(64, regs.getX(getReg(ops[1])));
          const m = BigInt.asIntN(64, regs.getX(getReg(ops[2])));
          const a = BigInt.asIntN(64, regs.getX(getReg(ops[3])));
          regs.setX(dst, BigInt.asUintN(64, a - n * m));
          break;
        }

        case "cset": {
          // CSET Rd, cond — set Rd to 1 if cond is true, 0 otherwise
          const cond = (ops[1] as { kind: "cond"; code: number }).code;
          const val = regs.checkCondition(cond) ? 1n : 0n;
          writeReg(ops[0], regs, val);
          break;
        }

        case "csel": {
          // CSEL Rd, Rn, Rm, cond — Rd = cond ? Rn : Rm
          const cond = (ops[3] as { kind: "cond"; code: number }).code;
          const val = regs.checkCondition(cond) ? readReg(ops[1], regs) : readReg(ops[2], regs);
          writeReg(ops[0], regs, val);
          break;
        }

        case "csinc": {
          // CSINC Rd, Rn, Rm, cond — Rd = cond ? Rn : Rm + 1
          const cond = (ops[3] as { kind: "cond"; code: number }).code;
          const val = regs.checkCondition(cond) ? readReg(ops[1], regs) : readReg(ops[2], regs) + 1n;
          writeReg(ops[0], regs, BigInt.asUintN(64, val));
          break;
        }

        case "csinv": {
          // CSINV Rd, Rn, Rm, cond — Rd = cond ? Rn : ~Rm
          const cond = (ops[3] as { kind: "cond"; code: number }).code;
          const val = regs.checkCondition(cond) ? readReg(ops[1], regs) : BigInt.asUintN(64, ~readReg(ops[2], regs));
          writeReg(ops[0], regs, val);
          break;
        }

        case "csneg": {
          // CSNEG Rd, Rn, Rm, cond — Rd = cond ? Rn : -Rm
          const cond = (ops[3] as { kind: "cond"; code: number }).code;
          const val = regs.checkCondition(cond) ? readReg(ops[1], regs) : BigInt.asUintN(64, -readReg(ops[2], regs));
          writeReg(ops[0], regs, val);
          break;
        }

        case "ldp": {
          const rt1 = getReg(ops[0]);
          const rt2 = getReg(ops[1]);
          const { addr, preWrite, postWrite } = resolveAddr(ops[2], regs);
          if (preWrite) preWrite();
          regs.setX(rt1, mem.read64(addr));
          regs.setX(rt2, mem.read64(addr + 8));
          if (postWrite) postWrite();
          break;
        }

        case "stp": {
          const rt1 = getReg(ops[0]);
          const rt2 = getReg(ops[1]);
          const { addr, preWrite, postWrite } = resolveAddr(ops[2], regs);
          if (preWrite) preWrite();
          mem.write64(addr, regs.getX(rt1));
          mem.write64(addr + 8, regs.getX(rt2));
          if (postWrite) postWrite();
          break;
        }

        // Branch operations
        case "b": {
          const labelName = (ops[0] as { kind: "label"; name: string }).name;
          const target = program.labels.get(labelName);
          if (target === undefined) throw new Error(`Line ${instr.line}: Undefined label: ${labelName}`);
          nextPC = target;
          break;
        }

        case "bl": {
          const labelName = (ops[0] as { kind: "label"; name: string }).name;
          const target = program.labels.get(labelName);
          if (target === undefined) throw new Error(`Line ${instr.line}: Undefined label: ${labelName}`);
          regs.setX(30, BigInt(regs.pc + 1)); // LR = next instruction
          nextPC = target;
          break;
        }

        case "br": {
          const target = Number(regs.getX(getReg(ops[0])));
          nextPC = target;
          break;
        }

        case "blr": {
          const target = Number(regs.getX(getReg(ops[0])));
          regs.setX(30, BigInt(regs.pc + 1)); // LR = next instruction
          nextPC = target;
          break;
        }

        case "ret": {
          const reg = ops.length > 0 ? getReg(ops[0]) : 30;
          const target = Number(regs.getX(reg));
          nextPC = target;
          break;
        }

        case "b.cond": {
          const cond = (ops[0] as { kind: "cond"; code: number }).code;
          const labelName = (ops[1] as { kind: "label"; name: string }).name;
          const target = program.labels.get(labelName);
          if (target === undefined) throw new Error(`Line ${instr.line}: Undefined label: ${labelName}`);
          if (regs.checkCondition(cond)) {
            nextPC = target;
          }
          break;
        }

        case "cbz": {
          const val = regs.getX(getReg(ops[0]));
          const labelName = (ops[1] as { kind: "label"; name: string }).name;
          const target = program.labels.get(labelName);
          if (target === undefined) throw new Error(`Line ${instr.line}: Undefined label: ${labelName}`);
          if (val === 0n) nextPC = target;
          break;
        }

        case "cbnz": {
          const val = regs.getX(getReg(ops[0]));
          const labelName = (ops[1] as { kind: "label"; name: string }).name;
          const target = program.labels.get(labelName);
          if (target === undefined) throw new Error(`Line ${instr.line}: Undefined label: ${labelName}`);
          if (val !== 0n) nextPC = target;
          break;
        }

        case "svc": {
          const result = handleSyscall(regs, mem);
          if (result.stdout) stdout += result.stdout;
          if (result.exit) {
            return { stdout, error: "", exitCode: result.exitCode };
          }
          break;
        }

        case "adr":
        case "adrp": {
          const rt = getReg(ops[0]);
          const labelName = (ops[1] as { kind: "label"; name: string }).name;
          const addr = program.labels.get(labelName);
          if (addr === undefined) throw new Error(`Line ${instr.line}: Undefined label: ${labelName}`);
          regs.setX(rt, BigInt(addr));
          break;
        }

        case "nop":
          break;

        case "rawword": {
          const opcode = Number(getImm(ops[0]));
          executeFP(opcode, fpRegs, regs, mem);
          break;
        }

        default:
          throw new Error(`Line ${instr.line}: Unimplemented instruction: ${instr.op}`);
      }
    } catch (err) {
      const msg = err instanceof Error ? err.message : String(err);
      return { stdout, error: msg, exitCode: 1 };
    }

    regs.pc = nextPC;
  }

  return { stdout, error: "", exitCode: 0 };
}
