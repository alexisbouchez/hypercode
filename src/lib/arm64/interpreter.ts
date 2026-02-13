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
    const offVal = Number(BigInt.asUintN(64, regs.getX(op.offsetReg)));
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

export function execute(program: AssembledProgram): ExecutionResult {
  const regs = new RegisterFile();
  const mem = new Memory();
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
