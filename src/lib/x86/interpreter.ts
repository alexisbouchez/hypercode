import type { AssembledProgram, Operand, RegSize } from "./assembler";
import { MAX_INSTRUCTIONS } from "./constants";
import { RegisterFile } from "./registers";
import { Memory } from "./memory";
import { handleSyscall } from "./syscalls";

export interface ExecutionResult {
  stdout: string;
  error: string;
  exitCode: number;
}

function readOperand(op: Operand, regs: RegisterFile, mem: Memory, labels: Map<string, number>, equLabels: Map<string, bigint>): bigint {
  switch (op.kind) {
    case "reg": {
      switch (op.size) {
        case "r64": return regs.get64(op.reg);
        case "r32": return BigInt(regs.get32(op.reg));
        case "r16": return BigInt(regs.get16(op.reg));
        case "r8": return BigInt(regs.get8(op.reg));
      }
      break;
    }
    case "imm":
      return op.value;
    case "mem": {
      const addr = resolveMemAddr(op, regs, labels, equLabels);
      switch (op.size) {
        case "r8": return BigInt(mem.readByte(addr));
        case "r16": return BigInt(mem.readU16(addr));
        case "r32": return BigInt(mem.readU32(addr));
        case "r64": return mem.readU64(addr);
      }
      break;
    }
    case "label": {
      // Label resolves to its address
      if (equLabels.has(op.name)) return equLabels.get(op.name)!;
      const addr = labels.get(op.name);
      if (addr === undefined) throw new Error(`Undefined label: ${op.name}`);
      return BigInt(addr);
    }
  }
  throw new Error(`Cannot read operand: ${op.kind}`);
}

function writeOperand(op: Operand, regs: RegisterFile, mem: Memory, labels: Map<string, number>, equLabels: Map<string, bigint>, value: bigint) {
  switch (op.kind) {
    case "reg": {
      switch (op.size) {
        case "r64": regs.set64(op.reg, value); return;
        case "r32": regs.set32(op.reg, Number(BigInt.asUintN(32, value))); return;
        case "r16": regs.set16(op.reg, Number(BigInt.asUintN(16, value))); return;
        case "r8": regs.set8(op.reg, Number(BigInt.asUintN(8, value))); return;
      }
      break;
    }
    case "mem": {
      const addr = resolveMemAddr(op, regs, labels, equLabels);
      switch (op.size) {
        case "r8": mem.writeByte(addr, Number(BigInt.asUintN(8, value))); return;
        case "r16": mem.writeU16(addr, Number(BigInt.asUintN(16, value))); return;
        case "r32": mem.writeU32(addr, Number(BigInt.asUintN(32, value))); return;
        case "r64": mem.writeU64(addr, value); return;
      }
      break;
    }
    default:
      throw new Error(`Cannot write to operand: ${op.kind}`);
  }
}

function resolveMemAddr(op: Operand & { kind: "mem" }, regs: RegisterFile, labels: Map<string, number>, equLabels: Map<string, bigint>): number {
  let addr = op.disp;

  if (op.baseLabel) {
    if (equLabels.has(op.baseLabel)) {
      addr += Number(equLabels.get(op.baseLabel)!);
    } else {
      const labelAddr = labels.get(op.baseLabel);
      if (labelAddr === undefined) throw new Error(`Undefined label: ${op.baseLabel}`);
      addr += labelAddr;
    }
  }

  if (op.base >= 0) {
    addr += Number(BigInt.asUintN(64, regs.get64(op.base)));
  }

  if (op.index >= 0) {
    addr += Number(BigInt.asUintN(64, regs.get64(op.index))) * op.scale;
  }

  return addr;
}

// Infer the operand size from the destination operand (for flags)
function inferSize(ops: Operand[]): RegSize {
  for (const op of ops) {
    if (op.kind === "reg") return op.size;
    if (op.kind === "mem") return op.size;
  }
  return "r64";
}

// Infer memory size from the other operand when memory operand has default size
function inferMemSize(dst: Operand, src: Operand): void {
  if (dst.kind === "mem" && src.kind === "reg") {
    dst.size = src.size;
  } else if (src.kind === "mem" && dst.kind === "reg") {
    src.size = dst.size;
  }
}

export function execute(program: AssembledProgram): ExecutionResult {
  const regs = new RegisterFile();
  const mem = new Memory();
  let stdout = "";

  // Load data segment into memory
  if (program.dataSegment.length > 0) {
    mem.writeBytes(program.dataBase, program.dataSegment);
  }

  // Start at _start label if it exists, otherwise instruction 0
  regs.rip = program.labels.get("_start") ?? 0;
  let instrCount = 0;

  while (regs.rip >= 0 && regs.rip < program.instructions.length) {
    if (instrCount++ > MAX_INSTRUCTIONS) {
      return { stdout, error: "Execution limit exceeded (infinite loop?)", exitCode: 1 };
    }

    const instr = program.instructions[regs.rip];
    const ops = instr.operands;
    let nextRIP = regs.rip + 1;

    try {
      switch (instr.op) {
        // ---------- Data movement ----------
        case "mov": {
          if (ops.length === 2) {
            inferMemSize(ops[0], ops[1]);
            const val = readOperand(ops[1], regs, mem, program.labels, program.equLabels);
            writeOperand(ops[0], regs, mem, program.labels, program.equLabels, val);
          }
          break;
        }

        case "movzx": {
          // Move with zero-extension
          const val = readOperand(ops[1], regs, mem, program.labels, program.equLabels);
          writeOperand(ops[0], regs, mem, program.labels, program.equLabels, BigInt.asUintN(64, val));
          break;
        }

        case "lea": {
          // Load effective address
          if (ops[1].kind === "mem") {
            const addr = resolveMemAddr(ops[1] as Operand & { kind: "mem" }, regs, program.labels, program.equLabels);
            writeOperand(ops[0], regs, mem, program.labels, program.equLabels, BigInt(addr));
          } else if (ops[1].kind === "label") {
            const addr = program.labels.get(ops[1].name);
            if (addr === undefined) throw new Error(`Line ${instr.line}: Undefined label: ${ops[1].name}`);
            writeOperand(ops[0], regs, mem, program.labels, program.equLabels, BigInt(addr));
          }
          break;
        }

        case "push": {
          const val = readOperand(ops[0], regs, mem, program.labels, program.equLabels);
          const rsp = regs.get64(4) - 8n; // rsp is register 4
          regs.set64(4, rsp);
          mem.writeU64(Number(BigInt.asUintN(64, rsp)), val);
          break;
        }

        case "pop": {
          const rsp = regs.get64(4);
          const val = mem.readU64(Number(BigInt.asUintN(64, rsp)));
          writeOperand(ops[0], regs, mem, program.labels, program.equLabels, val);
          regs.set64(4, rsp + 8n);
          break;
        }

        // ---------- Arithmetic ----------
        case "add": {
          inferMemSize(ops[0], ops[1]);
          const a = readOperand(ops[0], regs, mem, program.labels, program.equLabels);
          const b = readOperand(ops[1], regs, mem, program.labels, program.equLabels);
          const result = BigInt.asUintN(64, a + b);
          writeOperand(ops[0], regs, mem, program.labels, program.equLabels, result);
          regs.setFlagsArith64(result, a, b, false);
          break;
        }

        case "sub": {
          inferMemSize(ops[0], ops[1]);
          const a = readOperand(ops[0], regs, mem, program.labels, program.equLabels);
          const b = readOperand(ops[1], regs, mem, program.labels, program.equLabels);
          const result = BigInt.asUintN(64, a - b);
          writeOperand(ops[0], regs, mem, program.labels, program.equLabels, result);
          regs.setFlagsArith64(result, a, b, true);
          break;
        }

        case "inc": {
          const val = readOperand(ops[0], regs, mem, program.labels, program.equLabels);
          const result = BigInt.asUintN(64, val + 1n);
          writeOperand(ops[0], regs, mem, program.labels, program.equLabels, result);
          // inc doesn't affect CF
          const oldCF = regs.cf;
          regs.setFlagsArith64(result, val, 1n, false);
          regs.cf = oldCF;
          break;
        }

        case "dec": {
          const val = readOperand(ops[0], regs, mem, program.labels, program.equLabels);
          const result = BigInt.asUintN(64, val - 1n);
          writeOperand(ops[0], regs, mem, program.labels, program.equLabels, result);
          const oldCF = regs.cf;
          regs.setFlagsArith64(result, val, 1n, true);
          regs.cf = oldCF;
          break;
        }

        case "neg": {
          const val = readOperand(ops[0], regs, mem, program.labels, program.equLabels);
          const result = BigInt.asUintN(64, -val);
          writeOperand(ops[0], regs, mem, program.labels, program.equLabels, result);
          regs.setFlagsArith64(result, 0n, val, true);
          break;
        }

        case "mul": {
          // Unsigned multiply: rdx:rax = rax * operand
          const a = regs.get64(0); // rax
          const b = readOperand(ops[0], regs, mem, program.labels, program.equLabels);
          const result = BigInt.asUintN(128, a * b);
          regs.set64(0, BigInt.asUintN(64, result)); // low 64 bits -> rax
          regs.set64(2, BigInt.asUintN(64, result >> 64n)); // high 64 bits -> rdx
          break;
        }

        case "imul": {
          if (ops.length === 1) {
            // One operand: rdx:rax = rax * operand (signed)
            const a = BigInt.asIntN(64, regs.get64(0));
            const b = BigInt.asIntN(64, readOperand(ops[0], regs, mem, program.labels, program.equLabels));
            const result = a * b;
            regs.set64(0, BigInt.asUintN(64, result));
            regs.set64(2, BigInt.asUintN(64, result >> 64n));
          } else if (ops.length === 2) {
            // Two operand: dst = dst * src
            const a = BigInt.asIntN(64, readOperand(ops[0], regs, mem, program.labels, program.equLabels));
            const b = BigInt.asIntN(64, readOperand(ops[1], regs, mem, program.labels, program.equLabels));
            const result = BigInt.asUintN(64, a * b);
            writeOperand(ops[0], regs, mem, program.labels, program.equLabels, result);
          } else if (ops.length === 3) {
            // Three operand: dst = src1 * imm
            const a = BigInt.asIntN(64, readOperand(ops[1], regs, mem, program.labels, program.equLabels));
            const b = BigInt.asIntN(64, readOperand(ops[2], regs, mem, program.labels, program.equLabels));
            const result = BigInt.asUintN(64, a * b);
            writeOperand(ops[0], regs, mem, program.labels, program.equLabels, result);
          }
          break;
        }

        case "div": {
          // Unsigned divide: rax = rdx:rax / operand, rdx = rdx:rax % operand
          const divisor = readOperand(ops[0], regs, mem, program.labels, program.equLabels);
          if (divisor === 0n) throw new Error(`Line ${instr.line}: Division by zero`);
          const dividend = (BigInt.asUintN(64, regs.get64(2)) << 64n) | BigInt.asUintN(64, regs.get64(0));
          const quotient = dividend / divisor;
          const remainder = dividend % divisor;
          regs.set64(0, BigInt.asUintN(64, quotient));
          regs.set64(2, BigInt.asUintN(64, remainder));
          break;
        }

        case "idiv": {
          // Signed divide
          const divisor = BigInt.asIntN(64, readOperand(ops[0], regs, mem, program.labels, program.equLabels));
          if (divisor === 0n) throw new Error(`Line ${instr.line}: Division by zero`);
          const dividend = BigInt.asIntN(128, (BigInt.asIntN(64, regs.get64(2)) << 64n) | BigInt.asUintN(64, regs.get64(0)));
          const quotient = dividend / divisor;
          const remainder = dividend % divisor;
          regs.set64(0, BigInt.asUintN(64, quotient));
          regs.set64(2, BigInt.asUintN(64, remainder));
          break;
        }

        case "cqo": {
          // Sign-extend rax into rdx:rax
          const rax = BigInt.asIntN(64, regs.get64(0));
          regs.set64(2, rax < 0n ? BigInt.asUintN(64, -1n) : 0n);
          break;
        }

        case "cdq": {
          // Sign-extend eax into edx:eax
          const eax = regs.get32(0);
          regs.set32(2, (eax & 0x80000000) ? 0xFFFFFFFF : 0);
          break;
        }

        // ---------- Bitwise ----------
        case "and": {
          inferMemSize(ops[0], ops[1]);
          const a = readOperand(ops[0], regs, mem, program.labels, program.equLabels);
          const b = readOperand(ops[1], regs, mem, program.labels, program.equLabels);
          const result = BigInt.asUintN(64, a & b);
          writeOperand(ops[0], regs, mem, program.labels, program.equLabels, result);
          regs.setFlagsLogic64(result);
          break;
        }

        case "or": {
          inferMemSize(ops[0], ops[1]);
          const a = readOperand(ops[0], regs, mem, program.labels, program.equLabels);
          const b = readOperand(ops[1], regs, mem, program.labels, program.equLabels);
          const result = BigInt.asUintN(64, a | b);
          writeOperand(ops[0], regs, mem, program.labels, program.equLabels, result);
          regs.setFlagsLogic64(result);
          break;
        }

        case "xor": {
          inferMemSize(ops[0], ops[1]);
          const a = readOperand(ops[0], regs, mem, program.labels, program.equLabels);
          const b = readOperand(ops[1], regs, mem, program.labels, program.equLabels);
          const result = BigInt.asUintN(64, a ^ b);
          writeOperand(ops[0], regs, mem, program.labels, program.equLabels, result);
          regs.setFlagsLogic64(result);
          break;
        }

        case "not": {
          const val = readOperand(ops[0], regs, mem, program.labels, program.equLabels);
          writeOperand(ops[0], regs, mem, program.labels, program.equLabels, BigInt.asUintN(64, ~val));
          // NOT doesn't affect flags
          break;
        }

        case "shl":
        case "sal": {
          inferMemSize(ops[0], ops[1]);
          const val = readOperand(ops[0], regs, mem, program.labels, program.equLabels);
          const count = readOperand(ops[1], regs, mem, program.labels, program.equLabels) & 0x3Fn;
          const result = BigInt.asUintN(64, val << count);
          writeOperand(ops[0], regs, mem, program.labels, program.equLabels, result);
          regs.setFlagsLogic64(result);
          break;
        }

        case "shr": {
          inferMemSize(ops[0], ops[1]);
          const val = BigInt.asUintN(64, readOperand(ops[0], regs, mem, program.labels, program.equLabels));
          const count = readOperand(ops[1], regs, mem, program.labels, program.equLabels) & 0x3Fn;
          const result = val >> count;
          writeOperand(ops[0], regs, mem, program.labels, program.equLabels, result);
          regs.setFlagsLogic64(result);
          break;
        }

        case "sar": {
          inferMemSize(ops[0], ops[1]);
          const val = BigInt.asIntN(64, readOperand(ops[0], regs, mem, program.labels, program.equLabels));
          const count = readOperand(ops[1], regs, mem, program.labels, program.equLabels) & 0x3Fn;
          const result = BigInt.asUintN(64, val >> count);
          writeOperand(ops[0], regs, mem, program.labels, program.equLabels, result);
          regs.setFlagsLogic64(result);
          break;
        }

        // ---------- Comparison ----------
        case "cmp": {
          inferMemSize(ops[0], ops[1]);
          const a = readOperand(ops[0], regs, mem, program.labels, program.equLabels);
          const b = readOperand(ops[1], regs, mem, program.labels, program.equLabels);
          const result = BigInt.asUintN(64, a - b);
          regs.setFlagsArith64(result, a, b, true);
          break;
        }

        case "test": {
          inferMemSize(ops[0], ops[1]);
          const a = readOperand(ops[0], regs, mem, program.labels, program.equLabels);
          const b = readOperand(ops[1], regs, mem, program.labels, program.equLabels);
          const result = BigInt.asUintN(64, a & b);
          regs.setFlagsLogic64(result);
          break;
        }

        // ---------- Jumps ----------
        case "jmp": {
          const target = resolveJumpTarget(ops[0], program.labels, program.equLabels);
          nextRIP = target;
          break;
        }

        case "je":
        case "jz": {
          if (regs.zf) {
            nextRIP = resolveJumpTarget(ops[0], program.labels, program.equLabels);
          }
          break;
        }

        case "jne":
        case "jnz": {
          if (!regs.zf) {
            nextRIP = resolveJumpTarget(ops[0], program.labels, program.equLabels);
          }
          break;
        }

        case "jg":
        case "jnle": {
          // Greater (signed): ZF=0 and SF=OF
          if (!regs.zf && regs.sf === regs.of) {
            nextRIP = resolveJumpTarget(ops[0], program.labels, program.equLabels);
          }
          break;
        }

        case "jge":
        case "jnl": {
          // Greater or equal (signed): SF=OF
          if (regs.sf === regs.of) {
            nextRIP = resolveJumpTarget(ops[0], program.labels, program.equLabels);
          }
          break;
        }

        case "jl":
        case "jnge": {
          // Less (signed): SF!=OF
          if (regs.sf !== regs.of) {
            nextRIP = resolveJumpTarget(ops[0], program.labels, program.equLabels);
          }
          break;
        }

        case "jle":
        case "jng": {
          // Less or equal (signed): ZF=1 or SF!=OF
          if (regs.zf || regs.sf !== regs.of) {
            nextRIP = resolveJumpTarget(ops[0], program.labels, program.equLabels);
          }
          break;
        }

        case "ja":
        case "jnbe": {
          // Above (unsigned): CF=0 and ZF=0
          if (!regs.cf && !regs.zf) {
            nextRIP = resolveJumpTarget(ops[0], program.labels, program.equLabels);
          }
          break;
        }

        case "jae":
        case "jnb": {
          // Above or equal (unsigned): CF=0
          if (!regs.cf) {
            nextRIP = resolveJumpTarget(ops[0], program.labels, program.equLabels);
          }
          break;
        }

        case "jb":
        case "jnae": {
          // Below (unsigned): CF=1
          if (regs.cf) {
            nextRIP = resolveJumpTarget(ops[0], program.labels, program.equLabels);
          }
          break;
        }

        case "jbe":
        case "jna": {
          // Below or equal (unsigned): CF=1 or ZF=1
          if (regs.cf || regs.zf) {
            nextRIP = resolveJumpTarget(ops[0], program.labels, program.equLabels);
          }
          break;
        }

        // ---------- SETcc ----------
        case "sete":
        case "setz": {
          writeOperand(ops[0], regs, mem, program.labels, program.equLabels, regs.zf ? 1n : 0n);
          break;
        }

        case "setne":
        case "setnz": {
          writeOperand(ops[0], regs, mem, program.labels, program.equLabels, !regs.zf ? 1n : 0n);
          break;
        }

        case "setg": {
          writeOperand(ops[0], regs, mem, program.labels, program.equLabels, (!regs.zf && regs.sf === regs.of) ? 1n : 0n);
          break;
        }

        case "setge": {
          writeOperand(ops[0], regs, mem, program.labels, program.equLabels, (regs.sf === regs.of) ? 1n : 0n);
          break;
        }

        case "setl": {
          writeOperand(ops[0], regs, mem, program.labels, program.equLabels, (regs.sf !== regs.of) ? 1n : 0n);
          break;
        }

        case "setle": {
          writeOperand(ops[0], regs, mem, program.labels, program.equLabels, (regs.zf || regs.sf !== regs.of) ? 1n : 0n);
          break;
        }

        case "seta": {
          writeOperand(ops[0], regs, mem, program.labels, program.equLabels, (!regs.cf && !regs.zf) ? 1n : 0n);
          break;
        }

        case "setb": {
          writeOperand(ops[0], regs, mem, program.labels, program.equLabels, regs.cf ? 1n : 0n);
          break;
        }

        // ---------- Function call ----------
        case "call": {
          // Push return address, jump to target
          const rsp = regs.get64(4) - 8n;
          regs.set64(4, rsp);
          mem.writeU64(Number(BigInt.asUintN(64, rsp)), BigInt(regs.rip + 1));
          nextRIP = resolveJumpTarget(ops[0], program.labels, program.equLabels);
          break;
        }

        case "ret": {
          // Pop return address, jump to it
          const rsp = regs.get64(4);
          const retAddr = mem.readU64(Number(BigInt.asUintN(64, rsp)));
          regs.set64(4, rsp + 8n);
          nextRIP = Number(retAddr);
          break;
        }

        // ---------- syscall ----------
        case "syscall": {
          const result = handleSyscall(regs, mem);
          if (result.stdout) stdout += result.stdout;
          if (result.exit) {
            return { stdout, error: "", exitCode: result.exitCode };
          }
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

    regs.rip = nextRIP;
  }

  return { stdout, error: "", exitCode: 0 };
}

function resolveJumpTarget(op: Operand, labels: Map<string, number>, equLabels: Map<string, bigint>): number {
  if (op.kind === "label") {
    const target = labels.get(op.name);
    if (target === undefined) throw new Error(`Undefined label: ${op.name}`);
    return target;
  }
  if (op.kind === "imm") {
    return Number(op.value);
  }
  if (op.kind === "reg") {
    // indirect jump via register — not typical for lessons but handle it
    throw new Error("Indirect jumps via register not yet supported");
  }
  throw new Error(`Invalid jump target: ${op.kind}`);
}
