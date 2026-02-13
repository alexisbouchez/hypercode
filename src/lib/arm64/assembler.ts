import { REG_NAMES_64, REG_NAMES_32, COND_CODES, CODE_BASE, DATA_BASE } from "./constants";

// ---------- Instruction IR ----------

export type Operand =
  | { kind: "reg64"; reg: number; sp?: boolean }
  | { kind: "reg32"; reg: number }
  | { kind: "imm"; value: bigint }
  | { kind: "mem"; base: number; offset: number; preIndex: boolean; postIndex: boolean; useSP: boolean }
  | { kind: "mem_reg"; base: number; offsetReg: number; useSP: boolean }
  | { kind: "label"; name: string }
  | { kind: "shift"; type: "lsl" | "lsr" | "asr"; amount: number }
  | { kind: "cond"; code: number };

export interface Instruction {
  op: string;
  operands: Operand[];
  line: number;
}

export interface AssembledProgram {
  instructions: Instruction[];
  dataSegment: Uint8Array;
  dataBase: number;
  labels: Map<string, number>; // label -> instruction index or data address
  codeBase: number;
}

// ---------- Parser Helpers ----------

function stripComments(line: string): string {
  // Handle comments: // or /* */ or ; or #-style at start for preprocessor
  let inStr = false;
  for (let i = 0; i < line.length; i++) {
    const ch = line[i];
    if (ch === '"') inStr = !inStr;
    if (!inStr) {
      if (ch === "/" && line[i + 1] === "/") return line.slice(0, i);
      if (ch === ";") return line.slice(0, i);
    }
  }
  return line;
}

function parseRegister(token: string): Operand | null {
  const lower = token.toLowerCase();
  if (lower in REG_NAMES_64) {
    const isSP = lower === "sp";
    const reg = REG_NAMES_64[lower];
    if (isSP) return { kind: "reg64", reg, sp: true };
    return { kind: "reg64", reg };
  }
  if (lower in REG_NAMES_32) {
    return { kind: "reg32", reg: REG_NAMES_32[lower] };
  }
  return null;
}

function parseImmediate(token: string): bigint | null {
  let s = token;
  if (s.startsWith("#")) s = s.slice(1);
  s = s.trim();
  if (s === "") return null;

  const neg = s.startsWith("-");
  if (neg) s = s.slice(1);

  let val: bigint;
  if (s.startsWith("0x") || s.startsWith("0X")) {
    val = BigInt(s);
  } else if (s.startsWith("0b") || s.startsWith("0B")) {
    val = BigInt(s);
  } else if (/^\d+$/.test(s)) {
    val = BigInt(s);
  } else {
    return null;
  }

  return neg ? -val : val;
}

function parseShift(token: string): { type: "lsl" | "lsr" | "asr"; amount: number } | null {
  const lower = token.toLowerCase().trim();
  const m = /^(lsl|lsr|asr)\s+#?(\d+)$/.exec(lower);
  if (m) {
    return { type: m[1] as "lsl" | "lsr" | "asr", amount: parseInt(m[2]) };
  }
  return null;
}

// Tokenize an operand list, respecting brackets
function tokenizeOperands(raw: string): string[] {
  const tokens: string[] = [];
  let current = "";
  let bracketDepth = 0;

  for (const ch of raw) {
    if (ch === "[") {
      bracketDepth++;
      current += ch;
    } else if (ch === "]") {
      bracketDepth--;
      current += ch;
    } else if (ch === "," && bracketDepth === 0) {
      tokens.push(current.trim());
      current = "";
    } else {
      current += ch;
    }
  }

  if (current.trim()) tokens.push(current.trim());
  return tokens;
}

// Parse a memory operand like [X0], [X0, #8], [X0, #8]!, [X0], #8
// Returns the memory operand and any post-index operand
function parseMemoryOperand(tokens: string[], startIdx: number): { mem: Operand; consumed: number } | null {
  const token = tokens[startIdx];
  if (!token) return null;

  // Post-index: [Xn], #imm  (two separate tokens)
  const postMatch = /^\[(\w+)\]$/.exec(token);
  if (postMatch) {
    const baseReg = postMatch[1].toLowerCase();
    const regNum = REG_NAMES_64[baseReg];
    if (regNum === undefined) return null;
    const useSP = baseReg === "sp";

    // Check if next token is an immediate (post-index)
    const nextToken = tokens[startIdx + 1];
    if (nextToken) {
      const imm = parseImmediate(nextToken);
      if (imm !== null) {
        return {
          mem: { kind: "mem", base: regNum, offset: Number(imm), preIndex: false, postIndex: true, useSP },
          consumed: 2,
        };
      }
    }

    // Simple [Xn]
    return {
      mem: { kind: "mem", base: regNum, offset: 0, preIndex: false, postIndex: false, useSP },
      consumed: 1,
    };
  }

  // Pre-index: [Xn, #imm]!
  const preMatch = /^\[(\w+),\s*#?(-?\d+|0x[0-9a-fA-F]+)\]!$/.exec(token);
  if (preMatch) {
    const baseReg = preMatch[1].toLowerCase();
    const regNum = REG_NAMES_64[baseReg];
    if (regNum === undefined) return null;
    const useSP = baseReg === "sp";
    const offset = parseInt(preMatch[2]);
    return {
      mem: { kind: "mem", base: regNum, offset, preIndex: true, postIndex: false, useSP },
      consumed: 1,
    };
  }

  // Offset: [Xn, #imm] or [Xn]
  const offMatch = /^\[(\w+)(?:,\s*#?(-?\d+|0x[0-9a-fA-F]+))?\]$/.exec(token);
  if (offMatch) {
    const baseReg = offMatch[1].toLowerCase();
    const regNum = REG_NAMES_64[baseReg];
    if (regNum === undefined) return null;
    const useSP = baseReg === "sp";
    const offset = offMatch[2] ? parseInt(offMatch[2]) : 0;
    return {
      mem: { kind: "mem", base: regNum, offset, preIndex: false, postIndex: false, useSP },
      consumed: 1,
    };
  }

  // Register offset: [Xn, Xm]
  const regOffMatch = /^\[(\w+),\s*(\w+)\]$/.exec(token);
  if (regOffMatch) {
    const baseReg = regOffMatch[1].toLowerCase();
    const offReg = regOffMatch[2].toLowerCase();
    const baseNum = REG_NAMES_64[baseReg];
    const offNum = REG_NAMES_64[offReg] ?? REG_NAMES_32[offReg];
    if (baseNum === undefined || offNum === undefined) return null;
    const useSP = baseReg === "sp";
    return {
      mem: { kind: "mem_reg", base: baseNum, offsetReg: offNum, useSP },
      consumed: 1,
    };
  }

  return null;
}

// ---------- Main Assembler ----------

export function assemble(source: string): AssembledProgram {
  const lines = source.split("\n");
  const instructions: Instruction[] = [];
  const labels = new Map<string, number>();
  const dataLabels = new Map<string, number>();
  const dataBytes: number[] = [];
  let currentSection: "text" | "data" = "text";

  // First pass: collect labels and directives
  // We do two passes: first to collect labels, second to parse instructions

  // Parse all lines into an intermediate form
  interface ParsedLine {
    lineNum: number;
    label?: string;
    directive?: { name: string; args: string };
    instruction?: { mnemonic: string; operandsRaw: string };
    section?: "text" | "data";
  }

  const parsed: ParsedLine[] = [];

  for (let i = 0; i < lines.length; i++) {
    let line = stripComments(lines[i]).trim();
    if (!line) continue;

    const entry: ParsedLine = { lineNum: i + 1 };

    // Check for label
    const labelMatch = /^(\w+):(.*)$/.exec(line);
    if (labelMatch) {
      entry.label = labelMatch[1];
      line = labelMatch[2].trim();
      if (!line) {
        parsed.push(entry);
        continue;
      }
    }

    // Check for directives
    if (line.startsWith(".")) {
      const dirMatch = /^\.(\w+)\s*(.*)$/.exec(line);
      if (dirMatch) {
        const dirName = dirMatch[1].toLowerCase();
        const dirArgs = dirMatch[2].trim();

        if (dirName === "text") {
          entry.section = "text";
        } else if (dirName === "data") {
          entry.section = "data";
        } else if (dirName === "global" || dirName === "globl") {
          // ignored for our purposes
        } else {
          entry.directive = { name: dirName, args: dirArgs };
        }

        parsed.push(entry);
        continue;
      }
    }

    // It's an instruction
    const spaceIdx = line.indexOf(" ");
    if (spaceIdx === -1) {
      entry.instruction = { mnemonic: line, operandsRaw: "" };
    } else {
      entry.instruction = {
        mnemonic: line.slice(0, spaceIdx),
        operandsRaw: line.slice(spaceIdx + 1).trim(),
      };
    }

    parsed.push(entry);
  }

  // Process parsed lines: first pass to assign label addresses
  currentSection = "text";
  let instrIndex = 0;
  let dataOffset = 0;

  for (const p of parsed) {
    if (p.section) currentSection = p.section;

    if (p.label) {
      if (currentSection === "text") {
        labels.set(p.label, instrIndex);
      } else {
        dataLabels.set(p.label, DATA_BASE + dataOffset);
        labels.set(p.label, DATA_BASE + dataOffset);
      }
    }

    if (currentSection === "data" && p.directive) {
      switch (p.directive.name) {
        case "ascii": {
          const str = parseDirectiveString(p.directive.args);
          dataOffset += str.length;
          break;
        }
        case "asciz":
        case "string": {
          const str = parseDirectiveString(p.directive.args);
          dataOffset += str.length + 1; // null terminator
          break;
        }
        case "byte": {
          const vals = p.directive.args.split(",").map((s) => s.trim());
          dataOffset += vals.length;
          break;
        }
        case "word":
        case "4byte": {
          const vals = p.directive.args.split(",").map((s) => s.trim());
          dataOffset += vals.length * 4;
          break;
        }
        case "quad":
        case "8byte": {
          const vals = p.directive.args.split(",").map((s) => s.trim());
          dataOffset += vals.length * 8;
          break;
        }
        case "align":
        case "balign": {
          const align = parseInt(p.directive.args) || 4;
          const rem = dataOffset % align;
          if (rem !== 0) dataOffset += align - rem;
          break;
        }
        case "skip":
        case "space": {
          const size = parseInt(p.directive.args) || 1;
          dataOffset += size;
          break;
        }
      }
    }

    if (currentSection === "text" && p.instruction) {
      instrIndex++;
    }
  }

  // Second pass: generate data segment and instructions
  currentSection = "text";
  dataOffset = 0;

  for (const p of parsed) {
    if (p.section) currentSection = p.section;

    if (currentSection === "data" && p.directive) {
      switch (p.directive.name) {
        case "ascii": {
          const bytes = encodeDirectiveString(p.directive.args);
          for (const b of bytes) dataBytes.push(b);
          break;
        }
        case "asciz":
        case "string": {
          const bytes = encodeDirectiveString(p.directive.args);
          for (const b of bytes) dataBytes.push(b);
          dataBytes.push(0); // null terminator
          break;
        }
        case "byte": {
          const vals = p.directive.args.split(",").map((s) => s.trim());
          for (const v of vals) {
            dataBytes.push(parseInt(v) & 0xff);
          }
          break;
        }
        case "word":
        case "4byte": {
          const vals = p.directive.args.split(",").map((s) => s.trim());
          for (const v of vals) {
            const n = parseInt(v);
            dataBytes.push(n & 0xff, (n >> 8) & 0xff, (n >> 16) & 0xff, (n >> 24) & 0xff);
          }
          break;
        }
        case "quad":
        case "8byte": {
          const vals = p.directive.args.split(",").map((s) => s.trim());
          for (const v of vals) {
            const n = BigInt(v.startsWith("0x") ? v : parseInt(v));
            for (let b = 0; b < 8; b++) {
              dataBytes.push(Number((n >> BigInt(b * 8)) & 0xFFn));
            }
          }
          break;
        }
        case "align":
        case "balign": {
          const align = parseInt(p.directive.args) || 4;
          while (dataBytes.length % align !== 0) dataBytes.push(0);
          break;
        }
        case "skip":
        case "space": {
          const parts = p.directive.args.split(",").map((s) => s.trim());
          const size = parseInt(parts[0]) || 1;
          const fill = parts[1] ? parseInt(parts[1]) & 0xff : 0;
          for (let j = 0; j < size; j++) dataBytes.push(fill);
          break;
        }
      }
    }

    if (currentSection === "text" && p.instruction) {
      const instr = parseInstruction(p.instruction.mnemonic, p.instruction.operandsRaw, p.lineNum);
      instructions.push(instr);
    }
  }

  return {
    instructions,
    dataSegment: new Uint8Array(dataBytes),
    dataBase: DATA_BASE,
    labels,
    codeBase: CODE_BASE,
  };
}

// ---------- Directive String Parsing ----------

function parseDirectiveString(raw: string): Uint8Array {
  const bytes: number[] = [];
  // Remove surrounding quotes
  const m = /^"(.*)"$/.exec(raw.trim());
  if (!m) return new Uint8Array(0);

  const str = m[1];
  for (let i = 0; i < str.length; i++) {
    if (str[i] === "\\" && i + 1 < str.length) {
      i++;
      switch (str[i]) {
        case "n": bytes.push(10); break;
        case "t": bytes.push(9); break;
        case "r": bytes.push(13); break;
        case "0": bytes.push(0); break;
        case "\\": bytes.push(92); break;
        case '"': bytes.push(34); break;
        default: bytes.push(str.charCodeAt(i)); break;
      }
    } else {
      bytes.push(str.charCodeAt(i));
    }
  }

  return new Uint8Array(bytes);
}

function encodeDirectiveString(raw: string): Uint8Array {
  return parseDirectiveString(raw);
}

// ---------- Instruction Parsing ----------

function parseInstruction(mnemonic: string, operandsRaw: string, lineNum: number): Instruction {
  const mn = mnemonic.toLowerCase();
  const tokens = operandsRaw ? tokenizeOperands(operandsRaw) : [];
  const operands: Operand[] = [];

  // Handle B.cond
  if (mn.startsWith("b.")) {
    const condName = mn.slice(2);
    const condCode = COND_CODES[condName];
    if (condCode === undefined) throw new Error(`Line ${lineNum}: Unknown condition: ${condName}`);
    operands.push({ kind: "cond", code: condCode });
    if (tokens[0]) {
      operands.push({ kind: "label", name: tokens[0] });
    }
    return { op: "b.cond", operands, line: lineNum };
  }

  // Parse operands based on instruction type
  switch (mn) {
    case "mov":
    case "movz":
    case "movk":
    case "movn": {
      const dst = parseRegister(tokens[0]);
      if (!dst) throw new Error(`Line ${lineNum}: Invalid register: ${tokens[0]}`);
      operands.push(dst);

      // Second operand: register or immediate
      const src = parseRegister(tokens[1]);
      if (src) {
        operands.push(src);
      } else {
        const imm = parseImmediate(tokens[1]);
        if (imm === null) throw new Error(`Line ${lineNum}: Invalid operand: ${tokens[1]}`);
        operands.push({ kind: "imm", value: imm });
      }

      // Optional shift for MOVZ/MOVK
      if (tokens[2]) {
        const shift = parseShift(tokens[2]);
        if (shift) operands.push({ kind: "shift", ...shift });
      }
      return { op: mn, operands, line: lineNum };
    }

    case "add":
    case "sub":
    case "adds":
    case "subs":
    case "and":
    case "ands":
    case "orr":
    case "eor":
    case "lsl":
    case "lsr":
    case "asr": {
      // Three operands: Rd, Rn, Rm/#imm
      for (let i = 0; i < Math.min(tokens.length, 3); i++) {
        const reg = parseRegister(tokens[i]);
        if (reg) {
          operands.push(reg);
        } else {
          const imm = parseImmediate(tokens[i]);
          if (imm !== null) {
            operands.push({ kind: "imm", value: imm });
          } else {
            throw new Error(`Line ${lineNum}: Invalid operand: ${tokens[i]}`);
          }
        }
      }
      return { op: mn, operands, line: lineNum };
    }

    case "mul":
    case "sdiv":
    case "udiv": {
      // Three register operands: Rd, Rn, Rm
      for (const t of tokens) {
        const reg = parseRegister(t);
        if (!reg) throw new Error(`Line ${lineNum}: Expected register: ${t}`);
        operands.push(reg);
      }
      return { op: mn, operands, line: lineNum };
    }

    case "neg": {
      // NEG Xd, Xm
      for (const t of tokens) {
        const reg = parseRegister(t);
        if (!reg) throw new Error(`Line ${lineNum}: Expected register: ${t}`);
        operands.push(reg);
      }
      return { op: mn, operands, line: lineNum };
    }

    case "mvn": {
      // MVN Xd, Xm
      for (const t of tokens) {
        const reg = parseRegister(t);
        if (!reg) throw new Error(`Line ${lineNum}: Expected register: ${t}`);
        operands.push(reg);
      }
      return { op: mn, operands, line: lineNum };
    }

    case "cmp":
    case "cmn": {
      // CMP Xn, Xm/#imm
      const rn = parseRegister(tokens[0]);
      if (!rn) throw new Error(`Line ${lineNum}: Expected register: ${tokens[0]}`);
      operands.push(rn);

      const rm = parseRegister(tokens[1]);
      if (rm) {
        operands.push(rm);
      } else {
        const imm = parseImmediate(tokens[1]);
        if (imm === null) throw new Error(`Line ${lineNum}: Invalid operand: ${tokens[1]}`);
        operands.push({ kind: "imm", value: imm });
      }
      return { op: mn, operands, line: lineNum };
    }

    case "sxtw": {
      // SXTW Xd, Wn — sign-extend 32-bit to 64-bit
      const dst = parseRegister(tokens[0]);
      if (!dst) throw new Error(`Line ${lineNum}: Invalid register: ${tokens[0]}`);
      operands.push(dst);
      const src = parseRegister(tokens[1]);
      if (!src) throw new Error(`Line ${lineNum}: Invalid register: ${tokens[1]}`);
      operands.push(src);
      return { op: mn, operands, line: lineNum };
    }

    case "uxtb":
    case "uxth": {
      const dst = parseRegister(tokens[0]);
      if (!dst) throw new Error(`Line ${lineNum}: Invalid register: ${tokens[0]}`);
      operands.push(dst);
      const src = parseRegister(tokens[1]);
      if (!src) throw new Error(`Line ${lineNum}: Invalid register: ${tokens[1]}`);
      operands.push(src);
      return { op: mn, operands, line: lineNum };
    }

    case "tst": {
      // TST Xn, Xm/#imm (ANDS XZR, Xn, Xm/#imm)
      const rn = parseRegister(tokens[0]);
      if (!rn) throw new Error(`Line ${lineNum}: Expected register: ${tokens[0]}`);
      operands.push(rn);

      const rm = parseRegister(tokens[1]);
      if (rm) {
        operands.push(rm);
      } else {
        const imm = parseImmediate(tokens[1]);
        if (imm === null) throw new Error(`Line ${lineNum}: Invalid operand: ${tokens[1]}`);
        operands.push({ kind: "imm", value: imm });
      }
      return { op: mn, operands, line: lineNum };
    }

    case "madd":
    case "msub": {
      // MADD/MSUB Xd, Xn, Xm, Xa
      for (const t of tokens) {
        const reg = parseRegister(t);
        if (!reg) throw new Error(`Line ${lineNum}: Expected register: ${t}`);
        operands.push(reg);
      }
      return { op: mn, operands, line: lineNum };
    }

    case "ldr":
    case "ldrb":
    case "ldrh":
    case "ldrsw":
    case "str":
    case "strb":
    case "strh": {
      // Rt, [Xn, #offset] or =label for LDR
      const rt = parseRegister(tokens[0]);
      if (!rt) throw new Error(`Line ${lineNum}: Expected register: ${tokens[0]}`);
      operands.push(rt);

      // Check for =label (LDR pseudo-instruction)
      if (tokens[1] && tokens[1].startsWith("=")) {
        const labelName = tokens[1].slice(1);
        operands.push({ kind: "label", name: labelName });
        return { op: mn + "_label", operands, line: lineNum };
      }

      const mem = parseMemoryOperand(tokens, 1);
      if (!mem) throw new Error(`Line ${lineNum}: Invalid memory operand: ${tokens.slice(1).join(", ")}`);
      operands.push(mem.mem);
      // Check for post-index immediate already handled by parseMemoryOperand
      return { op: mn, operands, line: lineNum };
    }

    case "ldp":
    case "stp": {
      // Rt1, Rt2, [Xn, #offset]
      const rt1 = parseRegister(tokens[0]);
      const rt2 = parseRegister(tokens[1]);
      if (!rt1 || !rt2) throw new Error(`Line ${lineNum}: Expected two registers`);
      operands.push(rt1);
      operands.push(rt2);

      const mem = parseMemoryOperand(tokens, 2);
      if (!mem) throw new Error(`Line ${lineNum}: Invalid memory operand: ${tokens.slice(2).join(", ")}`);
      operands.push(mem.mem);
      return { op: mn, operands, line: lineNum };
    }

    case "b":
    case "bl": {
      operands.push({ kind: "label", name: tokens[0] });
      return { op: mn, operands, line: lineNum };
    }

    case "br": {
      const reg = parseRegister(tokens[0]);
      if (!reg) throw new Error(`Line ${lineNum}: Expected register: ${tokens[0]}`);
      operands.push(reg);
      return { op: mn, operands, line: lineNum };
    }

    case "ret": {
      // RET or RET Xn (defaults to X30)
      if (tokens[0]) {
        const reg = parseRegister(tokens[0]);
        if (reg) operands.push(reg);
      }
      return { op: mn, operands, line: lineNum };
    }

    case "cbz":
    case "cbnz": {
      const reg = parseRegister(tokens[0]);
      if (!reg) throw new Error(`Line ${lineNum}: Expected register: ${tokens[0]}`);
      operands.push(reg);
      operands.push({ kind: "label", name: tokens[1] });
      return { op: mn, operands, line: lineNum };
    }

    case "svc": {
      const imm = parseImmediate(tokens[0]);
      if (imm === null) throw new Error(`Line ${lineNum}: Expected immediate: ${tokens[0]}`);
      operands.push({ kind: "imm", value: imm });
      return { op: mn, operands, line: lineNum };
    }

    case "nop": {
      return { op: mn, operands: [], line: lineNum };
    }

    case "adr":
    case "adrp": {
      const reg = parseRegister(tokens[0]);
      if (!reg) throw new Error(`Line ${lineNum}: Expected register: ${tokens[0]}`);
      operands.push(reg);
      operands.push({ kind: "label", name: tokens[1] });
      return { op: mn, operands, line: lineNum };
    }

    default:
      throw new Error(`Line ${lineNum}: Unknown instruction: ${mnemonic}`);
  }
}
