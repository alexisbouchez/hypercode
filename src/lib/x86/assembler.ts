import { REG_NAMES_64, REG_NAMES_32, REG_NAMES_16, REG_NAMES_8, CODE_BASE, DATA_BASE } from "./constants";

// ---------- Instruction IR ----------

export type RegSize = "r64" | "r32" | "r16" | "r8";

export type Operand =
  | { kind: "reg"; reg: number; size: RegSize }
  | { kind: "imm"; value: bigint }
  | { kind: "mem"; base: number; index: number; scale: number; disp: number; size: RegSize; baseLabel?: string }
  | { kind: "label"; name: string };

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
  equLabels: Map<string, bigint>; // equ constants
  codeBase: number;
}

// ---------- Parser Helpers ----------

function stripComments(line: string): string {
  let inStr = false;
  for (let i = 0; i < line.length; i++) {
    const ch = line[i];
    if (ch === '"' || ch === "'") inStr = !inStr;
    if (!inStr && ch === ";") return line.slice(0, i);
  }
  return line;
}

function parseRegister(token: string): Operand | null {
  const lower = token.toLowerCase();
  if (lower in REG_NAMES_64) return { kind: "reg", reg: REG_NAMES_64[lower], size: "r64" };
  if (lower in REG_NAMES_32) return { kind: "reg", reg: REG_NAMES_32[lower], size: "r32" };
  if (lower in REG_NAMES_16) return { kind: "reg", reg: REG_NAMES_16[lower], size: "r16" };
  if (lower in REG_NAMES_8) return { kind: "reg", reg: REG_NAMES_8[lower], size: "r8" };
  return null;
}

function parseImmediate(token: string): bigint | null {
  let s = token.trim();
  if (!s) return null;

  const neg = s.startsWith("-");
  if (neg) s = s.slice(1).trim();

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

// Parse memory operand: [base + index*scale + disp] in Intel syntax
// Examples: [rax], [rbp-8], [rsi+rcx*4], [rsp+16], [rel msg]
function parseMemoryOperand(token: string, labels: Set<string>, equLabels: Map<string, bigint>): Operand | null {
  const m = /^\[(.+)\]$/.exec(token.trim());
  if (!m) return null;

  let inner = m[1].trim();
  let base = -1;
  let index = -1;
  let scale = 1;
  let disp = 0;
  let baseLabel: string | undefined;

  // Handle [rel label] pattern
  if (inner.toLowerCase().startsWith("rel ")) {
    inner = inner.slice(4).trim();
  }

  // Tokenize on + and -, keeping the sign with the token
  const parts: string[] = [];
  let current = "";
  for (let i = 0; i < inner.length; i++) {
    const ch = inner[i];
    if ((ch === "+" || ch === "-") && i > 0) {
      if (current.trim()) parts.push(current.trim());
      current = ch === "-" ? "-" : "";
    } else {
      current += ch;
    }
  }
  if (current.trim()) parts.push(current.trim());

  for (const part of parts) {
    // Check for index*scale pattern
    const scaleMatch = /^(-?)(\w+)\s*\*\s*(\d+)$/.exec(part);
    if (scaleMatch) {
      const regName = scaleMatch[2].toLowerCase();
      const regNum = REG_NAMES_64[regName] ?? REG_NAMES_32[regName];
      if (regNum !== undefined) {
        index = regNum;
        scale = parseInt(scaleMatch[3]);
        if (scaleMatch[1] === "-") disp -= 0; // negative index not standard
        continue;
      }
    }

    // Check for register
    const regOp = parseRegister(part.replace(/^-/, ""));
    if (regOp && regOp.kind === "reg") {
      const isNeg = part.startsWith("-");
      if (!isNeg) {
        if (base === -1) {
          base = regOp.reg;
        } else if (index === -1) {
          index = regOp.reg;
        }
      }
      continue;
    }

    // Check for label reference
    const cleanPart = part.replace(/^[+-]/, "").trim();
    if (labels.has(cleanPart) || equLabels.has(cleanPart)) {
      baseLabel = cleanPart;
      continue;
    }

    // Check for immediate displacement
    const imm = parseImmediate(part);
    if (imm !== null) {
      disp += Number(imm);
      continue;
    }

    // Could be a label we haven't resolved yet — store as baseLabel
    if (/^[a-zA-Z_]\w*$/.test(cleanPart)) {
      baseLabel = cleanPart;
      continue;
    }

    return null; // Unrecognized part
  }

  return { kind: "mem", base, index, scale, disp, size: "r64", baseLabel };
}

// Tokenize operands, respecting brackets
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

// Determine memory operand size from size prefix (BYTE, WORD, DWORD, QWORD)
function extractSizePrefix(token: string): { size: RegSize | null; rest: string } {
  const lower = token.toLowerCase().trim();
  if (lower.startsWith("qword ")) return { size: "r64", rest: token.slice(6).trim() };
  if (lower.startsWith("dword ")) return { size: "r32", rest: token.slice(6).trim() };
  if (lower.startsWith("word ")) return { size: "r16", rest: token.slice(5).trim() };
  if (lower.startsWith("byte ")) return { size: "r8", rest: token.slice(5).trim() };
  // Also handle "QWORD PTR" etc.
  if (lower.startsWith("qword ptr ")) return { size: "r64", rest: token.slice(10).trim() };
  if (lower.startsWith("dword ptr ")) return { size: "r32", rest: token.slice(10).trim() };
  if (lower.startsWith("word ptr ")) return { size: "r16", rest: token.slice(9).trim() };
  if (lower.startsWith("byte ptr ")) return { size: "r8", rest: token.slice(9).trim() };
  return { size: null, rest: token };
}

function parseDirectiveString(raw: string): Uint8Array {
  const bytes: number[] = [];
  let remaining = raw.trim();

  while (remaining.length > 0) {
    remaining = remaining.trim();

    if (remaining.startsWith('"') || remaining.startsWith("'")) {
      const quote = remaining[0];
      let i = 1;
      while (i < remaining.length && remaining[i] !== quote) {
        if (remaining[i] === "\\" && i + 1 < remaining.length) {
          i++;
          switch (remaining[i]) {
            case "n": bytes.push(10); break;
            case "t": bytes.push(9); break;
            case "r": bytes.push(13); break;
            case "0": bytes.push(0); break;
            case "\\": bytes.push(92); break;
            case '"': bytes.push(34); break;
            case "'": bytes.push(39); break;
            default: bytes.push(remaining.charCodeAt(i)); break;
          }
        } else {
          bytes.push(remaining.charCodeAt(i));
        }
        i++;
      }
      remaining = remaining.slice(i + 1).trim();
      if (remaining.startsWith(",")) remaining = remaining.slice(1);
    } else {
      // Numeric value
      const commaIdx = remaining.indexOf(",");
      const part = commaIdx >= 0 ? remaining.slice(0, commaIdx) : remaining;
      const val = parseInt(part.trim());
      if (!isNaN(val)) bytes.push(val & 0xFF);
      remaining = commaIdx >= 0 ? remaining.slice(commaIdx + 1) : "";
    }
  }

  return new Uint8Array(bytes);
}

// ---------- Main Assembler ----------

export function assemble(source: string): AssembledProgram {
  const lines = source.split("\n");
  const instructions: Instruction[] = [];
  const labels = new Map<string, number>();
  const equLabels = new Map<string, bigint>();
  const dataLabels = new Map<string, number>();
  const dataBytes: number[] = [];
  let currentSection: "text" | "data" | "bss" = "text";

  interface ParsedLine {
    lineNum: number;
    label?: string;
    directive?: { name: string; args: string };
    instruction?: { mnemonic: string; operandsRaw: string };
    section?: "text" | "data" | "bss";
    equ?: { name: string; value: bigint };
  }

  const parsed: ParsedLine[] = [];
  const allLabels = new Set<string>();

  // Pre-pass: collect all label names for memory operand parsing
  for (let i = 0; i < lines.length; i++) {
    let line = stripComments(lines[i]).trim();
    if (!line) continue;

    const labelMatch = /^(\w+):(.*)$/.exec(line);
    if (labelMatch) {
      allLabels.add(labelMatch[1]);
    }

    // Check for equ directive
    const equMatch = /^(\w+)\s+equ\s+(.+)$/i.exec(line);
    if (equMatch) {
      allLabels.add(equMatch[1]);
    }

    // NASM-style: label followed by data directive (no colon)
    // e.g., "msg db ..." or "count dd 0"
    const nasmLabelMatch = /^(\w+)\s+(db|dw|dd|dq)\s+/i.exec(line);
    if (nasmLabelMatch) {
      allLabels.add(nasmLabelMatch[1]);
    }
  }

  // First pass: parse all lines
  for (let i = 0; i < lines.length; i++) {
    let line = stripComments(lines[i]).trim();
    if (!line) continue;

    const entry: ParsedLine = { lineNum: i + 1 };

    // Check for equ directive (NAME equ VALUE)
    const equMatch = /^(\w+)\s+equ\s+(.+)$/i.exec(line);
    if (equMatch) {
      const val = parseImmediate(equMatch[2].trim());
      if (val !== null) {
        entry.equ = { name: equMatch[1], value: val };
        equLabels.set(equMatch[1], val);
        allLabels.add(equMatch[1]);
        parsed.push(entry);
        continue;
      }
    }

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

    // Check for section directives
    const sectionMatch = /^section\s+\.(\w+)/i.exec(line);
    if (sectionMatch) {
      const sec = sectionMatch[1].toLowerCase();
      if (sec === "text") entry.section = "text";
      else if (sec === "data") entry.section = "data";
      else if (sec === "bss") entry.section = "bss";
      parsed.push(entry);
      continue;
    }

    // Check for global directive
    if (/^global\s+/i.test(line)) {
      parsed.push(entry);
      continue;
    }

    // Check for NASM-style: label followed by data directive (no colon)
    // e.g., "msg db ..." when there's no label already detected
    if (!entry.label) {
      const nasmDataMatch = /^(\w+)\s+(db|dw|dd|dq)\s+(.+)$/i.exec(line);
      if (nasmDataMatch) {
        entry.label = nasmDataMatch[1];
        entry.directive = { name: nasmDataMatch[2].toLowerCase(), args: nasmDataMatch[3] };
        parsed.push(entry);
        continue;
      }
    }

    // Check for data directives (db, dw, dd, dq)
    const dirMatch = /^(db|dw|dd|dq)\s+(.+)$/i.exec(line);
    if (dirMatch) {
      entry.directive = { name: dirMatch[1].toLowerCase(), args: dirMatch[2] };
      parsed.push(entry);
      continue;
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

  // Second pass: assign label addresses
  currentSection = "text";
  let instrIndex = 0;
  let dataOffset = 0;

  for (const p of parsed) {
    if (p.section) currentSection = p.section;

    if (p.equ) {
      // equ labels don't need position
      continue;
    }

    if (p.label) {
      if (currentSection === "text") {
        labels.set(p.label, instrIndex);
      } else {
        dataLabels.set(p.label, DATA_BASE + dataOffset);
        labels.set(p.label, DATA_BASE + dataOffset);
      }
    }

    if ((currentSection === "data" || currentSection === "bss") && p.directive) {
      switch (p.directive.name) {
        case "db": {
          const bytes = parseDirectiveString(p.directive.args);
          dataOffset += bytes.length;
          break;
        }
        case "dw": {
          const vals = p.directive.args.split(",").map((s) => s.trim());
          dataOffset += vals.length * 2;
          break;
        }
        case "dd": {
          const vals = p.directive.args.split(",").map((s) => s.trim());
          dataOffset += vals.length * 4;
          break;
        }
        case "dq": {
          const vals = p.directive.args.split(",").map((s) => s.trim());
          dataOffset += vals.length * 8;
          break;
        }
      }
    }

    if (currentSection === "text" && p.instruction) {
      instrIndex++;
    }
  }

  // Third pass: generate data segment and instructions
  currentSection = "text";

  for (const p of parsed) {
    if (p.section) currentSection = p.section;

    if ((currentSection === "data" || currentSection === "bss") && p.directive) {
      switch (p.directive.name) {
        case "db": {
          const bytes = parseDirectiveString(p.directive.args);
          for (const b of bytes) dataBytes.push(b);
          break;
        }
        case "dw": {
          const vals = p.directive.args.split(",").map((s) => s.trim());
          for (const v of vals) {
            const n = parseInt(v);
            dataBytes.push(n & 0xff, (n >> 8) & 0xff);
          }
          break;
        }
        case "dd": {
          const vals = p.directive.args.split(",").map((s) => s.trim());
          for (const v of vals) {
            const n = parseInt(v);
            dataBytes.push(n & 0xff, (n >> 8) & 0xff, (n >> 16) & 0xff, (n >> 24) & 0xff);
          }
          break;
        }
        case "dq": {
          const vals = p.directive.args.split(",").map((s) => s.trim());
          for (const v of vals) {
            const n = BigInt(v.startsWith("0x") ? v : parseInt(v));
            for (let b = 0; b < 8; b++) {
              dataBytes.push(Number((n >> BigInt(b * 8)) & 0xFFn));
            }
          }
          break;
        }
      }
    }

    if (currentSection === "text" && p.instruction) {
      const instr = parseInstruction(p.instruction.mnemonic, p.instruction.operandsRaw, p.lineNum, allLabels, equLabels);
      instructions.push(instr);
    }
  }

  return {
    instructions,
    dataSegment: new Uint8Array(dataBytes),
    dataBase: DATA_BASE,
    labels,
    equLabels,
    codeBase: CODE_BASE,
  };
}

// ---------- Instruction Parsing ----------

function parseOperand(token: string, lineNum: number, allLabels: Set<string>, equLabels: Map<string, bigint>): Operand {
  // Check for size prefix + memory
  const { size: sizePrefix, rest } = extractSizePrefix(token);

  // Check for memory operand
  if (rest.includes("[")) {
    const mem = parseMemoryOperand(rest, allLabels, equLabels);
    if (mem && mem.kind === "mem") {
      if (sizePrefix) mem.size = sizePrefix;
      return mem;
    }
  }

  // Check for register
  const reg = parseRegister(rest);
  if (reg) return reg;

  // Check for equ label (resolve to immediate)
  if (equLabels.has(rest)) {
    return { kind: "imm", value: equLabels.get(rest)! };
  }

  // Check for immediate
  const imm = parseImmediate(rest);
  if (imm !== null) return { kind: "imm", value: imm };

  // Must be a label
  if (/^[a-zA-Z_]\w*$/.test(rest)) {
    return { kind: "label", name: rest };
  }

  throw new Error(`Line ${lineNum}: Invalid operand: ${token}`);
}

function parseInstruction(mnemonic: string, operandsRaw: string, lineNum: number, allLabels: Set<string>, equLabels: Map<string, bigint>): Instruction {
  const mn = mnemonic.toLowerCase();
  const tokens = operandsRaw ? tokenizeOperands(operandsRaw) : [];
  const operands: Operand[] = [];

  // Instructions with no operands
  if (["syscall", "ret", "nop", "cqo", "cdq"].includes(mn)) {
    return { op: mn, operands: [], line: lineNum };
  }

  // Single operand instructions
  if (["push", "pop", "inc", "dec", "neg", "not", "mul", "imul", "div", "idiv",
       "jmp", "je", "jz", "jne", "jnz", "jg", "jge", "jl", "jle", "ja", "jae", "jb", "jbe",
       "call", "sete", "setne", "setg", "setge", "setl", "setle", "seta", "setb"].includes(mn) && tokens.length === 1) {
    operands.push(parseOperand(tokens[0], lineNum, allLabels, equLabels));
    return { op: mn, operands, line: lineNum };
  }

  // IMUL can have 2 or 3 operands
  if (mn === "imul" && tokens.length >= 2) {
    for (const t of tokens) {
      operands.push(parseOperand(t, lineNum, allLabels, equLabels));
    }
    return { op: mn, operands, line: lineNum };
  }

  // Two operand instructions
  if (tokens.length === 2) {
    operands.push(parseOperand(tokens[0], lineNum, allLabels, equLabels));
    operands.push(parseOperand(tokens[1], lineNum, allLabels, equLabels));
    return { op: mn, operands, line: lineNum };
  }

  // Three operand (e.g., imul rax, rbx, 5)
  if (tokens.length === 3) {
    for (const t of tokens) {
      operands.push(parseOperand(t, lineNum, allLabels, equLabels));
    }
    return { op: mn, operands, line: lineNum };
  }

  // Zero operand case already handled
  if (tokens.length === 0) {
    return { op: mn, operands: [], line: lineNum };
  }

  // Single operand fallback
  if (tokens.length === 1) {
    operands.push(parseOperand(tokens[0], lineNum, allLabels, equLabels));
    return { op: mn, operands, line: lineNum };
  }

  throw new Error(`Line ${lineNum}: Cannot parse instruction: ${mnemonic} ${operandsRaw}`);
}
