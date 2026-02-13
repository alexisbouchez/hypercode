// ARM64 machine code disassembler
// Decodes 32-bit ARM64 instruction words into our assembler's text format

import type { ElfSymbol } from "./elf-parser";

const REG64 = (n: number) => {
	if (n === 31) return "XZR";
	if (n === 29) return "FP";
	if (n === 30) return "LR";
	return `X${n}`;
};

const REG32 = (n: number) => {
	if (n === 31) return "WZR";
	return `W${n}`;
};

const REG64_SP = (n: number) => {
	if (n === 31) return "SP";
	return REG64(n);
};

const COND_NAMES = ["EQ", "NE", "CS", "CC", "MI", "PL", "VS", "VC", "HI", "LS", "GE", "LT", "GT", "LE", "AL", "NV"];

function signExtend(value: number, bits: number): number {
	const mask = 1 << (bits - 1);
	return (value ^ mask) - mask;
}

function bits(instr: number, hi: number, lo: number): number {
	return (instr >>> lo) & ((1 << (hi - lo + 1)) - 1);
}

function bit(instr: number, pos: number): number {
	return (instr >>> pos) & 1;
}

interface DisasmLine {
	offset: number;
	text: string;
	isLabel?: boolean;
}

export function disassemble(
	textBytes: Uint8Array,
	symbols: ElfSymbol[],
	textSectionIndex: number,
): DisasmLine[] {
	const lines: DisasmLine[] = [];
	const view = new DataView(textBytes.buffer, textBytes.byteOffset, textBytes.byteLength);

	// Build label map from symbols in .text
	const labelMap = new Map<number, string>();
	for (const sym of symbols) {
		if (sym.sectionIndex === textSectionIndex && sym.name && sym.type === 2) {
			// STT_FUNC
			labelMap.set(sym.value, sym.name);
		}
	}

	for (let off = 0; off < textBytes.length; off += 4) {
		// Add label if one exists at this offset
		const label = labelMap.get(off);
		if (label) {
			lines.push({ offset: off, text: `${label}:`, isLabel: true });
		}

		const instr = view.getUint32(off, true); // little-endian
		const text = decodeInstruction(instr, off);
		lines.push({ offset: off, text: `\t${text}` });
	}

	return lines;
}

function decodeInstruction(instr: number, pc: number): string {
	// NOP
	if ((instr >>> 0) === 0xd503201f) return "NOP";

	// RET
	if (((instr & 0xfffffc1f) >>> 0) === 0xd65f0000) {
		const rn = bits(instr, 9, 5);
		if (rn === 30) return "RET";
		return `RET ${REG64(rn)}`;
	}

	// BR Xn
	if (((instr & 0xfffffc1f) >>> 0) === 0xd61f0000) {
		const rn = bits(instr, 9, 5);
		return `BR ${REG64(rn)}`;
	}

	// BL
	if ((instr >>> 26) === 0x25) {
		const imm26 = signExtend(bits(instr, 25, 0), 26);
		const target = pc + imm26 * 4;
		return `BL #${target}`;
	}

	// B
	if ((instr >>> 26) === 0x05) {
		const imm26 = signExtend(bits(instr, 25, 0), 26);
		const target = pc + imm26 * 4;
		return `B #${target}`;
	}

	// B.cond
	if (((instr & 0xff000010) >>> 0) === 0x54000000) {
		const cond = bits(instr, 3, 0);
		const imm19 = signExtend(bits(instr, 23, 5), 19);
		const target = pc + imm19 * 4;
		return `B.${COND_NAMES[cond]} #${target}`;
	}

	// CBZ / CBNZ
	if ((instr & 0x7e000000) === 0x34000000) {
		const sf = bit(instr, 31);
		const op = bit(instr, 24); // 0=CBZ, 1=CBNZ
		const imm19 = signExtend(bits(instr, 23, 5), 19);
		const rt = bits(instr, 4, 0);
		const target = pc + imm19 * 4;
		const mnemonic = op ? "CBNZ" : "CBZ";
		const reg = sf ? REG64(rt) : REG32(rt);
		return `${mnemonic} ${reg}, #${target}`;
	}

	// MOVZ / MOVN / MOVK
	if ((instr & 0x1f800000) === 0x12800000 || (instr & 0x1f800000) === 0x12a00000 ||
		(instr & 0x1f800000) === 0x12c00000 || (instr & 0x1f800000) === 0x12e00000) {
		const sf = bit(instr, 31);
		const opc = bits(instr, 30, 29);
		const hw = bits(instr, 22, 21);
		const imm16 = bits(instr, 20, 5);
		const rd = bits(instr, 4, 0);

		let mnemonic: string;
		if (opc === 0) mnemonic = "MOVN";
		else if (opc === 2) mnemonic = "MOVZ";
		else if (opc === 3) mnemonic = "MOVK";
		else return `unknown_mov 0x${instr.toString(16)}`;

		const reg = sf ? REG64(rd) : REG32(rd);
		const shift = hw * 16;

		if (mnemonic === "MOVN" && hw === 0) {
			// MOVN with shift 0 is often used for small negative numbers
			// MOV Xd, #~imm16
			const value = sf ? ~imm16 : (~imm16) & 0xFFFFFFFF;
			return `MOV ${reg}, #${value}`;
		}

		if (mnemonic === "MOVZ" && shift === 0) {
			return `MOV ${reg}, #${imm16}`;
		}

		if (shift === 0) {
			return `${mnemonic} ${reg}, #${imm16}`;
		}
		return `${mnemonic} ${reg}, #${imm16}, LSL #${shift}`;
	}

	// ADD/SUB (immediate)
	if ((instr & 0x1f000000) === 0x11000000) {
		const sf = bit(instr, 31);
		const op = bit(instr, 30); // 0=ADD, 1=SUB
		const S = bit(instr, 29);
		const sh = bit(instr, 22); // shift (0 or 12)
		const imm12 = bits(instr, 21, 10);
		const rn = bits(instr, 9, 5);
		const rd = bits(instr, 4, 0);

		let mnemonic = op ? "SUB" : "ADD";
		if (S) mnemonic += "S";

		// CMP is SUBS with Rd=XZR
		if (S && op && rd === 31) {
			const regN = sf ? REG64_SP(rn) : REG32(rn);
			const imm = sh ? imm12 << 12 : imm12;
			return `CMP ${regN}, #${imm}`;
		}

		// CMN is ADDS with Rd=XZR
		if (S && !op && rd === 31) {
			const regN = sf ? REG64_SP(rn) : REG32(rn);
			const imm = sh ? imm12 << 12 : imm12;
			return `CMN ${regN}, #${imm}`;
		}

		// MOV (from SP) is ADD Rd, SP, #0
		if (!op && !S && imm12 === 0 && (rn === 31 || rd === 31)) {
			const regD = sf ? REG64_SP(rd) : REG32(rd);
			const regN = sf ? REG64_SP(rn) : REG32(rn);
			return `MOV ${regD}, ${regN}`;
		}

		const regD = sf ? REG64_SP(rd) : REG32(rd);
		const regN = sf ? REG64_SP(rn) : REG32(rn);
		const imm = sh ? imm12 << 12 : imm12;
		return `${mnemonic} ${regD}, ${regN}, #${imm}`;
	}

	// ADD/SUB (shifted register)
	if ((instr & 0x1f200000) === 0x0b000000) {
		const sf = bit(instr, 31);
		const op = bit(instr, 30);
		const S = bit(instr, 29);
		const shift = bits(instr, 23, 22);
		const rm = bits(instr, 20, 16);
		const imm6 = bits(instr, 15, 10);
		const rn = bits(instr, 9, 5);
		const rd = bits(instr, 4, 0);

		let mnemonic = op ? "SUB" : "ADD";
		if (S) mnemonic += "S";

		// CMP
		if (S && op && rd === 31) {
			const regN = sf ? REG64(rn) : REG32(rn);
			const regM = sf ? REG64(rm) : REG32(rm);
			if (imm6 === 0) return `CMP ${regN}, ${regM}`;
			const shiftName = ["LSL", "LSR", "ASR"][shift] || "LSL";
			return `CMP ${regN}, ${regM}, ${shiftName} #${imm6}`;
		}

		// NEG is SUB Rd, XZR, Rm
		if (op && !S && rn === 31) {
			const regD = sf ? REG64(rd) : REG32(rd);
			const regM = sf ? REG64(rm) : REG32(rm);
			return `NEG ${regD}, ${regM}`;
		}

		const regD = sf ? REG64(rd) : REG32(rd);
		const regN = sf ? REG64(rn) : REG32(rn);
		const regM = sf ? REG64(rm) : REG32(rm);
		if (imm6 === 0) return `${mnemonic} ${regD}, ${regN}, ${regM}`;
		const shiftName = ["LSL", "LSR", "ASR"][shift] || "LSL";
		return `${mnemonic} ${regD}, ${regN}, ${regM}, ${shiftName} #${imm6}`;
	}

	// Logical (shifted register): AND, ORR, EOR, ANDS
	if ((instr & 0x1f000000) === 0x0a000000) {
		const sf = bit(instr, 31);
		const opc = bits(instr, 30, 29);
		const N = bit(instr, 21);
		const shift = bits(instr, 23, 22);
		const rm = bits(instr, 20, 16);
		const imm6 = bits(instr, 15, 10);
		const rn = bits(instr, 9, 5);
		const rd = bits(instr, 4, 0);

		const mnemonics = ["AND", "ORR", "EOR", "ANDS"];
		let mnemonic = mnemonics[opc];

		// MVN is ORR Rd, XZR, Rm, ... with N=1
		if (opc === 1 && N && rn === 31 && imm6 === 0 && shift === 0) {
			const regD = sf ? REG64(rd) : REG32(rd);
			const regM = sf ? REG64(rm) : REG32(rm);
			return `MVN ${regD}, ${regM}`;
		}

		// MOV is ORR Rd, XZR, Rm
		if (opc === 1 && !N && rn === 31 && imm6 === 0 && shift === 0) {
			const regD = sf ? REG64(rd) : REG32(rd);
			const regM = sf ? REG64(rm) : REG32(rm);
			return `MOV ${regD}, ${regM}`;
		}

		// TST is ANDS XZR, Rn, Rm
		if (opc === 3 && rd === 31) {
			mnemonic = "TST";
			const regN = sf ? REG64(rn) : REG32(rn);
			const regM = sf ? REG64(rm) : REG32(rm);
			if (imm6 === 0 && shift === 0) return `TST ${regN}, ${regM}`;
			const shiftName = ["LSL", "LSR", "ASR", "ROR"][shift];
			return `TST ${regN}, ${regM}, ${shiftName} #${imm6}`;
		}

		const regD = sf ? REG64(rd) : REG32(rd);
		const regN = sf ? REG64(rn) : REG32(rn);
		const regM = sf ? REG64(rm) : REG32(rm);
		if (imm6 === 0 && shift === 0) return `${mnemonic} ${regD}, ${regN}, ${regM}`;
		const shiftName = ["LSL", "LSR", "ASR", "ROR"][shift];
		return `${mnemonic} ${regD}, ${regN}, ${regM}, ${shiftName} #${imm6}`;
	}

	// Logical (immediate)
	if ((instr & 0x1f800000) === 0x12000000 || (instr & 0x1f800000) === 0x12400000 ||
		(instr & 0x1f800000) === 0x32000000 || (instr & 0x1f800000) === 0x32400000 ||
		(instr & 0x1f800000) === 0x52000000 || (instr & 0x1f800000) === 0x52400000 ||
		(instr & 0x1f800000) === 0x72000000 || (instr & 0x1f800000) === 0x72400000) {
		const sf = bit(instr, 31);
		const opc = bits(instr, 30, 29);
		const rd = bits(instr, 4, 0);
		const rn = bits(instr, 9, 5);

		const immr = bits(instr, 21, 16);
		const imms = bits(instr, 15, 10);
		const N = bit(instr, 22);

		const imm = decodeBitmaskImm(N, imms, immr, sf ? 64 : 32);
		if (imm !== null) {
			const mnemonics = ["AND", "ORR", "EOR", "ANDS"];
			let mnemonic = mnemonics[opc];

			// TST is ANDS XZR, ...
			if (opc === 3 && rd === 31) {
				const regN = sf ? REG64(rn) : REG32(rn);
				return `TST ${regN}, #0x${imm.toString(16)}`;
			}

			const regD = (opc === 3) ? (sf ? REG64(rd) : REG32(rd)) : (sf ? REG64_SP(rd) : REG32(rd));
			const regN = sf ? REG64(rn) : REG32(rn);
			return `${mnemonic} ${regD}, ${regN}, #0x${imm.toString(16)}`;
		}
	}

	// MUL / MADD / MSUB (Data Processing 3 source)
	if ((instr & 0x1f000000) === 0x1b000000) {
		const sf = bit(instr, 31);
		const op54 = bits(instr, 30, 29);
		const op31 = bits(instr, 23, 21);
		const rm = bits(instr, 20, 16);
		const o0 = bit(instr, 15);
		const ra = bits(instr, 14, 10);
		const rn = bits(instr, 9, 5);
		const rd = bits(instr, 4, 0);

		if (op54 === 0 && op31 === 0) {
			const regD = sf ? REG64(rd) : REG32(rd);
			const regN = sf ? REG64(rn) : REG32(rn);
			const regM = sf ? REG64(rm) : REG32(rm);
			const regA = sf ? REG64(ra) : REG32(ra);

			if (!o0 && ra === 31) return `MUL ${regD}, ${regN}, ${regM}`;
			if (o0 && ra === 31) {
				// MNEG
				return `MSUB ${regD}, ${regN}, ${regM}, ${regA}`;
			}
			const mnemonic = o0 ? "MSUB" : "MADD";
			return `${mnemonic} ${regD}, ${regN}, ${regM}, ${regA}`;
		}

		// SDIV / UDIV
		if (op54 === 0 && (op31 === 2 || op31 === 3)) {
			const regD = sf ? REG64(rd) : REG32(rd);
			const regN = sf ? REG64(rn) : REG32(rn);
			const regM = sf ? REG64(rm) : REG32(rm);
			const mnemonic = op31 === 2 ? "UDIV" : "SDIV";
			return `${mnemonic} ${regD}, ${regN}, ${regM}`;
		}
	}

	// SDIV / UDIV (Data Processing 2 source)
	if ((instr & 0x5fe00000) === 0x1ac00000) {
		const sf = bit(instr, 31);
		const rm = bits(instr, 20, 16);
		const opcode2 = bits(instr, 15, 10);
		const rn = bits(instr, 9, 5);
		const rd = bits(instr, 4, 0);

		if (opcode2 === 2 || opcode2 === 3) {
			const mnemonic = opcode2 === 2 ? "UDIV" : "SDIV";
			const regD = sf ? REG64(rd) : REG32(rd);
			const regN = sf ? REG64(rn) : REG32(rn);
			const regM = sf ? REG64(rm) : REG32(rm);
			return `${mnemonic} ${regD}, ${regN}, ${regM}`;
		}

		// LSL/LSR/ASR (register)
		if (opcode2 >= 8 && opcode2 <= 10) {
			const mnemonics = ["", "", "", "", "", "", "", "", "LSL", "LSR", "ASR"];
			const mnemonic = mnemonics[opcode2];
			const regD = sf ? REG64(rd) : REG32(rd);
			const regN = sf ? REG64(rn) : REG32(rn);
			const regM = sf ? REG64(rm) : REG32(rm);
			return `${mnemonic} ${regD}, ${regN}, ${regM}`;
		}
	}

	// CSEL / CSINC / CSET / CSINV / CSNEG
	if ((instr & 0x1fe00000) === 0x1a800000) {
		const sf = bit(instr, 31);
		const op = bit(instr, 30);
		const S = bit(instr, 29);
		const rm = bits(instr, 20, 16);
		const cond = bits(instr, 15, 12);
		const op2 = bit(instr, 10);
		const rn = bits(instr, 9, 5);
		const rd = bits(instr, 4, 0);

		if (S === 0) {
			const regD = sf ? REG64(rd) : REG32(rd);
			const regN = sf ? REG64(rn) : REG32(rn);
			const regM = sf ? REG64(rm) : REG32(rm);

			if (op === 0 && op2 === 0) {
				// CSEL
				return `CSEL ${regD}, ${regN}, ${regM}, ${COND_NAMES[cond]}`;
			}
			if (op === 0 && op2 === 1) {
				// CSINC - CSET is CSINC Rd, XZR, XZR, invert(cond)
				if (rn === 31 && rm === 31) {
					const invertedCond = cond ^ 1;
					return `CSET ${regD}, ${COND_NAMES[invertedCond]}`;
				}
				return `CSINC ${regD}, ${regN}, ${regM}, ${COND_NAMES[cond]}`;
			}
			if (op === 1 && op2 === 0) {
				// CSINV - CSETM is CSINV Rd, XZR, XZR, invert(cond)
				if (rn === 31 && rm === 31) {
					const invertedCond = cond ^ 1;
					return `CSETM ${regD}, ${COND_NAMES[invertedCond]}`;
				}
				return `CSINV ${regD}, ${regN}, ${regM}, ${COND_NAMES[cond]}`;
			}
			if (op === 1 && op2 === 1) {
				// CSNEG
				return `CSNEG ${regD}, ${regN}, ${regM}, ${COND_NAMES[cond]}`;
			}
		}
	}

	// Shift (immediate) - encoded as UBFM/SBFM
	if ((instr & 0x7f800000) === 0x53000000 || ((instr & 0x7f800000) >>> 0) === 0xd3400000) {
		const sf = bit(instr, 31);
		const opc = bits(instr, 30, 29);
		const N = bit(instr, 22);
		const immr = bits(instr, 21, 16);
		const imms = bits(instr, 15, 10);
		const rn = bits(instr, 9, 5);
		const rd = bits(instr, 4, 0);

		// UBFM: LSR / LSL / UBFX / UXTB / UXTH
		if (opc === 2) {
			const regD = sf ? REG64(rd) : REG32(rd);
			const regN = sf ? REG64(rn) : REG32(rn);
			const regSize = sf ? 63 : 31;

			if (imms === regSize) {
				// LSR
				return `LSR ${regD}, ${regN}, #${immr}`;
			}
			if (imms + 1 === immr) {
				// LSL
				const shift = regSize - imms;
				return `LSL ${regD}, ${regN}, #${shift}`;
			}
			if (imms === 7 && immr === 0) {
				return `UXTB ${regD}, ${REG32(rn)}`;
			}
			if (imms === 15 && immr === 0) {
				return `UXTH ${regD}, ${REG32(rn)}`;
			}
		}

		// SBFM: ASR / SXTW / SXTH / SXTB
		if (opc === 0) {
			const regD = sf ? REG64(rd) : REG32(rd);
			const regN = sf ? REG64(rn) : REG32(rn);
			const regSize = sf ? 63 : 31;

			if (imms === regSize) {
				return `ASR ${regD}, ${regN}, #${immr}`;
			}
			// SXTW: SBFM Xd, Xn, #0, #31 (sf=1, N=1, immr=0, imms=31)
			if (sf && N && immr === 0 && imms === 31) {
				return `SXTW ${REG64(rd)}, ${REG32(rn)}`;
			}
			if (sf && N && immr === 0 && imms === 15) {
				return `SXTH ${REG64(rd)}, ${REG32(rn)}`;
			}
			if (sf && N && immr === 0 && imms === 7) {
				return `SXTB ${REG64(rd)}, ${REG32(rn)}`;
			}
		}
	}

	// LDR/STR (immediate, unsigned offset)
	if ((instr & 0x3b000000) === 0x39000000) {
		const size = bits(instr, 31, 30);
		const V = bit(instr, 26);
		const opc = bits(instr, 23, 22);
		const imm12 = bits(instr, 21, 10);
		const rn = bits(instr, 9, 5);
		const rt = bits(instr, 4, 0);

		if (V === 0) {
			const scale = size;
			const offset = imm12 << scale;
			const regN = REG64_SP(rn);

			if (size === 3 && opc === 1) {
				// LDR (64-bit)
				if (offset === 0) return `LDR ${REG64(rt)}, [${regN}]`;
				return `LDR ${REG64(rt)}, [${regN}, #${offset}]`;
			}
			if (size === 3 && opc === 0) {
				// STR (64-bit)
				if (offset === 0) return `STR ${REG64(rt)}, [${regN}]`;
				return `STR ${REG64(rt)}, [${regN}, #${offset}]`;
			}
			if (size === 2 && opc === 1) {
				// LDR (32-bit)
				if (offset === 0) return `LDR ${REG32(rt)}, [${regN}]`;
				return `LDR ${REG32(rt)}, [${regN}, #${offset}]`;
			}
			if (size === 2 && opc === 0) {
				// STR (32-bit)
				if (offset === 0) return `STR ${REG32(rt)}, [${regN}]`;
				return `STR ${REG32(rt)}, [${regN}, #${offset}]`;
			}
			if (size === 1 && opc === 1) {
				// LDRH
				if (offset === 0) return `LDRH ${REG32(rt)}, [${regN}]`;
				return `LDRH ${REG32(rt)}, [${regN}, #${offset}]`;
			}
			if (size === 1 && opc === 0) {
				// STRH
				if (offset === 0) return `STRH ${REG32(rt)}, [${regN}]`;
				return `STRH ${REG32(rt)}, [${regN}, #${offset}]`;
			}
			if (size === 0 && opc === 1) {
				// LDRB
				if (offset === 0) return `LDRB ${REG32(rt)}, [${regN}]`;
				return `LDRB ${REG32(rt)}, [${regN}, #${offset}]`;
			}
			if (size === 0 && opc === 0) {
				// STRB
				if (offset === 0) return `STRB ${REG32(rt)}, [${regN}]`;
				return `STRB ${REG32(rt)}, [${regN}, #${offset}]`;
			}
			if (size === 2 && opc === 2) {
				// LDRSW
				if (offset === 0) return `LDRSW ${REG64(rt)}, [${regN}]`;
				return `LDRSW ${REG64(rt)}, [${regN}, #${offset}]`;
			}
		}
	}

	// LDR/STR (pre/post-index, immediate)
	if ((instr & 0x3b200c00) === 0x38000000) {
		const size = bits(instr, 31, 30);
		const V = bit(instr, 26);
		const opc = bits(instr, 23, 22);
		const imm9 = signExtend(bits(instr, 20, 12), 9);
		const type = bits(instr, 11, 10); // 01=post, 11=pre
		const rn = bits(instr, 9, 5);
		const rt = bits(instr, 4, 0);

		if (V === 0 && (type === 0 || type === 1 || type === 3)) {
			const regN = REG64_SP(rn);

			const getMnemonic = () => {
				if (size === 3 && opc === 1) return { mn: "LDR", reg: REG64(rt) };
				if (size === 3 && opc === 0) return { mn: "STR", reg: REG64(rt) };
				if (size === 2 && opc === 1) return { mn: "LDR", reg: REG32(rt) };
				if (size === 2 && opc === 0) return { mn: "STR", reg: REG32(rt) };
				if (size === 1 && opc === 1) return { mn: "LDRH", reg: REG32(rt) };
				if (size === 1 && opc === 0) return { mn: "STRH", reg: REG32(rt) };
				if (size === 0 && opc === 1) return { mn: "LDRB", reg: REG32(rt) };
				if (size === 0 && opc === 0) return { mn: "STRB", reg: REG32(rt) };
				if (size === 2 && opc === 2) return { mn: "LDRSW", reg: REG64(rt) };
				return null;
			};

			const r = getMnemonic();
			if (r) {
				if (type === 3) return `${r.mn} ${r.reg}, [${regN}, #${imm9}]!`;
				if (type === 1) return `${r.mn} ${r.reg}, [${regN}], #${imm9}`;
				// type === 0: unscaled immediate (same syntax as offset)
				if (imm9 === 0) return `${r.mn} ${r.reg}, [${regN}]`;
				return `${r.mn} ${r.reg}, [${regN}, #${imm9}]`;
			}
		}
	}

	// LDR/STR (register offset)
	if ((instr & 0x3b200c00) === 0x38200800) {
		const size = bits(instr, 31, 30);
		const V = bit(instr, 26);
		const opc = bits(instr, 23, 22);
		const rm = bits(instr, 20, 16);
		const rn = bits(instr, 9, 5);
		const rt = bits(instr, 4, 0);

		if (V === 0) {
			const regN = REG64_SP(rn);
			const regM = REG64(rm);

			if (size === 3 && opc === 1) return `LDR ${REG64(rt)}, [${regN}, ${regM}]`;
			if (size === 3 && opc === 0) return `STR ${REG64(rt)}, [${regN}, ${regM}]`;
			if (size === 2 && opc === 1) return `LDR ${REG32(rt)}, [${regN}, ${regM}]`;
			if (size === 2 && opc === 0) return `STR ${REG32(rt)}, [${regN}, ${regM}]`;
			if (size === 0 && opc === 1) return `LDRB ${REG32(rt)}, [${regN}, ${regM}]`;
			if (size === 0 && opc === 0) return `STRB ${REG32(rt)}, [${regN}, ${regM}]`;
		}
	}

	// LDP/STP (offset and pre/post-index)
	if (((instr & 0x3e000000) >>> 0) === 0x28000000 || ((instr & 0x3e000000) >>> 0) === 0x2c000000) {
		const opc = bits(instr, 31, 30);
		const V = bit(instr, 26);
		const type = bits(instr, 24, 23); // 01=post, 10=offset, 11=pre
		const L = bit(instr, 22);
		const imm7 = signExtend(bits(instr, 21, 15), 7);
		const rt2 = bits(instr, 14, 10);
		const rn = bits(instr, 9, 5);
		const rt1 = bits(instr, 4, 0);

		if (V === 0 && (type === 1 || type === 2 || type === 3)) {
			const sf = opc === 2; // 10 = 64-bit
			const scale = sf ? 8 : 4;
			const offset = imm7 * scale;
			const mnemonic = L ? "LDP" : "STP";
			const regT1 = sf ? REG64(rt1) : REG32(rt1);
			const regT2 = sf ? REG64(rt2) : REG32(rt2);
			const regN = REG64_SP(rn);

			if (type === 2) {
				if (offset === 0) return `${mnemonic} ${regT1}, ${regT2}, [${regN}]`;
				return `${mnemonic} ${regT1}, ${regT2}, [${regN}, #${offset}]`;
			}
			if (type === 3) return `${mnemonic} ${regT1}, ${regT2}, [${regN}, #${offset}]!`;
			if (type === 1) return `${mnemonic} ${regT1}, ${regT2}, [${regN}], #${offset}`;
		}
	}

	// ADRP
	if (((instr & 0x9f000000) >>> 0) === 0x90000000) {
		const rd = bits(instr, 4, 0);
		const immlo = bits(instr, 30, 29);
		const immhi = signExtend(bits(instr, 23, 5), 19);
		const imm = ((immhi << 2) | immlo) << 12;
		return `ADRP ${REG64(rd)}, #${imm + (pc & ~0xfff)}`;
	}

	// ADR
	if (((instr & 0x9f000000) >>> 0) === 0x10000000) {
		const rd = bits(instr, 4, 0);
		const immlo = bits(instr, 30, 29);
		const immhi = signExtend(bits(instr, 23, 5), 19);
		const imm = (immhi << 2) | immlo;
		return `ADR ${REG64(rd)}, #${imm + pc}`;
	}

	// SVC
	if (((instr & 0xffe0001f) >>> 0) === 0xd4000001) {
		const imm16 = bits(instr, 20, 5);
		return `SVC #${imm16}`;
	}

	return `.word 0x${instr.toString(16).padStart(8, '0')}`;
}

// Decode ARM64 bitmask immediate
function decodeBitmaskImm(N: number, imms: number, immr: number, regSize: number): number | null {
	const len = highestSetBit((N << 6) | (~imms & 0x3f));
	if (len < 1) return null;

	const size = 1 << len;
	const levels = size - 1;
	const S = imms & levels;
	const R = immr & levels;

	const welem = ((1 << (S + 1)) - 1) >>> 0;
	const rotated = ((welem >>> R) | (welem << (size - R))) & ((1 << size) - 1);

	let result = 0;
	for (let i = 0; i < regSize; i += size) {
		result = (result | (rotated << i)) >>> 0;
	}

	return result >>> 0;
}

function highestSetBit(value: number): number {
	for (let i = 6; i >= 0; i--) {
		if (value & (1 << i)) return i;
	}
	return -1;
}
