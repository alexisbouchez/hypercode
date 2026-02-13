// Linker: resolves relocations in disassembled ARM64 code
// Converts TCC's relocation-based code references to labels our assembler understands

import type { ElfSymbol, ElfRelocation, ElfParseResult } from "./elf-parser";
import {
	R_AARCH64_CALL26,
	R_AARCH64_ADR_PREL_PG_HI21,
	R_AARCH64_ADD_ABS_LO12_NC,
	R_AARCH64_LDST64_ABS_LO12_NC,
	R_AARCH64_LDST32_ABS_LO12_NC,
	R_AARCH64_LDST8_ABS_LO12_NC,
	R_AARCH64_MOVW_UABS_G0_NC,
	R_AARCH64_MOVW_UABS_G1_NC,
	R_AARCH64_MOVW_UABS_G2_NC,
	R_AARCH64_MOVW_UABS_G3,
} from "./elf-parser";

// Map from instruction offset to replacement assembly line
export interface LinkedLine {
	text: string;
	isLabel?: boolean;
}

// Known C runtime functions we provide
const RUNTIME_FUNCTIONS = new Set([
	"printf", "puts", "putchar", "exit",
	"strlen", "strcmp", "memcpy", "memset",
	"snprintf", "sprintf",
]);

export function link(
	disasmLines: { offset: number; text: string; isLabel?: boolean }[],
	elf: ElfParseResult,
	dataLabels: Map<string, number>,
): LinkedLine[] {
	const { symbols, relocations, textSection, dataSection, rodataSection } = elf;

	// Build relocation map: instruction offset -> relocation
	const relocMap = new Map<number, ElfRelocation>();
	for (const r of relocations) {
		relocMap.set(r.offset, r);
	}

	// Track ADRP pairs for combining
	const adrpTargets = new Map<number, { symbolName: string; offset: number }>();

	// First pass: identify ADRP relocations and their targets
	for (const r of relocations) {
		if (r.type === R_AARCH64_ADR_PREL_PG_HI21) {
			const sym = symbols[r.symbolIndex];
			if (sym) {
				adrpTargets.set(r.offset, { symbolName: sym.name, offset: r.offset });
			}
		}
	}

	// Build branch target map: offset -> label name (for resolving BL targets)
	const branchLabels = new Map<number, string>();
	for (const r of relocations) {
		if (r.type === R_AARCH64_CALL26) {
			const sym = symbols[r.symbolIndex];
			if (sym) {
				branchLabels.set(r.offset, sym.name);
			}
		}
	}

	// Collect all data/rodata symbols for label generation
	const dataSymNames = new Map<number, string>();
	for (const sym of symbols) {
		if (sym.name && dataSection) {
			const textIdx = elf.sections.indexOf(textSection!);
			const dataIdx = elf.sections.indexOf(dataSection);
			const rodataIdx = rodataSection ? elf.sections.indexOf(rodataSection) : -1;
			if (sym.sectionIndex === dataIdx || sym.sectionIndex === rodataIdx) {
				dataSymNames.set(sym.value, sym.name);
			}
		}
	}

	const result: LinkedLine[] = [];
	let skipNext = false;

	for (let i = 0; i < disasmLines.length; i++) {
		if (skipNext) {
			skipNext = false;
			continue;
		}

		const line = disasmLines[i];

		if (line.isLabel) {
			result.push({ text: line.text, isLabel: true });
			continue;
		}

		const reloc = relocMap.get(line.offset);

		if (reloc) {
			// Handle BL relocations - replace BL target with label
			if (reloc.type === R_AARCH64_CALL26) {
				const sym = symbols[reloc.symbolIndex];
				if (sym) {
					result.push({ text: `\tBL ${sym.name}` });
					continue;
				}
			}

			// Handle ADRP+ADD pair → LDR Xd, =label
			if (reloc.type === R_AARCH64_ADR_PREL_PG_HI21) {
				const sym = symbols[reloc.symbolIndex];
				// Check if next non-label line is ADD with matching relocation
				const nextLine = disasmLines[i + 1];
				if (nextLine && !nextLine.isLabel) {
					const nextReloc = relocMap.get(nextLine.offset);
					if (nextReloc && (
						nextReloc.type === R_AARCH64_ADD_ABS_LO12_NC ||
						nextReloc.type === R_AARCH64_LDST64_ABS_LO12_NC ||
						nextReloc.type === R_AARCH64_LDST32_ABS_LO12_NC ||
						nextReloc.type === R_AARCH64_LDST8_ABS_LO12_NC
					)) {
						// Extract destination register from ADRP line
						const adrpMatch = line.text.match(/ADRP\s+(X\d+)/i);
						if (adrpMatch && sym) {
							const reg = adrpMatch[1];
							const symName = sym.name;
							// Replace ADRP+ADD pair with LDR Xd, =label
							result.push({ text: `\tLDR ${reg}, =${symName}` });
							skipNext = true;
							continue;
						}
					}
				}
			}

			// Handle MOVW relocations for address loading
			if (reloc.type === R_AARCH64_MOVW_UABS_G0_NC ||
				reloc.type === R_AARCH64_MOVW_UABS_G1_NC ||
				reloc.type === R_AARCH64_MOVW_UABS_G2_NC ||
				reloc.type === R_AARCH64_MOVW_UABS_G3) {
				const sym = symbols[reloc.symbolIndex];
				if (sym && reloc.type === R_AARCH64_MOVW_UABS_G0_NC) {
					// First of a MOVZ/MOVK sequence - try to collapse to LDR =label
					const movMatch = line.text.match(/MOV[ZK]?\s+(X\d+|W\d+)/i) || line.text.match(/MOV\s+(X\d+|W\d+)/i);
					if (movMatch) {
						const reg = movMatch[1].toUpperCase();
						// Skip subsequent MOVK instructions for this register
						let j = i + 1;
						while (j < disasmLines.length && !disasmLines[j].isLabel) {
							const nr = relocMap.get(disasmLines[j].offset);
							if (nr && (nr.type === R_AARCH64_MOVW_UABS_G1_NC ||
								nr.type === R_AARCH64_MOVW_UABS_G2_NC ||
								nr.type === R_AARCH64_MOVW_UABS_G3)) {
								j++;
							} else {
								break;
							}
						}
						const xReg = reg.startsWith("W") ? `X${reg.slice(1)}` : reg;
						result.push({ text: `\tLDR ${xReg}, =${sym.name}` });
						// Skip the MOVK instructions we consumed
						const toSkip = j - i - 1;
						for (let k = 0; k < toSkip; k++) {
							i++;
						}
						continue;
					}
				}
			}
		}

		// Pass through as-is
		result.push({ text: line.text, isLabel: line.isLabel });
	}

	return result;
}

// Resolve branch targets in assembly text to label names
export function resolveBranchTargets(
	lines: LinkedLine[],
): string {
	// Build offset→label map from label lines
	const labelAtOffset = new Map<number, string>();
	let instrOffset = 0;

	// First pass: map labels to instruction indices
	const instrLines: { text: string; instrIndex: number }[] = [];
	let instrIdx = 0;
	for (const line of lines) {
		if (line.isLabel) {
			labelAtOffset.set(instrIdx, line.text.replace(/:$/, ""));
		} else {
			instrLines.push({ text: line.text, instrIndex: instrIdx });
			instrIdx++;
		}
	}

	// Build byte offset to label map
	const byteToLabel = new Map<number, string>();
	instrIdx = 0;
	for (const line of lines) {
		if (line.isLabel) {
			byteToLabel.set(instrIdx * 4, line.text.replace(/:$/, ""));
		} else {
			instrIdx++;
		}
	}

	// Build instruction-index-to-byte-offset reverse mapping
	// Actually, we need to figure out mapping from disasm byte offsets to labels
	// The disassembler uses byte offsets in #target notation

	// Reconstruct: for each instruction line, figure out what its byte offset was
	const result: string[] = [];
	let currentInstrByteOff = 0;
	const instrByteOffsets: number[] = [];

	for (const line of lines) {
		if (line.isLabel) {
			// label doesn't take bytes
			continue;
		}
		instrByteOffsets.push(currentInstrByteOff);
		currentInstrByteOff += 4;
	}

	// Rebuild: label byte offsets
	const labelByteMap = new Map<number, string>();
	currentInstrByteOff = 0;
	for (const line of lines) {
		if (line.isLabel) {
			labelByteMap.set(currentInstrByteOff, line.text.replace(/:$/, ""));
		} else {
			currentInstrByteOff += 4;
		}
	}

	// Output
	for (const line of lines) {
		if (line.isLabel) {
			result.push(line.text);
			continue;
		}

		let text = line.text;

		// Replace #<byte_offset> branch targets with labels
		// B #offset, BL #offset, B.cond #offset, CBZ/CBNZ reg, #offset
		text = text.replace(/\b(B|BL|B\.\w+)\s+#(-?\d+)/i, (match, mnemonic, offStr) => {
			const targetOff = parseInt(offStr);
			const label = labelByteMap.get(targetOff);
			if (label) return `${mnemonic} ${label}`;
			return match;
		});

		text = text.replace(/\b(CBZ|CBNZ)\s+(X\d+|W\d+),\s*#(-?\d+)/i, (match, mnemonic, reg, offStr) => {
			const targetOff = parseInt(offStr);
			const label = labelByteMap.get(targetOff);
			if (label) return `${mnemonic} ${reg}, ${label}`;
			return match;
		});

		result.push(text);
	}

	return result.join("\n");
}
