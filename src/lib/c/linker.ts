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
	R_AARCH64_ADR_GOT_PAGE,
	R_AARCH64_LD64_GOT_LO12_NC,
} from "./elf-parser";

// Map from instruction offset to replacement assembly line
export interface LinkedLine {
	text: string;
	isLabel?: boolean;
	originalOffset?: number; // Original byte offset in .text section
}

// Sanitize TCC symbol names (e.g. "L.1") to valid assembler labels (e.g. "L_1")
function sanitizeLabel(name: string): string {
	return name.replace(/[^a-zA-Z0-9_]/g, "_");
}

// Known C runtime functions we provide
const RUNTIME_FUNCTIONS = new Set([
	"printf", "puts", "putchar", "exit",
	"strlen", "strcmp", "memcpy", "memset",
	"snprintf", "sprintf",
	"malloc", "free", "calloc", "realloc",
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
		if (r.type === R_AARCH64_ADR_PREL_PG_HI21 || r.type === R_AARCH64_ADR_GOT_PAGE) {
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
			// Sanitize label names for our assembler (e.g. "L.1:" → "L_1:")
			const labelText = line.text.replace(/^([^:]+):/, (_, name) => `${sanitizeLabel(name)}:`);
			result.push({ text: labelText, isLabel: true, originalOffset: line.offset });
			continue;
		}

		const reloc = relocMap.get(line.offset);

		if (reloc) {
			// Handle BL relocations - replace BL target with label
			if (reloc.type === R_AARCH64_CALL26) {
				const sym = symbols[reloc.symbolIndex];
				if (sym) {
					result.push({ text: `\tBL ${sanitizeLabel(sym.name)}`, originalOffset: line.offset });
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
							// Replace ADRP+ADD pair with LDR Xd, =label
							result.push({ text: `\tLDR ${reg}, =${sanitizeLabel(sym.name)}`, originalOffset: line.offset });
							skipNext = true;
							continue;
						}
					}
				}
			}

			// Handle ADRP+LDR GOT pair → LDR Xd, =label
			if (reloc.type === R_AARCH64_ADR_GOT_PAGE) {
				const sym = symbols[reloc.symbolIndex];
				const nextLine = disasmLines[i + 1];
				if (nextLine && !nextLine.isLabel) {
					const nextReloc = relocMap.get(nextLine.offset);
					if (nextReloc && nextReloc.type === R_AARCH64_LD64_GOT_LO12_NC) {
						const adrpMatch = line.text.match(/ADRP\s+(X\d+)/i);
						if (adrpMatch && sym) {
							const reg = adrpMatch[1];
							result.push({ text: `\tLDR ${reg}, =${sanitizeLabel(sym.name)}`, originalOffset: line.offset });
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
						result.push({ text: `\tLDR ${xReg}, =${sanitizeLabel(sym.name)}`, originalOffset: line.offset });
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
		result.push({ text: line.text, isLabel: line.isLabel, originalOffset: line.offset });
	}

	return result;
}

// Resolve branch targets in assembly text to label names
export function resolveBranchTargets(
	lines: LinkedLine[],
): string {
	// Map original byte offset → label name for existing labels
	const labelByteMap = new Map<number, string>();
	// Track which labels already exist (to avoid duplicating them as synthetic)
	const existingLabels = new Set<string>();
	for (const line of lines) {
		if (line.isLabel && line.originalOffset !== undefined) {
			const name = line.text.replace(/:$/, "");
			labelByteMap.set(line.originalOffset, name);
			existingLabels.add(name);
		}
	}

	// Collect all branch target byte offsets from instruction text
	const branchTargets = new Set<number>();
	const branchPattern = /\b(?:B|BL|B\.\w+)\s+#(-?\d+)/i;
	const cbPattern = /\b(?:CBZ|CBNZ)\s+(?:X|W)\d+,\s*#(-?\d+)/i;
	for (const line of lines) {
		if (line.isLabel) continue;
		let m = branchPattern.exec(line.text);
		if (m) branchTargets.add(parseInt(m[1]));
		m = cbPattern.exec(line.text);
		if (m) branchTargets.add(parseInt(m[1]));
	}

	// Create synthetic labels for branch targets that don't have labels
	let syntheticId = 0;
	for (const off of branchTargets) {
		if (!labelByteMap.has(off)) {
			labelByteMap.set(off, `__L${syntheticId++}`);
		}
	}

	// Build output, inserting synthetic labels at the right original byte offsets
	const result: string[] = [];
	for (const line of lines) {
		if (line.isLabel) {
			result.push(line.text);
			continue;
		}

		// Insert synthetic label before this instruction if its originalOffset matches a branch target
		const origOff = line.originalOffset;
		if (origOff !== undefined) {
			const label = labelByteMap.get(origOff);
			if (label && !existingLabels.has(label)) {
				result.push(`${label}:`);
			}
		}

		let text = line.text;

		// Replace branch targets with label names
		text = text.replace(/\b(B|BL|B\.\w+)\s+#(-?\d+)/i, (match, mnemonic, offStr) => {
			const label = labelByteMap.get(parseInt(offStr));
			if (label) return `${mnemonic} ${label}`;
			return match;
		});

		text = text.replace(/\b(CBZ|CBNZ)\s+(X\d+|W\d+),\s*#(-?\d+)/i, (match, mnemonic, reg, offStr) => {
			const label = labelByteMap.get(parseInt(offStr));
			if (label) return `${mnemonic} ${reg}, ${label}`;
			return match;
		});

		result.push(text);
	}

	return result.join("\n");
}
