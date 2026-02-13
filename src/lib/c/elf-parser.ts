// ELF64 parser for ARM64 object files produced by TCC

const ELF_MAGIC = [0x7f, 0x45, 0x4c, 0x46]; // \x7fELF
const ELFCLASS64 = 2;
const ELFDATA2LSB = 1; // Little-endian
const EM_AARCH64 = 183;

// Section types
const SHT_SYMTAB = 2;
const SHT_STRTAB = 3;
const SHT_RELA = 4;

// Symbol binding
const STB_GLOBAL = 1;

// Relocation types for AArch64
export const R_AARCH64_CALL26 = 283;
export const R_AARCH64_ADR_PREL_PG_HI21 = 275;
export const R_AARCH64_ADD_ABS_LO12_NC = 277;
export const R_AARCH64_LDST64_ABS_LO12_NC = 286;
export const R_AARCH64_LDST32_ABS_LO12_NC = 285;
export const R_AARCH64_LDST8_ABS_LO12_NC = 278;
export const R_AARCH64_MOVW_UABS_G0_NC = 264;
export const R_AARCH64_MOVW_UABS_G1_NC = 266;
export const R_AARCH64_MOVW_UABS_G2_NC = 268;
export const R_AARCH64_MOVW_UABS_G3 = 269;
export const R_AARCH64_ADR_GOT_PAGE = 311;
export const R_AARCH64_LD64_GOT_LO12_NC = 312;
export const R_AARCH64_PREL32 = 261;
export const R_AARCH64_ABS64 = 257;

export interface ElfSection {
	name: string;
	type: number;
	flags: number;
	addr: number;
	offset: number;
	size: number;
	data: Uint8Array;
}

export interface ElfSymbol {
	name: string;
	value: number;
	size: number;
	binding: number;
	type: number;
	sectionIndex: number;
}

export interface ElfRelocation {
	offset: number;
	type: number;
	symbolIndex: number;
	addend: number;
}

export interface ElfParseResult {
	sections: ElfSection[];
	symbols: ElfSymbol[];
	relocations: ElfRelocation[];
	textSection: ElfSection | null;
	dataSection: ElfSection | null;
	rodataSection: ElfSection | null;
	bssSize: number;
}

function readU16(data: Uint8Array, off: number): number {
	return data[off] | (data[off + 1] << 8);
}

function readU32(data: Uint8Array, off: number): number {
	return (data[off] | (data[off + 1] << 8) | (data[off + 2] << 16) | (data[off + 3] << 24)) >>> 0;
}

function readU64(data: Uint8Array, off: number): number {
	// For ELF we only need values that fit in 53 bits (safe integer range)
	const lo = readU32(data, off);
	const hi = readU32(data, off + 4);
	return lo + hi * 0x100000000;
}

function readI64(data: Uint8Array, off: number): number {
	const lo = readU32(data, off);
	const hi = readU32(data, off + 4);
	// Sign extend from 64-bit
	if (hi & 0x80000000) {
		return -(0x100000000 * (0xffffffff - hi) + (0x100000000 - lo));
	}
	return lo + hi * 0x100000000;
}

function readCString(data: Uint8Array, off: number): string {
	let end = off;
	while (end < data.length && data[end] !== 0) end++;
	return new TextDecoder().decode(data.slice(off, end));
}

export function parseElf(data: Uint8Array): ElfParseResult {
	// Verify ELF magic
	for (let i = 0; i < 4; i++) {
		if (data[i] !== ELF_MAGIC[i]) {
			throw new Error("Not an ELF file");
		}
	}

	if (data[4] !== ELFCLASS64) throw new Error("Not a 64-bit ELF");
	if (data[5] !== ELFDATA2LSB) throw new Error("Not little-endian");
	const machine = readU16(data, 18);
	if (machine !== EM_AARCH64) throw new Error(`Not ARM64 (machine=${machine})`);

	// ELF header fields
	const shoff = readU64(data, 40);   // Section header table offset
	const shentsize = readU16(data, 58);
	const shnum = readU16(data, 60);
	const shstrndx = readU16(data, 62);

	// Read section headers
	const rawSections: {
		nameIdx: number;
		type: number;
		flags: number;
		addr: number;
		offset: number;
		size: number;
		link: number;
		info: number;
		entsize: number;
	}[] = [];

	for (let i = 0; i < shnum; i++) {
		const base = shoff + i * shentsize;
		rawSections.push({
			nameIdx: readU32(data, base),
			type: readU32(data, base + 4),
			flags: readU64(data, base + 8),
			addr: readU64(data, base + 16),
			offset: readU64(data, base + 24),
			size: readU64(data, base + 32),
			link: readU32(data, base + 40),
			info: readU32(data, base + 44),
			entsize: readU64(data, base + 56),
		});
	}

	// Read section name string table
	const shstrtab = rawSections[shstrndx];
	const shstrData = data.slice(shstrtab.offset, shstrtab.offset + shstrtab.size);

	// Build section objects
	const sections: ElfSection[] = rawSections.map((raw) => ({
		name: readCString(shstrData, raw.nameIdx),
		type: raw.type,
		flags: raw.flags,
		addr: raw.addr,
		offset: raw.offset,
		size: raw.size,
		data: data.slice(raw.offset, raw.offset + raw.size),
	}));

	// Find key sections
	const textSection = sections.find((s) => s.name === ".text") ?? null;
	const dataSection = sections.find((s) => s.name === ".data") ?? null;
	const rodataSection = sections.find((s) => s.name === ".rodata" || s.name === ".data.ro") ?? null;
	const bssSection = sections.find((s) => s.name === ".bss");
	const bssSize = bssSection?.size ?? 0;

	// Parse symbol table
	const symbols: ElfSymbol[] = [];
	for (let si = 0; si < rawSections.length; si++) {
		const raw = rawSections[si];
		if (raw.type !== SHT_SYMTAB) continue;

		const strtabSection = rawSections[raw.link];
		const strtabData = data.slice(strtabSection.offset, strtabSection.offset + strtabSection.size);
		const entsize = raw.entsize || 24;
		const count = raw.size / entsize;

		for (let j = 0; j < count; j++) {
			const base = raw.offset + j * entsize;
			const nameIdx = readU32(data, base);
			const info = data[base + 4];
			const sectionIndex = readU16(data, base + 6);
			const value = readU64(data, base + 8);
			const size = readU64(data, base + 16);

			symbols.push({
				name: readCString(strtabData, nameIdx),
				value,
				size,
				binding: info >> 4,
				type: info & 0xf,
				sectionIndex,
			});
		}
	}

	// Parse relocations
	const relocations: ElfRelocation[] = [];
	for (let si = 0; si < rawSections.length; si++) {
		const raw = rawSections[si];
		if (raw.type !== SHT_RELA) continue;

		// Only process .rela.text
		const targetName = sections[si].name;
		if (targetName !== ".rela.text") continue;

		const entsize = raw.entsize || 24;
		const count = raw.size / entsize;

		for (let j = 0; j < count; j++) {
			const base = raw.offset + j * entsize;
			const offset = readU64(data, base);
			const info = readU64(data, base + 8);
			const addend = readI64(data, base + 16);

			const symbolIndex = Math.floor(info / 0x100000000);
			const type = info & 0xffffffff;

			relocations.push({ offset, type, symbolIndex, addend });
		}
	}

	return { sections, symbols, relocations, textSection, dataSection, rodataSection, bssSize };
}
