import type { RunResult, Test, TestResult } from "@/lib/lessons/types";
import { parseElf } from "./c/elf-parser";
import { disassemble } from "./c/arm64-disasm";
import { link, resolveBranchTargets } from "./c/linker";
import { C_RUNTIME } from "./c/runtime";
import { HEADERS } from "./c/headers";
import { assemble } from "./arm64/assembler";
import { execute } from "./arm64/interpreter";

declare function createTCC(options?: Record<string, unknown>): Promise<{
	FS: {
		writeFile(path: string, data: string | Uint8Array): void;
		readFile(path: string, opts?: { encoding?: string }): Uint8Array;
		mkdir(path: string): void;
		stat(path: string): unknown;
	};
	callMain(args: string[]): number;
}>;

let tccModule: Awaited<ReturnType<typeof createTCC>> | null = null;
let ready = false;
let loadPromise: Promise<void> | null = null;

export function initCRunner(): Promise<void> {
	if (ready) return Promise.resolve();
	if (loadPromise) return loadPromise;

	loadPromise = (async () => {
		// Load TCC WASM module
		const script = document.createElement("script");
		script.src = "/tcc/tcc.js";
		await new Promise<void>((resolve, reject) => {
			script.onload = () => resolve();
			script.onerror = () => reject(new Error("Failed to load TCC WASM"));
			document.head.appendChild(script);
		});

		// Initialize TCC module
		const factory = (window as unknown as Record<string, unknown>).createTCC as typeof createTCC;
		tccModule = await factory({
			locateFile: (path: string) => `/tcc/${path}`,
			print: () => {},
			printErr: () => {},
		});

		// Set up include directory with our minimal headers
		try { tccModule.FS.mkdir("/usr"); } catch {}
		try { tccModule.FS.mkdir("/usr/include"); } catch {}

		for (const [name, content] of Object.entries(HEADERS)) {
			tccModule.FS.writeFile(`/usr/include/${name}`, content);
		}

		ready = true;
	})();

	return loadPromise;
}

export function isCReady(): boolean {
	return ready;
}

function compileCToElf(source: string): Uint8Array {
	if (!tccModule) throw new Error("TCC not initialized");

	// Write source to virtual filesystem
	tccModule.FS.writeFile("/tmp/input.c", source);

	// Compile to object file
	const exitCode = tccModule.callMain([
		"-c", "-nostdlib", "-I/usr/include",
		"-o", "/tmp/output.o", "/tmp/input.c",
	]);

	if (exitCode !== 0) {
		throw new Error("Compilation failed");
	}

	return tccModule.FS.readFile("/tmp/output.o");
}

function elfToAssembly(elfBytes: Uint8Array): string {
	const elf = parseElf(elfBytes);

	if (!elf.textSection) {
		throw new Error("No .text section in compiled output");
	}

	// Find text section index
	const textIdx = elf.sections.indexOf(elf.textSection);

	// Disassemble machine code
	const disasmLines = disassemble(elf.textSection.data, elf.symbols, textIdx);

	// Build data label map
	const dataLabels = new Map<string, number>();
	for (const sym of elf.symbols) {
		if (sym.name) {
			const sec = elf.sections[sym.sectionIndex];
			if (sec && (sec.name === ".data" || sec.name === ".rodata")) {
				dataLabels.set(sym.name, sym.value);
			}
		}
	}

	// Link: resolve relocations
	const linked = link(disasmLines, elf, dataLabels);

	// Resolve branch targets to labels
	const asmText = resolveBranchTargets(linked);

	// Build data section from .data and .rodata
	let dataSection = "";
	const buildDataDirectives = (section: typeof elf.dataSection, sectionName: string) => {
		if (!section || section.data.length === 0) return "";
		const sectionIdx = elf.sections.indexOf(section);

		// Collect symbols pointing into this section
		const sectionSyms = elf.symbols
			.filter((s) => s.sectionIndex === sectionIdx && s.name)
			.sort((a, b) => a.value - b.value);

		let result = "";
		for (let si = 0; si < sectionSyms.length; si++) {
			const sym = sectionSyms[si];
			const nextOff = si + 1 < sectionSyms.length ? sectionSyms[si + 1].value : section.data.length;
			const bytes = section.data.slice(sym.value, nextOff);

			result += `${sym.name}:\n`;

			// Try to detect string data (null-terminated)
			if (bytes.length > 0 && bytes[bytes.length - 1] === 0) {
				const strBytes = bytes.slice(0, -1);
				let isString = true;
				for (const b of strBytes) {
					if (b !== 10 && b !== 9 && b !== 13 && (b < 32 || b > 126)) {
						isString = false;
						break;
					}
				}
				if (isString && strBytes.length > 0) {
					const str = new TextDecoder().decode(strBytes)
						.replace(/\\/g, "\\\\")
						.replace(/\n/g, "\\n")
						.replace(/\t/g, "\\t")
						.replace(/"/g, '\\"');
					result += `\t.asciz "${str}"\n`;
					continue;
				}
			}

			// Fall back to .byte directives
			if (bytes.length > 0) {
				const byteStrs: string[] = [];
				for (const b of bytes) byteStrs.push(String(b));
				result += `\t.byte ${byteStrs.join(", ")}\n`;
			}
		}
		return result;
	};

	const dataDirectives = buildDataDirectives(elf.dataSection, ".data");
	const rodataDirectives = buildDataDirectives(elf.rodataSection, ".rodata");

	if (dataDirectives || rodataDirectives) {
		dataSection = ".data\n" + dataDirectives + rodataDirectives;
	}

	// Combine: data section + .text + user code
	let fullAsm = "";
	if (dataSection) {
		fullAsm += dataSection + "\n";
	}
	fullAsm += ".text\n" + asmText;

	return fullAsm;
}

function runAssembly(assembly: string): { stdout: string; error: string } {
	// Prepend C runtime to user assembly
	const fullSource = C_RUNTIME + "\n" + assembly;

	try {
		const program = assemble(fullSource);
		const result = execute(program);
		return { stdout: result.stdout, error: result.error };
	} catch (err) {
		return { stdout: "", error: err instanceof Error ? err.message : String(err) };
	}
}

export async function runC(code: string): Promise<RunResult> {
	try {
		const elfBytes = compileCToElf(code);
		const assembly = elfToAssembly(elfBytes);
		const result = runAssembly(assembly);
		return {
			stdout: result.stdout,
			stderr: "",
			error: result.error,
			generatedCode: assembly,
		};
	} catch (err) {
		return {
			stdout: "",
			stderr: "",
			error: err instanceof Error ? err.message : String(err),
		};
	}
}

export async function runTests(code: string, tests: Test[]): Promise<TestResult[]> {
	const results: TestResult[] = [];

	for (const test of tests) {
		try {
			const codeToRun = test.code ? test.code.replace("{{FUNC}}", code) : code;
			const elfBytes = compileCToElf(codeToRun);
			const assembly = elfToAssembly(elfBytes);
			const result = runAssembly(assembly);

			const hasError = result.error !== "";
			const actual = hasError ? result.error : result.stdout;

			results.push({
				name: test.name,
				passed: !hasError && result.stdout === test.expected,
				actual,
				expected: test.expected,
			});
		} catch (err) {
			results.push({
				name: test.name,
				passed: false,
				actual: err instanceof Error ? err.message : String(err),
				expected: test.expected,
			});
		}
	}

	return results;
}
