import { cLessons } from "@/lib/lessons/c/index";
import { parseElf } from "@/lib/c/elf-parser";
import { disassemble } from "@/lib/c/arm64-disasm";
import { link, resolveBranchTargets } from "@/lib/c/linker";
import { C_RUNTIME } from "@/lib/c/runtime";
import { HEADERS } from "@/lib/c/headers";
import { assemble } from "@/lib/arm64/assembler";
import { execute } from "@/lib/arm64/interpreter";
import type { LessonTestResult } from "./types";

// We need to load TCC as a Node.js module
let tccModule: {
	FS: {
		writeFile(path: string, data: string | Uint8Array): void;
		readFile(path: string, opts?: { encoding?: string }): Uint8Array;
		mkdir(path: string): void;
	};
	callMain(args: string[]): number;
} | null = null;

async function initTCC(): Promise<void> {
	if (tccModule) return;

	const path = await import("path");
	const { fileURLToPath } = await import("url");
	const tccPath = path.resolve(
		path.dirname(fileURLToPath(import.meta.url)),
		"../../../public/tcc/tcc.js",
	);

	// Dynamic import of the TCC module
	const createTCC = (await import(tccPath)).default;
	tccModule = await createTCC({
		print: () => {},
		printErr: () => {},
	});

	if (!tccModule) throw new Error("Failed to initialize TCC");

	// Set up headers
	try { tccModule.FS.mkdir("/usr"); } catch {}
	try { tccModule.FS.mkdir("/usr/include"); } catch {}
	try { tccModule.FS.mkdir("/tmp"); } catch {}

	for (const [name, content] of Object.entries(HEADERS)) {
		tccModule.FS.writeFile(`/usr/include/${name}`, content);
	}
}

function compileCToElf(source: string): Uint8Array {
	if (!tccModule) throw new Error("TCC not initialized");

	tccModule.FS.writeFile("/tmp/input.c", source);

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

	const textIdx = elf.sections.indexOf(elf.textSection);
	const disasmLines = disassemble(elf.textSection.data, elf.symbols, textIdx);
	const dataLabels = new Map<string, number>();

	for (const sym of elf.symbols) {
		if (sym.name) {
			const sec = elf.sections[sym.sectionIndex];
			if (sec && (sec.name === ".data" || sec.name === ".rodata")) {
				dataLabels.set(sym.name, sym.value);
			}
		}
	}

	const linked = link(disasmLines, elf, dataLabels);
	const asmText = resolveBranchTargets(linked);

	// Build data section
	let dataSection = "";
	const buildDataDirectives = (section: typeof elf.dataSection) => {
		if (!section || section.data.length === 0) return "";
		const sectionIdx = elf.sections.indexOf(section);
		const sectionSyms = elf.symbols
			.filter((s) => s.sectionIndex === sectionIdx && s.name)
			.sort((a, b) => a.value - b.value);

		let result = "";
		for (let si = 0; si < sectionSyms.length; si++) {
			const sym = sectionSyms[si];
			const nextOff = si + 1 < sectionSyms.length ? sectionSyms[si + 1].value : section.data.length;
			const bytes = section.data.slice(sym.value, nextOff);

			result += `${sym.name}:\n`;

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

			if (bytes.length > 0) {
				const byteStrs: string[] = [];
				for (const b of bytes) byteStrs.push(String(b));
				result += `\t.byte ${byteStrs.join(", ")}\n`;
			}
		}
		return result;
	};

	const dataDirectives = buildDataDirectives(elf.dataSection);
	const rodataDirectives = buildDataDirectives(elf.rodataSection);

	if (dataDirectives || rodataDirectives) {
		dataSection = ".data\n" + dataDirectives + rodataDirectives;
	}

	let fullAsm = "";
	if (dataSection) {
		fullAsm += dataSection + "\n";
	}
	fullAsm += ".text\n" + asmText;

	return fullAsm;
}

function runAssembly(assembly: string): { stdout: string; error: string } {
	const fullSource = C_RUNTIME + "\n" + assembly;
	try {
		const program = assemble(fullSource);
		const result = execute(program);
		return { stdout: result.stdout, error: result.error };
	} catch (err) {
		return { stdout: "", error: err instanceof Error ? err.message : String(err) };
	}
}

export async function runCTests(): Promise<LessonTestResult[]> {
	await initTCC();

	const results: LessonTestResult[] = [];

	for (const lesson of cLessons) {
		for (const test of lesson.tests) {
			try {
				const codeToRun = test.code
					? test.code.replace("{{FUNC}}", lesson.solution)
					: lesson.solution;

				const elfBytes = compileCToElf(codeToRun);
				const assembly = elfToAssembly(elfBytes);
				const result = runAssembly(assembly);

				const hasError = result.error !== "";
				const actual = hasError ? result.error : result.stdout;
				const passed = !hasError && result.stdout === test.expected;

				results.push({
					course: "c",
					lessonId: lesson.id,
					lessonTitle: lesson.title,
					testName: test.name,
					passed,
					actual,
					expected: test.expected,
				});
			} catch (err) {
				results.push({
					course: "c",
					lessonId: lesson.id,
					lessonTitle: lesson.title,
					testName: test.name,
					passed: false,
					actual: err instanceof Error ? err.message : String(err),
					expected: test.expected,
				});
			}
		}
	}

	return results;
}
