import type { RunResult, Test, TestResult } from "@/lib/lessons/types";
import { transpileCpp, executeCppJs } from "./cpp-transpiler";

let cppReady = false;

export function isCppReady(): boolean {
	return cppReady;
}

export function initCppRunner(): Promise<void> {
	cppReady = true;
	return Promise.resolve();
}

export async function runCpp(code: string): Promise<RunResult> {
	const { js, error: transpileError } = transpileCpp(code);
	if (transpileError) {
		return { stdout: "", stderr: "", error: transpileError };
	}
	const { stdout, error } = executeCppJs(js);
	return { stdout, stderr: "", error };
}

export function extractCppFunctions(code: string): string {
	let result = code;
	result = result.replace(/^\s*#include\s+[<"][^>"]+[>"]\s*\n/gm, "");
	result = result.replace(/^\s*using\s+namespace\s+\w+;\s*\n/gm, "");

	const mainRegex = /int\s+main\s*\([^)]*\)\s*\{/;
	const match = mainRegex.exec(result);
	if (match) {
		const startIndex = match.index;
		let braceCount = 0;
		let endIndex = startIndex;
		let foundOpen = false;
		for (let i = startIndex; i < result.length; i++) {
			if (result[i] === "{") { braceCount++; foundOpen = true; }
			else if (result[i] === "}") {
				braceCount--;
				if (foundOpen && braceCount === 0) { endIndex = i + 1; break; }
			}
		}
		result = result.slice(0, startIndex) + result.slice(endIndex);
	}
	return result.trim();
}

export async function runTests(
	code: string,
	tests: Test[],
): Promise<TestResult[]> {
	const results: TestResult[] = [];
	for (const test of tests) {
		try {
			const codeToRun = test.code
				? test.code.replace("{{FUNC}}", extractCppFunctions(code))
				: code;
			const { js, error: transpileError } = transpileCpp(codeToRun);
			if (transpileError) {
				results.push({ name: test.name, passed: false, actual: transpileError, expected: test.expected });
				continue;
			}
			const { stdout, error } = executeCppJs(js);
			const hasError = error !== "";
			const actual = hasError ? error : stdout;
			results.push({ name: test.name, passed: !hasError && stdout === test.expected, actual, expected: test.expected });
		} catch (err) {
			results.push({ name: test.name, passed: false, actual: err instanceof Error ? err.message : String(err), expected: test.expected });
		}
	}
	return results;
}
