import { cppLessons } from "@/lib/lessons/cpp/index";
import { transpileCpp, executeCppJs } from "@/lib/cpp-transpiler";
import type { LessonTestResult } from "./types";

function runCpp(code: string): { stdout: string; error: string } {
	const { js, error: transpileError } = transpileCpp(code);
	if (transpileError) return { stdout: "", error: transpileError };
	return executeCppJs(js);
}

export function runCppTests(): LessonTestResult[] {
	const results: LessonTestResult[] = [];

	for (const lesson of cppLessons) {
		for (const test of lesson.tests) {
			const codeToRun = test.code
				? test.code.replace("{{FUNC}}", lesson.solution)
				: lesson.solution;

			const result = runCpp(codeToRun);
			const hasError = result.error !== "";
			const actual = hasError ? result.error : result.stdout;
			const passed = !hasError && result.stdout === test.expected;

			results.push({
				course: "cpp",
				lessonId: lesson.id,
				lessonTitle: lesson.title,
				testName: test.name,
				passed,
				actual,
				expected: test.expected,
			});
		}
	}

	return results;
}
