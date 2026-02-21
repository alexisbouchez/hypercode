import { execSync } from "child_process";
import { writeFileSync, mkdirSync, rmSync } from "fs";
import { join } from "path";
import { tmpdir } from "os";
import { extractCFunctions } from "@/lib/c-runner";
import { classicalMechanicsLessons } from "@/lib/lessons/classical-mechanics/index";
import type { LessonTestResult } from "./types";

export function runClassicalMechanicsTests(): LessonTestResult[] {
	const results: LessonTestResult[] = [];
	const tmp = join(tmpdir(), `hypercode-classical-mechanics-${Date.now()}`);
	mkdirSync(tmp, { recursive: true });

	try {
		for (const lesson of classicalMechanicsLessons) {
			for (const test of lesson.tests) {
				let codeToRun = lesson.solution;

				if (test.code) {
					const funcs = extractCFunctions(lesson.solution);
					codeToRun = `#include <stdio.h>\n#include <math.h>\n` + test.code.replace("{{FUNC}}", funcs);
				}

				const srcFile = join(tmp, "prog.c");
				const binFile = join(tmp, "prog");
				writeFileSync(srcFile, codeToRun);

				let actual: string;
				let passed: boolean;

				try {
					execSync(`gcc -o "${binFile}" "${srcFile}" -lm`, {
						timeout: 15_000,
						stdio: "pipe",
					});
					const output = execSync(`"${binFile}"`, {
						timeout: 10_000,
						stdio: "pipe",
					});
					actual = output.toString();
					passed = actual === test.expected;
				} catch (err: unknown) {
					const e = err as { stderr?: Buffer; stdout?: Buffer };
					actual = (e.stderr?.toString() || e.stdout?.toString() || String(err)).trim();
					passed = false;
				}

				results.push({
					course: "classical-mechanics",
					lessonId: lesson.id,
					lessonTitle: lesson.title,
					testName: test.name,
					passed,
					actual,
					expected: test.expected,
				});
			}
		}
	} finally {
		rmSync(tmp, { recursive: true, force: true });
	}

	return results;
}
