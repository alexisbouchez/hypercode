import { execSync } from "child_process";
import { writeFileSync, mkdirSync, rmSync } from "fs";
import { join } from "path";
import { tmpdir } from "os";
import { extractJsFunctions } from "@/lib/js-runner";
import { wavesLessons } from "@/lib/lessons/waves/index";
import type { LessonTestResult } from "./types";

export function runWavesTests(): LessonTestResult[] {
	const results: LessonTestResult[] = [];
	const tmp = join(tmpdir(), `hypercode-waves-${Date.now()}`);
	mkdirSync(tmp, { recursive: true });

	try {
		for (const lesson of wavesLessons) {
			for (const test of lesson.tests) {
				let codeToRun = lesson.solution;

				if (test.code) {
					const funcs = extractJsFunctions(lesson.solution);
					codeToRun = test.code.replace("{{FUNC}}", funcs);
				}

				const file = join(tmp, "script.js");
				writeFileSync(file, codeToRun);

				let actual: string;
				let passed: boolean;

				try {
					const output = execSync("node script.js", {
						cwd: tmp,
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
					course: "waves",
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
