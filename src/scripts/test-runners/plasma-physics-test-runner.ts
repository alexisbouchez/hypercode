import { execSync } from "child_process";
import { writeFileSync, unlinkSync } from "fs";
import { tmpdir } from "os";
import { join } from "path";
import { extractPythonFunctions } from "@/lib/python-runner";
import { plasmaPhysicsLessons } from "@/lib/lessons/plasma-physics/index";
import type { LessonTestResult } from "./types";

export function runPlasmaPhysicsTests(): LessonTestResult[] {
	const results: LessonTestResult[] = [];

	for (const lesson of plasmaPhysicsLessons) {
		for (const test of lesson.tests) {
			const codeToRun = test.code
				? test.code.replace("{{FUNC}}", extractPythonFunctions(lesson.solution))
				: lesson.solution;

			const tmpFile = join(tmpdir(), `hypercode_plasma_physics_${Date.now()}_${Math.random().toString(36).slice(2)}.py`);
			try {
				writeFileSync(tmpFile, codeToRun);
				const actual = execSync(`python3 "${tmpFile}"`, {
					timeout: 30000,
					env: { ...process.env, PYTHONIOENCODING: "utf-8" },
				}).toString();

				results.push({
					course: "plasma-physics",
					lessonId: lesson.id,
					lessonTitle: lesson.title,
					testName: test.name,
					passed: actual === test.expected,
					actual,
					expected: test.expected,
				});
			} catch (err: any) {
				const actual = err.stdout
					? err.stdout.toString()
					: err.message || String(err);
				results.push({
					course: "plasma-physics",
					lessonId: lesson.id,
					lessonTitle: lesson.title,
					testName: test.name,
					passed: false,
					actual,
					expected: test.expected,
				});
			} finally {
				try { unlinkSync(tmpFile); } catch {}
			}
		}
	}

	return results;
}
