import { execSync } from "child_process";
import { writeFileSync, mkdirSync, rmSync } from "fs";
import { join } from "path";
import { tmpdir } from "os";
import { extractTsFunctions } from "@/lib/ts-runner";
import { tsLessons } from "@/lib/lessons/typescript/index";
import type { LessonTestResult } from "./types";

export function runTsTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];
  const tmp = join(tmpdir(), `hypercode-ts-${Date.now()}`);
  mkdirSync(tmp, { recursive: true });

  try {
    for (const lesson of tsLessons) {
      for (const test of lesson.tests) {
        let codeToRun = lesson.solution;

        if (test.code) {
          const funcs = extractTsFunctions(lesson.solution);
          codeToRun = test.code.replace("{{FUNC}}", funcs);
        }

        const file = join(tmp, "script.ts");
        writeFileSync(file, codeToRun);

        let actual: string;
        let passed: boolean;

        try {
          const output = execSync("npx tsx script.ts", {
            cwd: tmp,
            timeout: 15_000,
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
          course: "typescript",
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
