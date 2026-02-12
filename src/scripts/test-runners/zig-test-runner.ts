import { execSync } from "child_process";
import { writeFileSync, mkdirSync, rmSync } from "fs";
import { join } from "path";
import { tmpdir } from "os";
import { extractZigFunctions } from "@/lib/zig-runner";
import { zigLessons } from "@/lib/lessons/zig/index";
import type { LessonTestResult } from "./types";

export function runZigTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];
  const tmp = join(tmpdir(), `hypercode-zig-${Date.now()}`);
  mkdirSync(tmp, { recursive: true });

  try {
    for (const lesson of zigLessons) {
      for (const test of lesson.tests) {
        let codeToRun = lesson.solution;

        if (test.code) {
          const funcs = extractZigFunctions(lesson.solution);
          codeToRun = test.code.replace("{{FUNC}}", funcs);
        }

        const file = join(tmp, "main.zig");
        writeFileSync(file, codeToRun);

        let actual: string;
        let passed: boolean;

        try {
          const output = execSync("zig run main.zig 2>&1", {
            cwd: tmp,
            timeout: 30_000,
            stdio: "pipe",
            shell: "/bin/sh",
          });
          actual = output.toString();
          passed = actual === test.expected;
        } catch (err: unknown) {
          const e = err as { stderr?: Buffer; stdout?: Buffer };
          actual = (e.stdout?.toString() || e.stderr?.toString() || String(err)).trim();
          passed = false;
        }

        results.push({
          course: "zig",
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
