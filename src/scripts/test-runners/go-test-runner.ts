import { execSync } from "child_process";
import { writeFileSync, mkdirSync, rmSync } from "fs";
import { join } from "path";
import { tmpdir } from "os";
import { extractFunctions } from "@/lib/go-runner";
import { lessons } from "@/lib/lessons/index";
import type { LessonTestResult } from "./types";

export function runGoTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];
  const tmp = join(tmpdir(), `hypercode-go-${Date.now()}`);
  mkdirSync(tmp, { recursive: true });

  try {
    // Initialize a Go module in the temp directory
    execSync("go mod init hypercode-test", { cwd: tmp, stdio: "pipe" });

    for (const lesson of lessons) {
      for (const test of lesson.tests) {
        let codeToRun = lesson.solution;

        if (test.code) {
          const funcs = extractFunctions(lesson.solution);
          codeToRun = test.code.replace("{{FUNC}}", funcs);
        }

        const file = join(tmp, "main.go");
        writeFileSync(file, codeToRun);

        let actual: string;
        let passed: boolean;

        try {
          const output = execSync("go run main.go", {
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
          course: "go",
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
