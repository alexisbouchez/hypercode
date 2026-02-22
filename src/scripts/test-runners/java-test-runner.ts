import { execSync } from "child_process";
import { writeFileSync, mkdirSync, rmSync } from "fs";
import { join } from "path";
import { tmpdir } from "os";
import { javaLessons } from "@/lib/lessons/java/index";
import type { LessonTestResult } from "./types";

export function runJavaTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];
  const tmp = join(tmpdir(), `hypercode-java-${Date.now()}`);
  mkdirSync(tmp, { recursive: true });

  try {
    for (const lesson of javaLessons) {
      for (const test of lesson.tests) {
        const codeToRun = lesson.solution;

        const file = join(tmp, "Main.java");
        writeFileSync(file, codeToRun);

        let actual: string;
        let passed: boolean;

        try {
          execSync("javac Main.java", {
            cwd: tmp,
            timeout: 30_000,
            stdio: "pipe",
          });
          const output = execSync("java Main", {
            cwd: tmp,
            timeout: 10_000,
            stdio: "pipe",
          });
          actual = output.toString();
          passed = actual === test.expected;
        } catch (err: unknown) {
          const e = err as { stderr?: Buffer; stdout?: Buffer };
          actual = (
            e.stderr?.toString() ||
            e.stdout?.toString() ||
            String(err)
          ).trim();
          passed = false;
        }

        results.push({
          course: "java",
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
