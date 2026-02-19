import { execSync } from "child_process";
import { writeFileSync, mkdirSync, rmSync } from "fs";
import { join } from "path";
import { tmpdir } from "os";
import { extractHolycFunctions } from "@/lib/holyc-runner";
import { holycLessons } from "@/lib/lessons/holyc/index";
import type { LessonTestResult } from "./types";

function hasAiwnios(): boolean {
  try {
    execSync("aiwnios --version", { stdio: "pipe" });
    return true;
  } catch {
    return false;
  }
}

export function runHolycTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];

  if (!hasAiwnios()) {
    console.log("  (skipped: aiwnios CLI not found)");
    return results;
  }

  const tmp = join(tmpdir(), `hypercode-holyc-${Date.now()}`);
  mkdirSync(tmp, { recursive: true });

  try {
    for (const lesson of holycLessons) {
      for (const test of lesson.tests) {
        let codeToRun = lesson.solution;

        if (test.code) {
          const funcs = extractHolycFunctions(lesson.solution);
          codeToRun = test.code.replace("{{FUNC}}", funcs);
        }

        const file = join(tmp, "main.hc");
        writeFileSync(file, codeToRun);

        let actual: string;
        let passed: boolean;

        try {
          const output = execSync(`aiwnios ${file}`, {
            cwd: tmp,
            timeout: 15_000,
            stdio: "pipe",
          });
          actual = output.toString();
          passed = actual === test.expected;
        } catch (err: unknown) {
          const e = err as { stderr?: Buffer; stdout?: Buffer };
          actual = (e.stdout?.toString() || e.stderr?.toString() || String(err)).trim();
          passed = false;
        }

        results.push({
          course: "holyc",
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
