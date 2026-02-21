import { execSync } from "child_process";
import { writeFileSync, mkdirSync, rmSync } from "fs";
import { join } from "path";
import { tmpdir } from "os";
import { rustLessons } from "@/lib/lessons/rust/index";
import type { LessonTestResult } from "./types";

function extractRustItems(solution: string): string {
  const mainPattern = /\bfn\s+main\s*\(\s*\)\s*\{/;
  const match = mainPattern.exec(solution);
  if (!match) return solution.trim();
  let depth = 0;
  let i = match.index;
  while (i < solution.length) {
    if (solution[i] === "{") depth++;
    else if (solution[i] === "}") {
      depth--;
      if (depth === 0) {
        return (solution.slice(0, match.index) + solution.slice(i + 1)).trim();
      }
    }
    i++;
  }
  return solution.trim();
}

export function runRustTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];
  const tmp = join(tmpdir(), `hypercode-rust-${Date.now()}`);
  mkdirSync(tmp, { recursive: true });

  try {
    for (const lesson of rustLessons) {
      for (const test of lesson.tests) {
        let codeToRun = lesson.solution;
        if (test.code) {
          codeToRun = test.code.replace("{{FUNC}}", extractRustItems(lesson.solution));
        }

        const srcFile = join(tmp, "main.rs");
        const binFile = join(tmp, "main_bin");
        writeFileSync(srcFile, codeToRun);

        let actual: string;
        let passed: boolean;

        try {
          execSync(`rustc ${srcFile} -o ${binFile} 2>&1`, {
            timeout: 30_000,
            stdio: "pipe",
          });
          const output = execSync(binFile, { timeout: 10_000, stdio: "pipe" });
          actual = output.toString();
          passed = actual === test.expected;
        } catch (err: unknown) {
          const e = err as {
            stderr?: Buffer;
            stdout?: Buffer;
            message?: string;
          };
          actual = (
            e.stderr?.toString() ||
            e.stdout?.toString() ||
            e.message ||
            String(err)
          ).trim();
          passed = false;
        }

        results.push({
          course: "rust",
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
