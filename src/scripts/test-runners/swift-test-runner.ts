import { execSync } from "child_process";
import { writeFileSync, mkdirSync, rmSync } from "fs";
import { join } from "path";
import { tmpdir } from "os";
import { extractSwiftDeclarations } from "@/lib/swift-runner";
import { swiftLessons } from "@/lib/lessons/swift/index";
import type { LessonTestResult } from "./types";

function hasSwift(): boolean {
  try {
    execSync("swift --version", { stdio: "pipe" });
    return true;
  } catch {
    return false;
  }
}

export function runSwiftTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];

  if (!hasSwift()) {
    console.log("  (skipped: swift not found)");
    return results;
  }

  const tmp = join(tmpdir(), `hypercode-swift-${Date.now()}`);
  mkdirSync(tmp, { recursive: true });

  try {
    for (const lesson of swiftLessons) {
      for (const test of lesson.tests) {
        let codeToRun = lesson.solution;

        if (test.code) {
          const decls = extractSwiftDeclarations(lesson.solution);
          codeToRun = test.code.replace("{{FUNC}}", decls);
        }

        const file = join(tmp, "main.swift");
        writeFileSync(file, codeToRun);

        let actual: string;
        let passed: boolean;

        try {
          const output = execSync("swift main.swift", {
            cwd: tmp,
            timeout: 30_000,
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
          course: "swift",
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
