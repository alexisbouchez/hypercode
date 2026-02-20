import { execSync } from "child_process";
import { writeFileSync, mkdirSync, rmSync } from "fs";
import { join } from "path";
import { tmpdir } from "os";
import { extractRubyMethods } from "@/lib/ruby-runner";
import { rubyLessons } from "@/lib/lessons/ruby/index";
import type { LessonTestResult } from "./types";

export function runRubyTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];
  const tmp = join(tmpdir(), `hypercode-ruby-${Date.now()}`);
  mkdirSync(tmp, { recursive: true });

  try {
    for (const lesson of rubyLessons) {
      for (const test of lesson.tests) {
        let codeToRun = lesson.solution;

        if (test.code) {
          const methods = extractRubyMethods(lesson.solution);
          codeToRun = test.code.replace("{{FUNC}}", methods);
        }

        const file = join(tmp, "script.rb");
        writeFileSync(file, codeToRun);

        let actual: string;
        let passed: boolean;

        try {
          const output = execSync("ruby script.rb", {
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
          course: "ruby",
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
