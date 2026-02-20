import { execSync } from "child_process";
import { writeFileSync, mkdirSync, rmSync } from "fs";
import { join } from "path";
import { tmpdir } from "os";
import { extractHaskellFunctions } from "@/lib/haskell-runner";
import { haskellLessons } from "@/lib/lessons/haskell/index";
import type { LessonTestResult } from "./types";

const RUNGHC = "/opt/zerobrew/prefix/Cellar/ghc/9.14.1/lib/ghc-9.14.1/bin/runghc-9.14.1";
const GHC = "/opt/zerobrew/prefix/Cellar/ghc/9.14.1/lib/ghc-9.14.1/bin/ghc";

function hasRunghc(): boolean {
  try {
    execSync(`${RUNGHC} --version`, { stdio: "pipe" });
    return true;
  } catch {
    return false;
  }
}

export function runHaskellTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];

  if (!hasRunghc()) {
    console.log("  (skipped: runghc not found)");
    return results;
  }

  const tmp = join(tmpdir(), `hypercode-haskell-${Date.now()}`);
  mkdirSync(tmp, { recursive: true });

  try {
    for (const lesson of haskellLessons) {
      for (const test of lesson.tests) {
        let codeToRun = lesson.solution;

        if (test.code) {
          const fns = extractHaskellFunctions(lesson.solution);
          codeToRun = test.code.replace("{{FUNC}}", fns);
        }

        const file = join(tmp, "script.hs");
        writeFileSync(file, codeToRun);

        let actual: string;
        let passed: boolean;

        try {
          const cmd = `${RUNGHC} -f ${GHC} script.hs`;
          const output = execSync(cmd, {
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
          course: "haskell",
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
