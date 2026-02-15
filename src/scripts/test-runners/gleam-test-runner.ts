import { execSync } from "child_process";
import { writeFileSync, mkdirSync, rmSync, readFileSync } from "fs";
import { join } from "path";
import { tmpdir } from "os";
import { extractGleamFunctions } from "@/lib/gleam-runner";
import { gleamLessons } from "@/lib/lessons/gleam/index";
import type { LessonTestResult } from "./types";

function hasGleam(): boolean {
  try {
    execSync("gleam --version", { stdio: "pipe" });
    return true;
  } catch {
    return false;
  }
}

export function runGleamTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];

  if (!hasGleam()) {
    console.log("  (skipped: gleam CLI not found)");
    return results;
  }

  const tmp = join(tmpdir(), `hypercode-gleam-${Date.now()}`);
  mkdirSync(tmp, { recursive: true });

  try {
    // Initialize a Gleam project with JavaScript target
    execSync("gleam new test_project --skip-github --skip-git", {
      cwd: tmp,
      stdio: "pipe",
    });

    const projectDir = join(tmp, "test_project");

    // Set target to JavaScript and remove test/dev-dependency files
    const tomlPath = join(projectDir, "gleam.toml");
    let toml = readFileSync(tomlPath, "utf-8");
    // Insert target after the version line (must be in top-level section, not under [dependencies])
    toml = toml.replace(
      /^(version\s*=\s*".+")$/m,
      '$1\ntarget = "javascript"',
    );
    // Remove dev-dependencies section to avoid gleeunit issues
    toml = toml.replace(/\[dev-dependencies\][\s\S]*$/, "");
    writeFileSync(tomlPath, toml);
    rmSync(join(projectDir, "test"), { recursive: true, force: true });

    for (const lesson of gleamLessons) {
      for (const test of lesson.tests) {
        let codeToRun = lesson.solution;

        if (test.code) {
          const funcs = extractGleamFunctions(lesson.solution);
          codeToRun = test.code.replace("{{FUNC}}", funcs);
        }

        const file = join(projectDir, "src", "test_project.gleam");
        writeFileSync(file, codeToRun);

        let actual: string;
        let passed: boolean;

        try {
          const output = execSync("gleam run 2>&1", {
            cwd: projectDir,
            timeout: 30_000,
            stdio: "pipe",
            shell: "/bin/sh",
          });
          // Filter out Gleam build progress lines (Compiling, Compiled, Running)
          actual = output
            .toString()
            .split("\n")
            .filter((line: string) => !line.match(/^\s*(Compiling|Compiled|Running|Downloaded|Downloading|Resolving|Added)\s/))
            .join("\n");
          passed = actual === test.expected;
        } catch (err: unknown) {
          const e = err as { stderr?: Buffer; stdout?: Buffer };
          actual = (e.stdout?.toString() || e.stderr?.toString() || String(err)).trim();
          passed = false;
        }

        results.push({
          course: "gleam",
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
