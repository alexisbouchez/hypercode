import { execSync } from "child_process";
import { writeFileSync, mkdirSync, rmSync } from "fs";
import { join } from "path";
import { tmpdir } from "os";
import { extractCSharpDeclarations } from "@/lib/csharp-runner";
import { csharpLessons } from "@/lib/lessons/csharp/index";
import type { LessonTestResult } from "./types";

const CSPROJ = `<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>enable</Nullable>
    <ImplicitUsings>enable</ImplicitUsings>
  </PropertyGroup>
</Project>
`;

export function runCSharpTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];
  const tmp = join(tmpdir(), `hypercode-csharp-${Date.now()}`);
  mkdirSync(tmp, { recursive: true });

  // Write the project file once
  writeFileSync(join(tmp, "test.csproj"), CSPROJ);

  try {
    for (const lesson of csharpLessons) {
      for (const test of lesson.tests) {
        let codeToRun = lesson.solution;

        if (test.code) {
          const declarations = extractCSharpDeclarations(lesson.solution);
          codeToRun = test.code.replace("{{FUNC}}", declarations);
        }

        writeFileSync(join(tmp, "Program.cs"), codeToRun);

        let actual: string;
        let passed: boolean;

        try {
          const output = execSync("dotnet run --project test.csproj", {
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
          course: "csharp",
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
