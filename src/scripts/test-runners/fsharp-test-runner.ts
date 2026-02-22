import { transpileFSharp, extractFSharpDeclarations } from "@/lib/fsharp-runner";
import { fsharpLessons } from "@/lib/lessons/fsharp";
import type { LessonTestResult } from "./types";

export function runFSharpTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];

  for (const lesson of fsharpLessons) {
    for (const test of lesson.tests) {
      let codeToRun = lesson.solution;
      if (test.code) {
        const defs = extractFSharpDeclarations(lesson.solution);
        codeToRun = test.code.replace("{{FUNC}}", defs);
      }

      try {
        const js = transpileFSharp(codeToRun);
        const fn = new Function(js + "\nreturn __output.join('');");
        const actual = fn() as string;
        results.push({
          course: "fsharp",
          lessonId: lesson.id,
          lessonTitle: lesson.title,
          testName: test.name,
          passed: actual === test.expected,
          actual,
          expected: test.expected,
        });
      } catch (err) {
        results.push({
          course: "fsharp",
          lessonId: lesson.id,
          lessonTitle: lesson.title,
          testName: test.name,
          passed: false,
          actual: String(err),
          expected: test.expected,
        });
      }
    }
  }

  return results;
}
