import { transpileElixir, extractElixirDefs } from "@/lib/elixir-runner";
import { elixirLessons } from "@/lib/lessons/elixir";
import type { LessonTestResult } from "./types";

export function runElixirTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];

  for (const lesson of elixirLessons) {
    for (const test of lesson.tests) {
      let codeToRun = lesson.solution;
      if (test.code) {
        const defs = extractElixirDefs(lesson.solution);
        codeToRun = test.code.replace("{{FUNC}}", defs);
      }

      try {
        const js = transpileElixir(codeToRun);
        const fn = new Function(js + "\nreturn __output.join('');");
        const actual = fn() as string;
        results.push({
          course: "elixir",
          lessonId: lesson.id,
          lessonTitle: lesson.title,
          testName: test.name,
          passed: actual === test.expected,
          actual,
          expected: test.expected,
        });
      } catch (err) {
        results.push({
          course: "elixir",
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
