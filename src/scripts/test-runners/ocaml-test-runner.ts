import { transpileOCaml, extractOCamlDeclarations } from "@/lib/ocaml-runner";
import { ocamlLessons } from "@/lib/lessons/ocaml";
import type { LessonTestResult } from "./types";

export function runOCamlTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];

  for (const lesson of ocamlLessons) {
    for (const test of lesson.tests) {
      let codeToRun = lesson.solution;
      if (test.code) {
        const decls = extractOCamlDeclarations(lesson.solution);
        codeToRun = test.code.replace("{{FUNC}}", decls);
      }

      try {
        const js = transpileOCaml(codeToRun);
        // eslint-disable-next-line no-new-func
        const fn = new Function(js + "\nreturn __output.join('');");
        const actual = fn() as string;
        results.push({
          course: "ocaml",
          lessonId: lesson.id,
          lessonTitle: lesson.title,
          testName: test.name,
          passed: actual.trim() === test.expected.trim(),
          actual,
          expected: test.expected,
        });
      } catch (err) {
        results.push({
          course: "ocaml",
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
