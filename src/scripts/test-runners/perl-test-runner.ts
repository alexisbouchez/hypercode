import { transpilePerl, extractPerlDeclarations } from "@/lib/perl-runner";
import { perlLessons } from "@/lib/lessons/perl";
import type { LessonTestResult } from "./types";

export function runPerlTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];

  for (const lesson of perlLessons) {
    for (const test of lesson.tests) {
      let codeToRun = lesson.solution;
      if (test.code) {
        const decls = extractPerlDeclarations(lesson.solution);
        codeToRun = test.code.replace("{{FUNC}}", decls);
      }

      try {
        const js = transpilePerl(codeToRun);
        // eslint-disable-next-line no-new-func
        const fn = new Function(js + "\nreturn __output.join('');");
        const actual = fn() as string;
        results.push({
          course: "perl",
          lessonId: lesson.id,
          lessonTitle: lesson.title,
          testName: test.name,
          passed: actual.trim() === test.expected.trim(),
          actual,
          expected: test.expected,
        });
      } catch (err) {
        results.push({
          course: "perl",
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
