import { htmlLessons } from "@/lib/lessons/html";
import type { LessonTestResult } from "./types";

export function runHtmlTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];

  for (const lesson of htmlLessons) {
    for (const test of lesson.tests) {
      let actual = "";

      if (test.code) {
        const codeToRun = test.code.replace("{{HTML}}", JSON.stringify(lesson.solution));
        const origLog = console.log;
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        console.log = (...args: any[]) => {
          actual += args.map((a) => (typeof a === "object" && a !== null ? JSON.stringify(a) : String(a))).join(" ") + "\n";
        };
        try {
          new Function(codeToRun)();
        } catch (_) {
          // ignore
        } finally {
          console.log = origLog;
        }
      }

      results.push({
        course: "html",
        lessonId: lesson.id,
        lessonTitle: lesson.title,
        testName: test.name,
        passed: actual === test.expected,
        actual,
        expected: test.expected,
      });
    }
  }

  return results;
}
