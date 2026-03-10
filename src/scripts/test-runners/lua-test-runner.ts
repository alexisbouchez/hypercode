import { transpileLua, extractLuaDeclarations } from "@/lib/lua-runner";
import { luaLessons } from "@/lib/lessons/lua";
import type { LessonTestResult } from "./types";

export function runLuaTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];

  for (const lesson of luaLessons) {
    for (const test of lesson.tests) {
      let codeToRun = lesson.solution;
      if (test.code) {
        const decls = extractLuaDeclarations(lesson.solution);
        codeToRun = test.code.replace("{{FUNC}}", decls);
      }

      try {
        const js = transpileLua(codeToRun);
        // eslint-disable-next-line no-new-func
        const fn = new Function(js + "\nreturn __output.join('');");
        const actual = fn() as string;
        results.push({
          course: "lua",
          lessonId: lesson.id,
          lessonTitle: lesson.title,
          testName: test.name,
          passed: actual.trim() === test.expected.trim(),
          actual,
          expected: test.expected,
        });
      } catch (err) {
        results.push({
          course: "lua",
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
