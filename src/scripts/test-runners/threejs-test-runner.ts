import * as THREE from "three";
import { threejsLessons } from "@/lib/lessons/threejs/index";
import type { LessonTestResult } from "./types";

/** Extract top-level function declarations and arrow-function assignments (mirrors threejs-runner.ts). */
function extractFunctions(code: string): string {
  const lines = code.split("\n");
  const blocks: string[] = [];
  let depth = 0;
  let inBlock = false;
  let current: string[] = [];

  for (const line of lines) {
    const trimmed = line.trim();
    const isFuncDecl = /^function\s+\w+/.test(trimmed);
    const isArrowOrExpr =
      /^(const|let|var)\s+\w+\s*=\s*(async\s+)?(function|\()/.test(trimmed);

    if (!inBlock && (isFuncDecl || isArrowOrExpr)) {
      inBlock = true;
      current = [line];
      depth = 0;
      for (const ch of line) {
        if (ch === "{") depth++;
        else if (ch === "}") depth--;
      }
      if (depth === 0 && !trimmed.endsWith("{")) {
        blocks.push(line);
        current = [];
        inBlock = false;
      }
    } else if (inBlock) {
      current.push(line);
      for (const ch of line) {
        if (ch === "{") depth++;
        else if (ch === "}") depth--;
      }
      if (depth <= 0) {
        blocks.push(current.join("\n"));
        current = [];
        inBlock = false;
        depth = 0;
      }
    }
  }

  return blocks.join("\n\n");
}

export function runThreejsTests(): LessonTestResult[] {
  const results: LessonTestResult[] = [];

  for (const lesson of threejsLessons) {
    const userFunctions = extractFunctions(lesson.solution);

    for (const test of lesson.tests) {
      const testCode = test.code
        ? test.code.replace("{{FUNC}}", userFunctions)
        : lesson.solution;

      let actual = "";
      const originalLog = console.log;
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      console.log = (...args: any[]) => {
        actual +=
          args
            .map((a) =>
              typeof a === "object" && a !== null ? JSON.stringify(a) : String(a)
            )
            .join(" ") + "\n";
      };

      try {
        const fn = new Function("THREE", testCode);
        fn(THREE);
      } catch (_) {
        // Ignore errors (DOM/WebGL calls expected to fail in Node)
      } finally {
        console.log = originalLog;
      }

      results.push({
        course: "threejs",
        lessonId: lesson.id,
        lessonTitle: lesson.title,
        testName: test.name,
        passed: actual.trim() === test.expected.trim(),
        actual,
        expected: test.expected,
      });
    }
  }

  return results;
}
