"use client";

import { useCallback } from "react";
import type { Lesson, Chapter, RunResult, Test, TestResult } from "@/lib/lessons/types";
import { initGoRunner, isGoReady, runGo, runTests as runGoTests } from "@/lib/go-runner";
import { initZigRunner, isZigReady, runZig, runTests as runZigTests } from "@/lib/zig-runner";
import { initSqlRunner, isSqlReady, runSql, runTests as runSqlTests } from "@/lib/sql-runner";
import { initArm64Runner, isArm64Ready, runArm64, runTests as runArm64Tests } from "@/lib/arm64-runner";
import { LessonShell } from "./lesson-shell";

interface LessonShellWrapperProps {
  courseId: string;
  language: string;
  runtimeLabel: string;
  pdfPath?: string;
  lesson: Lesson;
  lessons: Lesson[];
  chapters: Chapter[];
}

export function LessonShellWrapper({
  courseId,
  language,
  runtimeLabel,
  pdfPath,
  lesson,
  lessons,
  chapters,
}: LessonShellWrapperProps) {
  const initRunner = useCallback((): Promise<void> => {
    if (courseId === "postgresql") return initSqlRunner();
    if (courseId === "zig") return initZigRunner();
    if (courseId === "arm64") return initArm64Runner();
    return initGoRunner();
  }, [courseId]);

  const isRunnerReady = useCallback((): boolean => {
    if (courseId === "postgresql") return isSqlReady();
    if (courseId === "zig") return isZigReady();
    if (courseId === "arm64") return isArm64Ready();
    return isGoReady();
  }, [courseId]);

  const runCode = useCallback(async (code: string): Promise<RunResult> => {
    if (courseId === "postgresql") return runSql(code);
    if (courseId === "zig") return runZig(code);
    if (courseId === "arm64") return runArm64(code);
    return runGo(code);
  }, [courseId]);

  const runTestsFn = useCallback(async (code: string, tests: Test[]): Promise<TestResult[]> => {
    if (courseId === "postgresql") return runSqlTests(code, tests);
    if (courseId === "zig") return runZigTests(code, tests);
    if (courseId === "arm64") return runArm64Tests(code, tests);
    return runGoTests(code, tests);
  }, [courseId]);

  return (
    <LessonShell
      courseId={courseId}
      language={language}
      runtimeLabel={runtimeLabel}
      pdfPath={pdfPath}
      lesson={lesson}
      lessons={lessons}
      chapters={chapters}
      initRunner={initRunner}
      isRunnerReady={isRunnerReady}
      runCode={runCode}
      runTests={runTestsFn}
    />
  );
}
