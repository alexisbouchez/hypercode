"use client";

import { useCallback } from "react";
import type { Lesson, Chapter, RunResult, Test, TestResult } from "@/lib/lessons/types";
import { initGoRunner, isGoReady, runGo, runTests as runGoTests } from "@/lib/go-runner";
import { initZigRunner, isZigReady, runZig, runTests as runZigTests } from "@/lib/zig-runner";
import { initSqlRunner, isSqlReady, runSql, runTests as runSqlTests } from "@/lib/sql-runner";
import { initArm64Runner, isArm64Ready, runArm64, runTests as runArm64Tests } from "@/lib/arm64-runner";
import { initCRunner, isCReady, runC, runTests as runCTests } from "@/lib/c-runner";
import { initGleamRunner, isGleamReady, runGleam, runTests as runGleamTests } from "@/lib/gleam-runner";
import { initRRunner, isRReady, runR, runTests as runRTests } from "@/lib/r-runner";
import { initHolycRunner, isHolycReady, runHolyC, runTests as runHolycTests } from "@/lib/holyc-runner";
import { initLinuxRunner, isLinuxReady, runLinux, runTests as runLinuxTests } from "@/lib/linux-runner";
import { initJsRunner, isJsReady, runJs, runTests as runJsTests } from "@/lib/js-runner";
import { initTsRunner, isTsReady, runTs, runTests as runTsTests } from "@/lib/ts-runner";
import { initRubyRunner, isRubyReady, runRuby, runTests as runRubyTests } from "@/lib/ruby-runner";
import { initThreeJsRunner, isThreeJsReady, runThreeJs, runTests as runThreeJsTests } from "@/lib/threejs-runner";
import { initPythonRunner, isPythonReady, runPython, runTests as runPythonTests } from "@/lib/python-runner";
import { initHaskellRunner, isHaskellReady, runHaskell, runTests as runHaskellTests } from "@/lib/haskell-runner";
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
    if (courseId === "c") return initCRunner();
    if (courseId === "gleam") return initGleamRunner();
    if (courseId === "r") return initRRunner();
    if (courseId === "holyc") return initHolycRunner();
    if (courseId === "coreutils") return initCRunner();
    if (courseId === "trees") return initCRunner();
    if (courseId === "kernel") return initCRunner();
    if (courseId === "linked-lists") return initCRunner();
    if (courseId === "linux") return initLinuxRunner();
    if (courseId === "javascript") return initJsRunner();
    if (courseId === "typescript") return initTsRunner();
    if (courseId === "ruby") return initRubyRunner();
    if (courseId === "algorithms") return initJsRunner();
    if (courseId === "distributed-systems") return initJsRunner();
    if (courseId === "threejs") return initThreeJsRunner();
    if (courseId === "python") return initPythonRunner();
    if (courseId === "graphs") return initPythonRunner();
    if (courseId === "linear-algebra") return initPythonRunner();
    if (courseId === "statistics") return initPythonRunner();
    if (courseId === "diffeq") return initPythonRunner();
    if (courseId === "haskell") return initHaskellRunner();
    return initGoRunner();
  }, [courseId]);

  const isRunnerReady = useCallback((): boolean => {
    if (courseId === "postgresql") return isSqlReady();
    if (courseId === "zig") return isZigReady();
    if (courseId === "arm64") return isArm64Ready();
    if (courseId === "c") return isCReady();
    if (courseId === "gleam") return isGleamReady();
    if (courseId === "r") return isRReady();
    if (courseId === "holyc") return isHolycReady();
    if (courseId === "coreutils") return isCReady();
    if (courseId === "trees") return isCReady();
    if (courseId === "kernel") return isCReady();
    if (courseId === "linked-lists") return isCReady();
    if (courseId === "linux") return isLinuxReady();
    if (courseId === "javascript") return isJsReady();
    if (courseId === "typescript") return isTsReady();
    if (courseId === "ruby") return isRubyReady();
    if (courseId === "algorithms") return isJsReady();
    if (courseId === "distributed-systems") return isJsReady();
    if (courseId === "threejs") return isThreeJsReady();
    if (courseId === "python") return isPythonReady();
    if (courseId === "graphs") return isPythonReady();
    if (courseId === "linear-algebra") return isPythonReady();
    if (courseId === "statistics") return isPythonReady();
    if (courseId === "diffeq") return isPythonReady();
    if (courseId === "haskell") return isHaskellReady();
    return isGoReady();
  }, [courseId]);

  const runCode = useCallback(async (code: string): Promise<RunResult> => {
    if (courseId === "postgresql") return runSql(code);
    if (courseId === "zig") return runZig(code);
    if (courseId === "arm64") return runArm64(code);
    if (courseId === "c") return runC(code);
    if (courseId === "gleam") return runGleam(code);
    if (courseId === "r") return runR(code);
    if (courseId === "holyc") return runHolyC(code);
    if (courseId === "coreutils") return runC(code);
    if (courseId === "trees") return runC(code);
    if (courseId === "kernel") return runC(code);
    if (courseId === "linked-lists") return runC(code);
    if (courseId === "linux") return runLinux(code);
    if (courseId === "javascript") return runJs(code);
    if (courseId === "typescript") return runTs(code);
    if (courseId === "ruby") return runRuby(code);
    if (courseId === "algorithms") return runJs(code);
    if (courseId === "distributed-systems") return runJs(code);
    if (courseId === "threejs") return runThreeJs(code);
    if (courseId === "python") return runPython(code);
    if (courseId === "graphs") return runPython(code);
    if (courseId === "linear-algebra") return runPython(code);
    if (courseId === "statistics") return runPython(code);
    if (courseId === "diffeq") return runPython(code);
    if (courseId === "haskell") return runHaskell(code);
    return runGo(code);
  }, [courseId]);

  const runTestsFn = useCallback(async (code: string, tests: Test[]): Promise<TestResult[]> => {
    if (courseId === "postgresql") return runSqlTests(code, tests);
    if (courseId === "zig") return runZigTests(code, tests);
    if (courseId === "arm64") return runArm64Tests(code, tests);
    if (courseId === "c") return runCTests(code, tests);
    if (courseId === "gleam") return runGleamTests(code, tests);
    if (courseId === "r") return runRTests(code, tests);
    if (courseId === "holyc") return runHolycTests(code, tests);
    if (courseId === "coreutils") return runCTests(code, tests);
    if (courseId === "trees") return runCTests(code, tests);
    if (courseId === "kernel") return runCTests(code, tests);
    if (courseId === "linked-lists") return runCTests(code, tests);
    if (courseId === "linux") return runLinuxTests(code, tests);
    if (courseId === "javascript") return runJsTests(code, tests);
    if (courseId === "typescript") return runTsTests(code, tests);
    if (courseId === "ruby") return runRubyTests(code, tests);
    if (courseId === "algorithms") return runJsTests(code, tests);
    if (courseId === "distributed-systems") return runJsTests(code, tests);
    if (courseId === "threejs") return runThreeJsTests(code, tests);
    if (courseId === "python") return runPythonTests(code, tests);
    if (courseId === "graphs") return runPythonTests(code, tests);
    if (courseId === "linear-algebra") return runPythonTests(code, tests);
    if (courseId === "statistics") return runPythonTests(code, tests);
    if (courseId === "diffeq") return runPythonTests(code, tests);
    if (courseId === "haskell") return runHaskellTests(code, tests);
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
