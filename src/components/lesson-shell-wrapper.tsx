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
import { initSqliteRunner, isSqliteReady, runSqlite, runTests as runSqliteTests } from "@/lib/sqlite-runner";
import { initRedisRunner, isRedisReady, runRedis, runTests as runRedisTests } from "@/lib/redis-runner";
import { initCppRunner, isCppReady, runCpp, runTests as runCppTests } from "@/lib/cpp-runner";
import { initMusicRunner, isMusicReady, runMusic, runTests as runMusicTests } from "@/lib/music-runner";
import { initMysqlRunner, isMysqlReady, runMysql, runTests as runMysqlTests } from "@/lib/mysql-runner";
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
    if (courseId === "calculus") return initCRunner();
    if (courseId === "calculus2") return initCRunner();
    if (courseId === "calculus3") return initCRunner();
    if (courseId === "circuits") return initCRunner();
    if (courseId === "classical-mechanics") return initCRunner();
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
    if (courseId === "quantum") return initPythonRunner();
    if (courseId === "genomics") return initPythonRunner();
    if (courseId === "microgpt") return initPythonRunner();
    if (courseId === "haskell") return initHaskellRunner();
    if (courseId === "sqlite") return initSqliteRunner();
    if (courseId === "redis") return initRedisRunner();
    if (courseId === "cpp") return initCppRunner();
    if (courseId === "raytracer") return initCppRunner();
    if (courseId === "music") return initMusicRunner();
    if (courseId === "waves") return initMusicRunner();
    if (courseId === "electromagnetism") return initPythonRunner();
    if (courseId === "advanced-linear-algebra") return initPythonRunner();
    if (courseId === "advanced-quantum") return initPythonRunner();
    if (courseId === "thermodynamics") return initPythonRunner();
    if (courseId === "special-relativity") return initPythonRunner();
    if (courseId === "fluid-mechanics") return initPythonRunner();
    if (courseId === "general-relativity") return initPythonRunner();
    if (courseId === "nuclear-physics") return initPythonRunner();
    if (courseId === "particle-physics") return initPythonRunner();
    if (courseId === "number-theory") return initPythonRunner();
    if (courseId === "optics") return initPythonRunner();
    if (courseId === "cryptography") return initPythonRunner();
    if (courseId === "cosmology") return initPythonRunner();
    if (courseId === "astrophysics") return initPythonRunner();
    if (courseId === "plasma-physics") return initPythonRunner();
    if (courseId === "condensed-matter") return initPythonRunner();
    if (courseId === "biophysics") return initPythonRunner();
    if (courseId === "mathematical-physics") return initPythonRunner();
    if (courseId === "signal-processing") return initPythonRunner();
    if (courseId === "machine-learning") return initPythonRunner();
    if (courseId === "information-theory") return initPythonRunner();
    if (courseId === "mysql") return initMysqlRunner();
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
    if (courseId === "calculus") return isCReady();
    if (courseId === "calculus2") return isCReady();
    if (courseId === "calculus3") return isCReady();
    if (courseId === "circuits") return isCReady();
    if (courseId === "classical-mechanics") return isCReady();
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
    if (courseId === "quantum") return isPythonReady();
    if (courseId === "genomics") return isPythonReady();
    if (courseId === "microgpt") return isPythonReady();
    if (courseId === "haskell") return isHaskellReady();
    if (courseId === "sqlite") return isSqliteReady();
    if (courseId === "redis") return isRedisReady();
    if (courseId === "cpp") return isCppReady();
    if (courseId === "raytracer") return isCppReady();
    if (courseId === "music") return isMusicReady();
    if (courseId === "waves") return isMusicReady();
    if (courseId === "electromagnetism") return isPythonReady();
    if (courseId === "advanced-linear-algebra") return isPythonReady();
    if (courseId === "advanced-quantum") return isPythonReady();
    if (courseId === "thermodynamics") return isPythonReady();
    if (courseId === "special-relativity") return isPythonReady();
    if (courseId === "fluid-mechanics") return isPythonReady();
    if (courseId === "general-relativity") return isPythonReady();
    if (courseId === "nuclear-physics") return isPythonReady();
    if (courseId === "particle-physics") return isPythonReady();
    if (courseId === "number-theory") return isPythonReady();
    if (courseId === "optics") return isPythonReady();
    if (courseId === "cryptography") return isPythonReady();
    if (courseId === "cosmology") return isPythonReady();
    if (courseId === "astrophysics") return isPythonReady();
    if (courseId === "plasma-physics") return isPythonReady();
    if (courseId === "condensed-matter") return isPythonReady();
    if (courseId === "biophysics") return isPythonReady();
    if (courseId === "mathematical-physics") return isPythonReady();
    if (courseId === "signal-processing") return isPythonReady();
    if (courseId === "machine-learning") return isPythonReady();
    if (courseId === "information-theory") return isPythonReady();
    if (courseId === "mysql") return isMysqlReady();
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
    if (courseId === "calculus") return runC(code);
    if (courseId === "calculus2") return runC(code);
    if (courseId === "calculus3") return runC(code);
    if (courseId === "circuits") return runC(code);
    if (courseId === "classical-mechanics") return runC(code);
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
    if (courseId === "quantum") return runPython(code);
    if (courseId === "genomics") return runPython(code);
    if (courseId === "microgpt") return runPython(code);
    if (courseId === "haskell") return runHaskell(code);
    if (courseId === "sqlite") return runSqlite(code);
    if (courseId === "redis") return runRedis(code);
    if (courseId === "cpp") return runCpp(code);
    if (courseId === "raytracer") return runCpp(code);
    if (courseId === "music") return runMusic(code);
    if (courseId === "waves") return runMusic(code);
    if (courseId === "electromagnetism") return runPython(code);
    if (courseId === "advanced-linear-algebra") return runPython(code);
    if (courseId === "advanced-quantum") return runPython(code);
    if (courseId === "thermodynamics") return runPython(code);
    if (courseId === "special-relativity") return runPython(code);
    if (courseId === "fluid-mechanics") return runPython(code);
    if (courseId === "general-relativity") return runPython(code);
    if (courseId === "nuclear-physics") return runPython(code);
    if (courseId === "particle-physics") return runPython(code);
    if (courseId === "number-theory") return runPython(code);
    if (courseId === "optics") return runPython(code);
    if (courseId === "cryptography") return runPython(code);
    if (courseId === "cosmology") return runPython(code);
    if (courseId === "astrophysics") return runPython(code);
    if (courseId === "plasma-physics") return runPython(code);
    if (courseId === "condensed-matter") return runPython(code);
    if (courseId === "biophysics") return runPython(code);
    if (courseId === "mathematical-physics") return runPython(code);
    if (courseId === "signal-processing") return runPython(code);
    if (courseId === "machine-learning") return runPython(code);
    if (courseId === "information-theory") return runPython(code);
    if (courseId === "mysql") return runMysql(code);
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
    if (courseId === "calculus") return runCTests(code, tests);
    if (courseId === "calculus2") return runCTests(code, tests);
    if (courseId === "calculus3") return runCTests(code, tests);
    if (courseId === "circuits") return runCTests(code, tests);
    if (courseId === "classical-mechanics") return runCTests(code, tests);
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
    if (courseId === "quantum") return runPythonTests(code, tests);
    if (courseId === "genomics") return runPythonTests(code, tests);
    if (courseId === "microgpt") return runPythonTests(code, tests);
    if (courseId === "haskell") return runHaskellTests(code, tests);
    if (courseId === "sqlite") return runSqliteTests(code, tests);
    if (courseId === "redis") return runRedisTests(code, tests);
    if (courseId === "cpp") return runCppTests(code, tests);
    if (courseId === "raytracer") return runCppTests(code, tests);
    if (courseId === "music") return runMusicTests(code, tests);
    if (courseId === "waves") return runMusicTests(code, tests);
    if (courseId === "electromagnetism") return runPythonTests(code, tests);
    if (courseId === "advanced-linear-algebra") return runPythonTests(code, tests);
    if (courseId === "advanced-quantum") return runPythonTests(code, tests);
    if (courseId === "thermodynamics") return runPythonTests(code, tests);
    if (courseId === "special-relativity") return runPythonTests(code, tests);
    if (courseId === "fluid-mechanics") return runPythonTests(code, tests);
    if (courseId === "general-relativity") return runPythonTests(code, tests);
    if (courseId === "nuclear-physics") return runPythonTests(code, tests);
    if (courseId === "particle-physics") return runPythonTests(code, tests);
    if (courseId === "number-theory") return runPythonTests(code, tests);
    if (courseId === "optics") return runPythonTests(code, tests);
    if (courseId === "cryptography") return runPythonTests(code, tests);
    if (courseId === "cosmology") return runPythonTests(code, tests);
    if (courseId === "astrophysics") return runPythonTests(code, tests);
    if (courseId === "plasma-physics") return runPythonTests(code, tests);
    if (courseId === "condensed-matter") return runPythonTests(code, tests);
    if (courseId === "biophysics") return runPythonTests(code, tests);
    if (courseId === "mathematical-physics") return runPythonTests(code, tests);
    if (courseId === "signal-processing") return runPythonTests(code, tests);
    if (courseId === "machine-learning") return runPythonTests(code, tests);
    if (courseId === "information-theory") return runPythonTests(code, tests);
    if (courseId === "mysql") return runMysqlTests(code, tests);
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
