"use client";

import { useState, useEffect, useCallback } from "react";
import Link from "next/link";
import type { Lesson, Chapter, TestResult, RunResult, Test } from "@/lib/lessons/types";
import {
  loadProgress,
  setCurrentLesson,
  saveCode,
  getSavedCode,
  markCompleted,
  unmarkCompleted,
} from "@/lib/progress";
import { EXERCISE_SCHEMA_SUMMARY } from "@/lib/sql-shared";
import { AppSidebar } from "./sidebar";
import { LessonContent } from "./lesson-content";
import { CodeEditor } from "./code-editor";
import { OutputPanel } from "./output-panel";
import { Button } from "./ui/button";
import { Switch } from "@base-ui/react/switch";
import { SidebarProvider, SidebarInset, SidebarTrigger } from "./ui/sidebar";
import {
  ResizablePanelGroup,
  ResizablePanel,
  ResizableHandle,
} from "./ui/resizable";
import { ThemeToggle } from "./theme-toggle";
import { SchemaDrawer } from "./schema-drawer";

export interface LessonShellProps {
  courseId: string;
  language: string;
  runtimeLabel: string;
  pdfPath?: string;
  lesson: Lesson;
  lessons: Lesson[];
  chapters: Chapter[];
  initRunner: () => Promise<void>;
  isRunnerReady: () => boolean;
  runCode: (code: string) => Promise<RunResult>;
  runTests: (code: string, tests: Test[]) => Promise<TestResult[]>;
}

function useIsMac() {
  const [isMac, setIsMac] = useState(false);
  useEffect(() => {
    setIsMac(navigator.platform.startsWith("Mac"));
  }, []);
  return isMac;
}

export function LessonShell({
  courseId,
  language,
  runtimeLabel,
  pdfPath,
  lesson,
  lessons,
  chapters,
  initRunner,
  isRunnerReady,
  runCode,
  runTests,
}: LessonShellProps) {
  const isMac = useIsMac();
  const mod = isMac ? "⌘" : "Ctrl+";
  const currentLesson = lesson;
  const currentIndex = lessons.findIndex((l) => l.id === currentLesson.id);
  const hasPrev = currentIndex > 0;
  const hasNext = currentIndex < lessons.length - 1;

  const [code, setCode] = useState(currentLesson.starterCode);
  const [output, setOutput] = useState("");
  const [error, setError] = useState("");
  const [testResults, setTestResults] = useState<TestResult[]>([]);
  const [isRunning, setIsRunning] = useState(false);
  const [showSolution, setShowSolution] = useState(false);
  const [completedLessons, setCompletedLessons] = useState<string[]>([]);
  const [runtimeReady, setRuntimeReady] = useState(false);
  const [runtimeError, setRuntimeError] = useState<string | null>(null);
  const [showSchema, setShowSchema] = useState(false);
  const [readMode, setReadMode] = useState(() => {
    if (typeof window === "undefined") return false;
    return localStorage.getItem("hypercode-read-mode") === "true";
  });

  // Load progress on mount and restore saved code
  useEffect(() => {
    const progress = loadProgress(courseId);
    setCompletedLessons(progress.completedLessons);
    setCurrentLesson(courseId, currentLesson.id);

    const saved = getSavedCode(courseId, currentLesson.id);
    if (saved) {
      setCode(saved);
    } else {
      setCode(currentLesson.starterCode);
    }

    // Reset editor state on lesson change
    setOutput("");
    setError("");
    setTestResults([]);
    setShowSolution(false);
  }, [courseId, currentLesson.id, currentLesson.starterCode]);

  // Initialize runner (skip in read mode)
  useEffect(() => {
    if (readMode) return;
    setRuntimeError(null);
    initRunner()
      .then(() => {
        setRuntimeReady(isRunnerReady());
        setRuntimeError(null);
      })
      .catch((err) => {
        setRuntimeReady(false);
        setRuntimeError(err instanceof Error ? err.message : String(err));
      });
  }, [readMode, initRunner, isRunnerReady]);

  const toggleReadMode = useCallback(() => {
    setReadMode((prev) => {
      const next = !prev;
      localStorage.setItem("hypercode-read-mode", String(next));
      return next;
    });
  }, []);

  const handleRun = useCallback(async () => {
    setIsRunning(true);
    setShowSolution(false);

    saveCode(courseId, currentLesson.id, code);

    const result = await runCode(code);
    setOutput(result.stdout);
    setError(result.error || result.stderr);

    const results = await runTests(code, currentLesson.tests);
    setTestResults(results);

    const allPassed = results.every((t) => t.passed);
    if (allPassed) {
      markCompleted(courseId, currentLesson.id);
      setCompletedLessons((prev) =>
        prev.includes(currentLesson.id) ? prev : [...prev, currentLesson.id],
      );
    }

    setIsRunning(false);
  }, [courseId, code, currentLesson.id, currentLesson.tests, runCode, runTests]);

  const handleViewSolution = useCallback(() => {
    setShowSolution((prev) => !prev);
  }, []);

  const handleResetCode = useCallback(() => {
    setCode(currentLesson.starterCode);
    setOutput("");
    setError("");
    setTestResults([]);
    setShowSolution(false);
  }, [currentLesson.starterCode]);

  // Save code before navigating away
  const handleBeforeNavigate = useCallback(() => {
    saveCode(courseId, currentLesson.id, code);
  }, [courseId, currentLesson.id, code]);

  const showDiff = showSolution;

  const isCurrentCompleted = completedLessons.includes(currentLesson.id);

  const handleToggleComplete = useCallback(() => {
    if (isCurrentCompleted) {
      unmarkCompleted(courseId, currentLesson.id);
      setCompletedLessons((prev) => prev.filter((id) => id !== currentLesson.id));
    } else {
      markCompleted(courseId, currentLesson.id);
      setCompletedLessons((prev) => [...prev, currentLesson.id]);
    }
  }, [courseId, currentLesson.id, isCurrentCompleted]);

  const topBar = (
    <div className="px-4 py-2 border-b border-border flex items-center gap-2 shrink-0">
      <SidebarTrigger />
      <div className="text-xs font-medium text-muted-foreground">
        Lesson {currentIndex + 1} of {lessons.length}
      </div>
      <div className="flex-1" />
      {readMode && (
        <Button
          variant={isCurrentCompleted ? "ghost" : "outline"}
          size="sm"
          onClick={handleToggleComplete}
        >
          {isCurrentCompleted ? "Completed" : "Mark as complete"}
        </Button>
      )}
      {readMode && <ThemeToggle />}
      <label className="flex items-center gap-2 cursor-pointer select-none">
        <span className="text-xs text-muted-foreground">Interactive</span>
        <Switch.Root
          checked={readMode}
          onCheckedChange={toggleReadMode}
          className="relative inline-flex h-5 w-9 shrink-0 items-center rounded-full border border-border bg-muted transition-colors data-[checked]:bg-primary"
        >
          <Switch.Thumb className="pointer-events-none block h-4 w-4 rounded-full bg-background shadow-sm transition-transform translate-x-0 data-[checked]:translate-x-4" />
        </Switch.Root>
        <span className="text-xs text-muted-foreground">Read-only</span>
      </label>
    </div>
  );

  const navigation = (
    <div className="px-6 py-3 border-t border-border flex items-center justify-between shrink-0">
      {hasPrev ? (
        <Button
          variant="ghost"
          size="sm"
          nativeButton={false}
          render={<Link href={`/${courseId}/lessons/${lessons[currentIndex - 1].id}`} />}
          onClick={handleBeforeNavigate}
        >
          &larr; Previous
        </Button>
      ) : (
        <Button
          variant="ghost"
          size="sm"
          nativeButton={false}
          render={<Link href={`/${courseId}/introduction`} />}
          onClick={handleBeforeNavigate}
        >
          &larr; Previous
        </Button>
      )}
      {hasNext ? (
        <Button
          variant="ghost"
          size="sm"
          nativeButton={false}
          render={<Link href={`/${courseId}/lessons/${lessons[currentIndex + 1].id}`} />}
          onClick={handleBeforeNavigate}
        >
          Next &rarr;
        </Button>
      ) : (
        <Button
          variant="ghost"
          size="sm"
          nativeButton={false}
          render={<Link href={`/${courseId}/whats-next`} />}
          onClick={handleBeforeNavigate}
        >
          Next &rarr;
        </Button>
      )}
    </div>
  );

  const contentPanel = (
    <div className="h-full flex flex-col overflow-hidden">
      {topBar}
      <div className="flex-1 overflow-y-auto px-6 py-6">
        <div className={readMode ? "max-w-3xl mx-auto w-full" : undefined}>
          <div className="mb-6">
            <h1 className="text-3xl font-bold tracking-tight text-foreground font-display">
              {currentLesson.title}
            </h1>
          </div>
          <LessonContent content={currentLesson.content} />
        </div>
      </div>
      {navigation}
    </div>
  );

  if (readMode) {
    return (
      <SidebarProvider className="h-screen !min-h-0">
        <AppSidebar
          courseId={courseId}
          chapters={chapters}
          lessons={lessons}
          pdfPath={pdfPath}
          currentLessonId={currentLesson.id}
          completedLessons={completedLessons}
        />
        <SidebarInset className="overflow-hidden h-full">
          {contentPanel}
        </SidebarInset>
      </SidebarProvider>
    );
  }

  return (
    <SidebarProvider className="h-screen !min-h-0">
      <AppSidebar
        courseId={courseId}
        chapters={chapters}
        lessons={lessons}
        pdfPath={pdfPath}
        currentLessonId={currentLesson.id}
        completedLessons={completedLessons}
      />
      <SidebarInset className="overflow-hidden h-full">
        <ResizablePanelGroup orientation="horizontal">
          {/* Left panel: lesson content */}
          <ResizablePanel defaultSize="35" minSize="20" maxSize="50">
            {contentPanel}
          </ResizablePanel>

          <ResizableHandle withHandle />

          {/* Right panel: toolbar + editor/output vertical split */}
          <ResizablePanel defaultSize="65" minSize="30">
            <div className="h-full flex flex-col overflow-hidden">
              {/* Toolbar */}
              <div className="px-4 py-2 border-b border-border flex items-center gap-2 bg-card shrink-0">
                <Button size="sm" onClick={handleRun} disabled={isRunning}>
                  {isRunning ? "Running..." : "Run"}
                  <kbd className="ml-1.5 text-[10px] opacity-60 font-sans">{mod}↵</kbd>
                </Button>
                <Button
                  variant={showSolution ? "secondary" : "outline"}
                  size="sm"
                  onClick={handleViewSolution}
                >
                  {showSolution ? "Hide Diff" : "Compare Solution"}
                </Button>
                {courseId === "postgresql" && (
                  <Button
                    variant={showSchema ? "secondary" : "outline"}
                    size="sm"
                    onClick={() => setShowSchema((v) => !v)}
                  >
                    {showSchema ? "Hide Tables" : "Show Tables"}
                  </Button>
                )}
                <Button
                  variant="ghost"
                  size="sm"
                  onClick={handleResetCode}
                >
                  Reset
                </Button>
                <div className="flex-1" />
                {!runtimeReady && !runtimeError && (
                  <span className="text-xs text-muted-foreground">
                    {runtimeLabel} loading...
                  </span>
                )}
                {runtimeError && (
                  <span className="text-xs text-destructive" title={runtimeError}>
                    {runtimeLabel} failed to load
                  </span>
                )}
                {runtimeReady && (
                  <span className="text-xs text-primary">
                    {runtimeLabel} ready
                  </span>
                )}
                <ThemeToggle />
              </div>

              {/* Editor + Output: vertical resizable split */}
              <div className="flex-1 min-h-0 relative">
                <SchemaDrawer open={showSchema} onClose={() => setShowSchema(false)} />
                <ResizablePanelGroup orientation="vertical">
                  <ResizablePanel defaultSize="70" minSize="20">
                    <div className="h-full overflow-hidden">
                      <CodeEditor
                        value={code}
                        onChange={(v) => setCode(v)}
                        language={language}
                        solution={showDiff ? currentLesson.solution : undefined}
                        onRun={handleRun}
                      />
                    </div>
                  </ResizablePanel>

                  <ResizableHandle withHandle />

                  <ResizablePanel defaultSize="30" minSize="10">
                    <div className="h-full bg-muted overflow-auto">
                      <OutputPanel
                        output={output}
                        error={error}
                        testResults={testResults}
                        isRunning={isRunning}
                        schemaReference={
                          courseId === "postgresql" ? (
                            <div className="text-xs">
                              <div className="text-muted-foreground font-semibold mb-2">
                                Available tables
                              </div>
                              <div className="space-y-1.5 text-foreground/90">
                                {EXERCISE_SCHEMA_SUMMARY.map(({ table, columns }) => (
                                  <div key={table}>
                                    <span className="font-semibold text-foreground">
                                      {table}
                                    </span>
                                    <span className="text-muted-foreground">
                                      {" "}({columns.join(", ")})
                                    </span>
                                  </div>
                                ))}
                              </div>
                            </div>
                          ) : undefined
                        }
                      />
                    </div>
                  </ResizablePanel>
                </ResizablePanelGroup>
              </div>
            </div>
          </ResizablePanel>
        </ResizablePanelGroup>
      </SidebarInset>
    </SidebarProvider>
  );
}
