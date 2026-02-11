"use client";

import { useState, useEffect, useCallback } from "react";
import Link from "next/link";
import type { Lesson, Chapter, TestResult } from "@/lib/lessons/types";
import { initGoRunner, runGo, runTests, isGoReady } from "@/lib/go-runner";
import {
  loadProgress,
  setCurrentLesson,
  saveCode,
  getSavedCode,
  markCompleted,
  unmarkCompleted,
} from "@/lib/progress";
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

interface LessonShellProps {
  lesson: Lesson;
  lessons: Lesson[];
  chapters: Chapter[];
}

export function LessonShell({ lesson, lessons, chapters }: LessonShellProps) {
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
  const [goReady, setGoReady] = useState(false);
  const [readMode, setReadMode] = useState(() => {
    if (typeof window === "undefined") return false;
    return localStorage.getItem("hypercode-read-mode") === "true";
  });

  // Load progress on mount and restore saved code
  useEffect(() => {
    const progress = loadProgress();
    setCompletedLessons(progress.completedLessons);
    setCurrentLesson(currentLesson.id);

    const saved = getSavedCode(currentLesson.id);
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
  }, [currentLesson.id, currentLesson.starterCode]);

  // Initialize Go runner (skip in read mode)
  useEffect(() => {
    if (readMode) return;
    initGoRunner().then(() => {
      setGoReady(isGoReady());
    });
  }, [readMode]);

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

    saveCode(currentLesson.id, code);

    const result = await runGo(code);
    setOutput(result.stdout);
    setError(result.error || result.stderr);

    const results = await runTests(code, currentLesson.tests);
    setTestResults(results);

    const allPassed = results.every((t) => t.passed);
    if (allPassed) {
      markCompleted(currentLesson.id);
      setCompletedLessons((prev) =>
        prev.includes(currentLesson.id) ? prev : [...prev, currentLesson.id],
      );
    }

    setIsRunning(false);
  }, [code, currentLesson.id, currentLesson.tests]);

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
    saveCode(currentLesson.id, code);
  }, [currentLesson.id, code]);

  const showDiff = showSolution;

  const isCurrentCompleted = completedLessons.includes(currentLesson.id);

  const handleToggleComplete = useCallback(() => {
    if (isCurrentCompleted) {
      unmarkCompleted(currentLesson.id);
      setCompletedLessons((prev) => prev.filter((id) => id !== currentLesson.id));
    } else {
      markCompleted(currentLesson.id);
      setCompletedLessons((prev) => [...prev, currentLesson.id]);
    }
  }, [currentLesson.id, isCurrentCompleted]);

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
          render={<Link href={`/lessons/${lessons[currentIndex - 1].id}`} />}
          onClick={handleBeforeNavigate}
        >
          Previous
        </Button>
      ) : (
        <Button
          variant="ghost"
          size="sm"
          nativeButton={false}
          render={<Link href="/introduction" />}
          onClick={handleBeforeNavigate}
        >
          Previous
        </Button>
      )}
      {hasNext ? (
        <Button
          variant="ghost"
          size="sm"
          nativeButton={false}
          render={<Link href={`/lessons/${lessons[currentIndex + 1].id}`} />}
          onClick={handleBeforeNavigate}
        >
          Next
        </Button>
      ) : (
        <Button
          variant="ghost"
          size="sm"
          nativeButton={false}
          render={<Link href="/whats-next" />}
          onClick={handleBeforeNavigate}
        >
          Next
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
                </Button>
                <Button
                  variant={showSolution ? "secondary" : "outline"}
                  size="sm"
                  onClick={handleViewSolution}
                >
                  {showSolution ? "Hide Diff" : "Compare Solution"}
                </Button>
                <Button
                  variant="ghost"
                  size="sm"
                  onClick={handleResetCode}
                >
                  Reset
                </Button>
                <div className="flex-1" />
                {!goReady && (
                  <span className="text-xs text-muted-foreground">
                    Go runtime loading...
                  </span>
                )}
                {goReady && (
                  <span className="text-xs text-primary">
                    Go runtime ready
                  </span>
                )}
                <ThemeToggle />
              </div>

              {/* Editor + Output: vertical resizable split */}
              <div className="flex-1 min-h-0">
                <ResizablePanelGroup orientation="vertical">
                  <ResizablePanel defaultSize="70" minSize="20">
                    <div className="h-full overflow-hidden">
                      <CodeEditor
                        value={code}
                        onChange={(v) => setCode(v)}
                        solution={showDiff ? currentLesson.solution : undefined}
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
