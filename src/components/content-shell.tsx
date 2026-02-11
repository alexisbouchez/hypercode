"use client";

import { useEffect, useState } from "react";
import Link from "next/link";
import { loadProgress } from "@/lib/progress";
import { AppSidebar } from "./sidebar";
import { LessonContent } from "./lesson-content";
import { Button } from "./ui/button";
import { ThemeToggle } from "./theme-toggle";
import { SidebarProvider, SidebarInset, SidebarTrigger } from "./ui/sidebar";

interface ContentShellProps {
  title: string;
  content: string;
  activePage: string;
  prevHref?: string;
  nextHref?: string;
}

export function ContentShell({
  title,
  content,
  activePage,
  prevHref,
  nextHref,
}: ContentShellProps) {
  const [completedLessons, setCompletedLessons] = useState<string[]>([]);

  useEffect(() => {
    const progress = loadProgress();
    setCompletedLessons(progress.completedLessons);
  }, []);

  return (
    <SidebarProvider className="h-screen !min-h-0">
      <AppSidebar
        currentLessonId={activePage}
        completedLessons={completedLessons}
      />
      <SidebarInset className="overflow-hidden h-full">
        <div className="h-full flex flex-col overflow-hidden">
          <div className="px-4 py-2 border-b border-border flex items-center gap-2 shrink-0">
            <SidebarTrigger />
            <div className="flex-1" />
            <ThemeToggle />
          </div>
          <div className="flex-1 overflow-y-auto px-6 py-6">
            <div className="max-w-3xl mx-auto w-full">
              <div className="mb-6">
                <h1 className="text-3xl font-bold tracking-tight text-foreground font-display">
                  {title}
                </h1>
              </div>
              <LessonContent content={content} />
            </div>
          </div>
          <div className="px-6 py-3 border-t border-border flex items-center justify-between shrink-0">
            {prevHref ? (
              <Button
                variant="ghost"
                size="sm"
                nativeButton={false}
                render={<Link href={prevHref} />}
              >
                &larr; Previous
              </Button>
            ) : (
              <Button variant="ghost" size="sm" disabled>
                &larr; Previous
              </Button>
            )}
            {nextHref ? (
              <Button
                variant="ghost"
                size="sm"
                nativeButton={false}
                render={<Link href={nextHref} />}
              >
                Next &rarr;
              </Button>
            ) : (
              <Button variant="ghost" size="sm" disabled>
                Next &rarr;
              </Button>
            )}
          </div>
        </div>
      </SidebarInset>
    </SidebarProvider>
  );
}
