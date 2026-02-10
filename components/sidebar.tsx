"use client";

import { chapters, lessons } from "@/lib/lessons";
import { cn } from "@/lib/utils";
import {
  Sidebar as SidebarUI,
  SidebarContent,
  SidebarFooter,
  SidebarGroup,
  SidebarGroupContent,
  SidebarGroupLabel,
  SidebarHeader,
  SidebarMenu,
  SidebarMenuItem,
  SidebarMenuButton,
} from "./ui/sidebar";

interface SidebarProps {
  currentLessonId: string;
  completedLessons: string[];
  onSelectLesson: (id: string) => void;
}

export function AppSidebar({
  currentLessonId,
  completedLessons,
  onSelectLesson,
}: SidebarProps) {
  return (
    <SidebarUI collapsible="icon">
      <SidebarHeader className="p-4 group-data-[collapsible=icon]:p-1">
        <h1 className="text-lg font-bold tracking-tight text-foreground group-data-[collapsible=icon]:hidden">
          Hypercode
        </h1>
        <p className="text-xs text-muted-foreground mt-0.5 group-data-[collapsible=icon]:hidden">
          Learn the Go programming language
        </p>
      </SidebarHeader>
      <SidebarContent>
        {chapters.map((chapter) => {
          const chapterLessons = lessons.filter(
            (l) => l.chapterId === chapter.id,
          );
          if (chapterLessons.length === 0) return null;

          return (
            <SidebarGroup key={chapter.id} className="group-data-[collapsible=icon]:p-1">
              <SidebarGroupLabel className="uppercase tracking-wider group-data-[collapsible=icon]:hidden">{chapter.title}</SidebarGroupLabel>
              <SidebarGroupContent>
                <SidebarMenu>
                  {chapterLessons.map((lesson) => {
                    const isCurrent = lesson.id === currentLessonId;
                    const isComplete = completedLessons.includes(lesson.id);
                    const globalIndex = lessons.indexOf(lesson);

                    return (
                      <SidebarMenuItem key={lesson.id}>
                        <SidebarMenuButton
                          isActive={isCurrent}
                          onClick={() => onSelectLesson(lesson.id)}
                          className="cursor-pointer data-active:bg-primary/10 data-active:text-primary text-foreground/70"
                        >
                          <span
                            className={cn(
                              "w-5 h-5 rounded-full flex items-center justify-center text-[10px] shrink-0 border",
                              isComplete
                                ? "bg-primary/15 border-primary/30 text-primary"
                                : isCurrent
                                  ? "border-primary/50 text-primary"
                                  : "border-border text-muted-foreground",
                            )}
                          >
                            {isComplete ? (
                              <svg
                                className="!size-2.5"
                                viewBox="0 0 24 24"
                                fill="none"
                                stroke="currentColor"
                                strokeWidth="4"
                                strokeLinecap="round"
                                strokeLinejoin="round"
                              >
                                <path d="M5 13l4 4L19 7" />
                              </svg>
                            ) : (
                              globalIndex + 1
                            )}
                          </span>
                          <span className="truncate">{lesson.title}</span>
                        </SidebarMenuButton>
                      </SidebarMenuItem>
                    );
                  })}
                </SidebarMenu>
              </SidebarGroupContent>
            </SidebarGroup>
          );
        })}
      </SidebarContent>
      <SidebarFooter className="p-4 group-data-[collapsible=icon]:p-1">
        <div className="flex items-center gap-3 group-data-[collapsible=icon]:hidden">
          <div className="flex-1 h-1.5 bg-muted rounded-full overflow-hidden">
            <div
              className="h-full bg-primary rounded-full transition-all duration-300"
              style={{
                width: `${(completedLessons.length / lessons.length) * 100}%`,
              }}
            />
          </div>
          <span className="text-xs tabular-nums text-muted-foreground shrink-0">
            {completedLessons.length}/{lessons.length}
          </span>
        </div>
      </SidebarFooter>
    </SidebarUI>
  );
}
