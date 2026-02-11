"use client";

import Image from "next/image";
import Link from "next/link";
import { chapters, lessons } from "@/lib/lessons";
import { cn } from "@/lib/utils";
import { buttonVariants } from "./ui/button";
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
import logo from "../logo.png"

interface SidebarProps {
  currentLessonId: string;
  completedLessons: string[];
}

export function AppSidebar({
  currentLessonId,
  completedLessons,
}: SidebarProps) {
  return (
    <SidebarUI collapsible="icon">
      <SidebarHeader className="px-4 pt-3 pb-2 group-data-[collapsible=icon]:p-1">
        <Image
          src={logo}
          alt="Hypercode"
          width={163}
          height={36}
          className="group-data-[collapsible=icon]:hidden"
          priority
        />

        <div className="flex items-center gap-3 group-data-[collapsible=icon]:hidden">
          <span className="text-xs text-muted-foreground shrink-0">Progress:</span>
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
      </SidebarHeader>
      <SidebarContent>
        <SidebarGroup className="group-data-[collapsible=icon]:p-1">
          <SidebarGroupContent>
            <SidebarMenu>
              <SidebarMenuItem>
                <SidebarMenuButton
                  render={<Link href="/introduction" />}
                  isActive={currentLessonId === "introduction"}
                  className="cursor-pointer data-active:bg-primary/10 data-active:text-primary text-foreground/70"
                >
                  <span
                    className={cn(
                      "w-5 h-5 rounded-full flex items-center justify-center text-[10px] shrink-0 border",
                      currentLessonId === "introduction"
                        ? "border-primary/50 text-primary"
                        : "border-border text-muted-foreground",
                    )}
                  >
                    <svg className="size-2.5!" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2.5" strokeLinecap="round" strokeLinejoin="round">
                      <path d="M4 19.5A2.5 2.5 0 016.5 17H20" />
                      <path d="M6.5 2H20v20H6.5A2.5 2.5 0 014 19.5v-15A2.5 2.5 0 016.5 2z" />
                    </svg>
                  </span>
                  <span className="truncate">Introduction</span>
                </SidebarMenuButton>
              </SidebarMenuItem>
            </SidebarMenu>
          </SidebarGroupContent>
        </SidebarGroup>
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
                          render={<Link href={`/lessons/${lesson.id}`} />}
                          isActive={isCurrent}
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
        <SidebarGroup className="group-data-[collapsible=icon]:p-1">
          <SidebarGroupContent>
            <SidebarMenu>
              <SidebarMenuItem>
                <SidebarMenuButton
                  render={<Link href="/whats-next" />}
                  isActive={currentLessonId === "whats-next"}
                  className="cursor-pointer data-active:bg-primary/10 data-active:text-primary text-foreground/70"
                >
                  <span
                    className={cn(
                      "w-5 h-5 rounded-full flex items-center justify-center text-[10px] shrink-0 border",
                      currentLessonId === "whats-next"
                        ? "border-primary/50 text-primary"
                        : "border-border text-muted-foreground",
                    )}
                  >
                    <svg className="size-2.5!" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2.5" strokeLinecap="round" strokeLinejoin="round">
                      <path d="M4 15s1-1 4-1 5 2 8 2 4-1 4-1V3s-1 1-4 1-5-2-8-2-4 1-4 1z" />
                      <line x1="4" y1="22" x2="4" y2="15" />
                    </svg>
                  </span>
                  <span className="truncate">What's Next?</span>
                </SidebarMenuButton>
              </SidebarMenuItem>
            </SidebarMenu>
          </SidebarGroupContent>
        </SidebarGroup>
      </SidebarContent>
      <SidebarFooter className="p-4 group-data-[collapsible=icon]:p-1">
        <div className="grid grid-cols-2 gap-2 group-data-[collapsible=icon]:hidden">
          <a
            href="/go-book.pdf"
            download
            className={cn(buttonVariants({ variant: "outline", size: "sm" }), "col-span-2 w-full")}
          >
            <svg role="img" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg" className="size-4! fill-current"><path d="M12 16l-5-5h3V4h4v7h3l-5 5zm-7 2h14v2H5v-2z"/></svg>
            Download PDF
          </a>
          <a
            href="https://github.com/alexisbouchez/hypercode"
            target="_blank"
            rel="noopener noreferrer"
            className={cn(buttonVariants({ variant: "outline", size: "sm" }), "w-full")}
          >
            <svg role="img" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg" className="!size-4 fill-current"><path d="M12 .297c-6.63 0-12 5.373-12 12 0 5.303 3.438 9.8 8.205 11.385.6.113.82-.258.82-.577 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61C4.422 18.07 3.633 17.7 3.633 17.7c-1.087-.744.084-.729.084-.729 1.205.084 1.838 1.236 1.838 1.236 1.07 1.835 2.809 1.305 3.495.998.108-.776.417-1.305.76-1.605-2.665-.3-5.466-1.332-5.466-5.93 0-1.31.465-2.38 1.235-3.22-.135-.303-.54-1.523.105-3.176 0 0 1.005-.322 3.3 1.23.96-.267 1.98-.399 3-.405 1.02.006 2.04.138 3 .405 2.28-1.552 3.285-1.23 3.285-1.23.645 1.653.24 2.873.12 3.176.765.84 1.23 1.91 1.23 3.22 0 4.61-2.805 5.625-5.475 5.92.42.36.81 1.096.81 2.22 0 1.606-.015 2.896-.015 3.286 0 .315.21.69.825.57C20.565 22.092 24 17.592 24 12.297c0-6.627-5.373-12-12-12"/></svg>
            GitHub
          </a>
          <a
            href="https://discord.gg/ekrFAtS6Bj"
            target="_blank"
            rel="noopener noreferrer"
            className={cn(buttonVariants({ variant: "outline", size: "sm" }), "w-full")}
          >
            <svg role="img" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg" className="size-4! fill-current"><path d="M20.317 4.3698a19.7913 19.7913 0 00-4.8851-1.5152.0741.0741 0 00-.0785.0371c-.211.3753-.4447.8648-.6083 1.2495-1.8447-.2762-3.68-.2762-5.4868 0-.1636-.3933-.4058-.8742-.6177-1.2495a.077.077 0 00-.0785-.037 19.7363 19.7363 0 00-4.8852 1.515.0699.0699 0 00-.0321.0277C.5334 9.0458-.319 13.5799.0992 18.0578a.0824.0824 0 00.0312.0561c2.0528 1.5076 4.0413 2.4228 5.9929 3.0294a.0777.0777 0 00.0842-.0276c.4616-.6304.8731-1.2952 1.226-1.9942a.076.076 0 00-.0416-.1057c-.6528-.2476-1.2743-.5495-1.8722-.8923a.077.077 0 01-.0076-.1277c.1258-.0943.2517-.1923.3718-.2914a.0743.0743 0 01.0776-.0105c3.9278 1.7933 8.18 1.7933 12.0614 0a.0739.0739 0 01.0785.0095c.1202.099.246.1981.3728.2924a.077.077 0 01-.0066.1276 12.2986 12.2986 0 01-1.873.8914.0766.0766 0 00-.0407.1067c.3604.698.7719 1.3628 1.225 1.9932a.076.076 0 00.0842.0286c1.961-.6067 3.9495-1.5219 6.0023-3.0294a.077.077 0 00.0313-.0552c.5004-5.177-.8382-9.6739-3.5485-13.6604a.061.061 0 00-.0312-.0286zM8.02 15.3312c-1.1825 0-2.1569-1.0857-2.1569-2.419 0-1.3332.9555-2.4189 2.157-2.4189 1.2108 0 2.1757 1.0952 2.1568 2.419 0 1.3332-.9555 2.4189-2.1569 2.4189zm7.9748 0c-1.1825 0-2.1569-1.0857-2.1569-2.419 0-1.3332.9554-2.4189 2.1569-2.4189 1.2108 0 2.1757 1.0952 2.1568 2.419 0 1.3332-.946 2.4189-2.1568 2.4189Z"/></svg>
            Discord
          </a>
        </div>
      </SidebarFooter>
    </SidebarUI>
  );
}
