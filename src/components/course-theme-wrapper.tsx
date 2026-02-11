"use client";

import { usePathname } from "next/navigation";

const THEMED_COURSE_IDS = ["go", "zig", "postgresql"] as const;

export function CourseThemeWrapper({ children }: { children: React.ReactNode }) {
  const pathname = usePathname();
  const segment = pathname?.split("/").filter(Boolean)[0];
  const courseId =
    segment && THEMED_COURSE_IDS.includes(segment as (typeof THEMED_COURSE_IDS)[number])
      ? segment
      : null;

  if (!courseId) {
    return <>{children}</>;
  }

  return (
    <div className={`theme-${courseId} contents`} data-course-theme={courseId}>
      {children}
    </div>
  );
}
