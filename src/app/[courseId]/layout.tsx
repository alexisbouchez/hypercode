import { courses } from "@/lib/courses";

export function generateStaticParams() {
  return courses.map((c) => ({ courseId: c.id }));
}

export default function CourseLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return <>{children}</>;
}
