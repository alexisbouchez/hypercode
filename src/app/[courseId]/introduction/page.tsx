import { notFound } from "next/navigation";
import type { Metadata } from "next";
import { courses, getCourse } from "@/lib/courses";
import { ContentShell } from "@/components/content-shell";

interface IntroductionPageProps {
  params: Promise<{ courseId: string }>;
}

export function generateStaticParams() {
  return courses.map((c) => ({ courseId: c.id }));
}

export async function generateMetadata({
  params,
}: IntroductionPageProps): Promise<Metadata> {
  const { courseId } = await params;
  const course = getCourse(courseId);
  if (!course) return {};

  const title = `Introduction - ${course.title} - Hypercode`;
  const description = `Learn why ${course.title} is worth learning and what this interactive course covers.`;

  return {
    title,
    description,
  };
}

export default async function IntroductionPage({ params }: IntroductionPageProps) {
  const { courseId } = await params;
  const course = getCourse(courseId);
  if (!course) notFound();

  return (
    <ContentShell
      courseId={course.id}
      chapters={course.chapters}
      lessons={course.lessons}
      pdfPath={course.pdfPath}
      title="Introduction"
      content={course.introductionContent}
      activePage="introduction"
      nextHref={`/${course.id}/lessons/${course.lessons[0].id}`}
    />
  );
}
