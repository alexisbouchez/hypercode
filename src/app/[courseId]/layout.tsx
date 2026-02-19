import type { Metadata } from "next";
import { courses } from "@/lib/courses";

export function generateStaticParams() {
  return courses.map((c) => ({ courseId: c.id }));
}

export async function generateMetadata({
  params,
}: {
  params: Promise<{ courseId: string }>;
}): Promise<Metadata> {
  const { courseId } = await params;
  const course = courses.find((c) => c.id === courseId);
  if (!course) return {};

  const title = `Learn ${course.title}`;
  const description = course.description;
  const url = `https://hypercode.alexisbouchez.com/${courseId}`;

  return {
    title,
    description,
    openGraph: {
      type: "website",
      siteName: "Hypercode",
      title,
      description,
      url,
      images: [{ url: "/nabla-hypercode.png", width: 1200, height: 630, alt: `Learn ${course.title} on Hypercode` }],
    },
    twitter: {
      card: "summary_large_image",
      title,
      description,
      images: ["/nabla-hypercode.png"],
    },
  };
}

export default function CourseLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return <>{children}</>;
}
