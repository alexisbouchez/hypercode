import type { MetadataRoute } from "next";
import { courses } from "@/lib/courses";

export default function sitemap(): MetadataRoute.Sitemap {
  const base = "https://hypercode.alexisbouchez.com";

  const entries: MetadataRoute.Sitemap = [
    {
      url: base,
      changeFrequency: "monthly",
      priority: 1.0,
    },
  ];

  for (const course of courses) {
    entries.push({
      url: `${base}/${course.id}/introduction`,
      changeFrequency: "monthly",
      priority: 0.9,
    });

    for (const lesson of course.lessons) {
      entries.push({
        url: `${base}/${course.id}/lessons/${lesson.id}`,
        changeFrequency: "monthly",
        priority: 0.8,
      });
    }

    entries.push({
      url: `${base}/${course.id}/whats-next`,
      changeFrequency: "monthly",
      priority: 0.7,
    });
  }

  return entries;
}
