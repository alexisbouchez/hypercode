import Link from "next/link";
import Image from "next/image";
import { courses } from "@/lib/courses";
import { ThemeToggle } from "@/components/theme-toggle";
import logo from "../logo.png";

export default function HomePage() {
  return (
    <div className="min-h-screen bg-background">
      <header className="border-b border-border">
        <div className="max-w-5xl mx-auto px-6 py-4 flex items-center justify-between">
          <Image src={logo} alt="Hypercode" width={163} height={36} priority />
          <ThemeToggle />
        </div>
      </header>

      <main className="max-w-5xl mx-auto px-6 py-16">
        <div className="text-center mb-12">
          <h1 className="text-4xl font-bold tracking-tight text-foreground font-display mb-4">
            Learn Programming Interactively
          </h1>
          <p className="text-lg text-muted-foreground max-w-2xl mx-auto">
            Master programming languages with hands-on exercises that run directly in your browser. No account needed.
          </p>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-2 gap-6 max-w-3xl mx-auto">
          {courses.map((course) => (
            <Link
              key={course.id}
              href={`/${course.id}`}
              className="group block rounded-lg border border-border bg-card p-6 transition-colors hover:border-primary/50 hover:bg-primary/5"
            >
              <div className="mb-3">
                <h2 className="text-2xl font-bold text-foreground font-display group-hover:text-primary transition-colors">
                  {course.title}
                </h2>
              </div>
              <p className="text-sm text-muted-foreground mb-4">
                {course.description}
              </p>
              <div className="text-xs text-muted-foreground">
                {course.chapters.length} chapters &middot; {course.lessons.length} lessons
              </div>
            </Link>
          ))}
        </div>
      </main>
    </div>
  );
}
