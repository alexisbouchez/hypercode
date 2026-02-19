import type { Metadata } from "next";
import { Geist, Geist_Mono, Space_Grotesk } from "next/font/google";
import { TooltipProvider } from "@/components/ui/tooltip";
import { ThemeProvider } from "@/components/theme-provider";
import { CourseThemeWrapper } from "@/components/course-theme-wrapper";
import "./globals.css";

const geistSans = Geist({
  variable: "--font-geist-sans",
  subsets: ["latin"],
});

const geistMono = Geist_Mono({
  variable: "--font-geist-mono",
  subsets: ["latin"],
});

const spaceGrotesk = Space_Grotesk({
  variable: "--font-space-grotesk",
  subsets: ["latin"],
});

export const metadata: Metadata = {
  metadataBase: new URL("https://www.hypercode.run"),
  title: {
    default: "Hypercode - Learn Programming Interactively",
    template: "%s | Hypercode",
  },
  description:
    "Learn Go, Zig, C, ARM64, PostgreSQL, Gleam, R, and HolyC interactively in your browser. No account needed.",
  keywords: [
    "Go", "Golang", "Zig", "C", "ARM64", "assembly", "PostgreSQL", "SQL",
    "Gleam", "R", "HolyC", "TempleOS", "learn programming", "programming tutorial",
    "interactive coding", "Hypercode",
  ],
  openGraph: {
    type: "website",
    siteName: "Hypercode",
    title: "Hypercode - Learn Programming Interactively",
    description:
      "Learn Go, Zig, C, ARM64, PostgreSQL, Gleam, R, and HolyC interactively in your browser. No account needed.",
    url: "https://www.hypercode.run",
    images: [{ url: "/nabla-hypercode.png", width: 1200, height: 630, alt: "Hypercode" }],
  },
  twitter: {
    card: "summary_large_image",
    title: "Hypercode - Learn Programming Interactively",
    description:
      "Learn Go, Zig, C, ARM64, PostgreSQL, Gleam, R, and HolyC interactively in your browser. No account needed.",
    images: ["/nabla-hypercode.png"],
  },
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
    <html lang="en" suppressHydrationWarning>
      <head>
        <script
          type="application/ld+json"
          dangerouslySetInnerHTML={{
            __html: JSON.stringify({
              "@context": "https://schema.org",
              "@type": "WebSite",
              name: "Hypercode",
              url: "https://www.hypercode.run",
              description:
                "Learn Go, Zig, C, ARM64, PostgreSQL, Gleam, R, and HolyC interactively in your browser.",
            }),
          }}
        />
      </head>
      <body
        className={`${geistSans.variable} ${geistMono.variable} ${spaceGrotesk.variable} antialiased`}
      >
        <ThemeProvider
          attribute="class"
          defaultTheme="dark"
          enableSystem
          disableTransitionOnChange
        >
          <TooltipProvider>
            <CourseThemeWrapper>{children}</CourseThemeWrapper>
          </TooltipProvider>
        </ThemeProvider>
      </body>
    </html>
  );
}
