import type { Metadata } from "next";
import { Geist, Geist_Mono, Space_Grotesk } from "next/font/google";
import { TooltipProvider } from "@/components/ui/tooltip";
import { ThemeProvider } from "@/components/theme-provider";
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
  metadataBase: new URL("https://hypercode.alexisbouchez.com"),
  title: {
    default: "Hypercode - Learn Programming Interactively",
    template: "%s",
  },
  description:
    "An interactive platform to learn Go, Zig, and more programming languages from scratch. No account needed.",
  keywords: [
    "Go",
    "Golang",
    "Zig",
    "learn Go",
    "learn Zig",
    "programming tutorial",
    "interactive",
    "programming",
    "coding",
    "Hypercode",
  ],
  openGraph: {
    type: "website",
    siteName: "Hypercode",
    title: "Hypercode - Learn Programming Interactively",
    description:
      "An interactive platform to learn Go, Zig, and more programming languages from scratch. No account needed.",
    url: "https://hypercode.alexisbouchez.com",
  },
  twitter: {
    card: "summary_large_image",
    title: "Hypercode - Learn Programming Interactively",
    description:
      "An interactive platform to learn Go, Zig, and more programming languages from scratch. No account needed.",
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
              url: "https://hypercode.alexisbouchez.com",
              description:
                "An interactive platform to learn Go, Zig, and more programming languages from scratch.",
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
          <TooltipProvider>{children}</TooltipProvider>
        </ThemeProvider>
      </body>
    </html>
  );
}
