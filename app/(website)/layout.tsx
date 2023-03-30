import SiteConfig from '@/config';
import Footer from '@/lib/website/components/layout/footer';
import Header from '@/lib/website/components/layout/header';
import { Inter } from 'next/font/google';
import '../globals.css';

const inter = Inter({ subsets: ['latin'], variable: '--font-inter' });

export const metadata = { title: SiteConfig.title, description: SiteConfig.description };

export default function RootLayout({ children }: { children: React.ReactNode }) {
  return (
    <html lang="en" className={`${inter.variable} h-full antialiased`}>
      <body className="flex h-full flex-col bg-zinc-50 dark:bg-black">
        <div className="fixed inset-0 flex justify-center sm:px-8 bg-bottom bg-cover">
          <div className="flex w-full max-w-7xl lg:px-8">
            <div className="w-full bg-white ring-1 ring-zinc-100 dark:bg-zinc-900 dark:ring-zinc-300/20"></div>
          </div>
        </div>

        <div className="relative">
          {/* HEADER */}
          <Header avatar={SiteConfig.images.avatar} links={SiteConfig.linksNav} />

          {/* CONTENT */}
          {children}

          {/* FOOTER */}
          <Footer copyright={SiteConfig.copyright} links={SiteConfig.linksSocial} />
        </div>
      </body>
    </html>
  );
}
