import { SimpleLink } from '@/lib/website/types';

export interface FooterProps {
  copyright: string;
  links: SimpleLink[];
}

export default function Footer({ copyright, links }: FooterProps) {
  return (
    <footer className="mt-32 sm:px-8">
      <div className="mx-auto max-w-7xl lg:px-8">
        <div className="border-t border-zinc-100 pt-10 pb-16 dark:border-zinc-700/40">
          <div className="relative px-4 sm:px-8 lg:px-12">
            <div className="mx-auto max-w-2xl lg:max-w-5xl">
              <div className="flex flex-col items-center justify-between gap-6 sm:flex-row">
                <div className="flex gap-6 text-sm font-medium text-zinc-800 dark:text-zinc-200">
                  {links.map((link) => (
                    <SocialLinkComponent key={link.name} link={link} />
                  ))}
                </div>
                <p className="text-sm text-zinc-400 dark:text-zinc-500">{copyright}</p>
              </div>
            </div>
          </div>
        </div>
      </div>
    </footer>
  );
}

export function SocialLinkComponent({ link: item }: { link: SimpleLink }) {
  return (
    <a key={item.name} href={item.href} className="text-gray-400 hover:text-gray-500">
      <span className="sr-only">{item.name}</span>
      {item.icon ? <item.icon className="h-6 w-6" aria-hidden="true" /> : <span>{item.name}</span>}
    </a>
  );
}
