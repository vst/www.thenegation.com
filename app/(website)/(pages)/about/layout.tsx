import SiteConfig from '@/config';
import { faEnvelope } from '@fortawesome/free-solid-svg-icons';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import Image from 'next/image';
import Link from 'next/link';

export default function PagesLayout({ children }: { children: React.ReactNode }) {
  return (
    <div className="grid grid-cols-1 gap-y-16 lg:grid-cols-2 lg:grid-rows-[auto_1fr] lg:gap-y-12">
      <div className="lg:pl-20">
        <div className="max-w-xs px-2.5 lg:max-w-none">
          <Image
            src={SiteConfig.images.about || ''}
            alt=""
            sizes="(min-width: 1024px) 32rem, 20rem"
            className="aspect-square rotate-3 rounded-2xl bg-zinc-100 object-cover dark:bg-zinc-800"
            width={800}
            height={600}
          />
        </div>
      </div>
      <div className="prose  lg:order-first lg:row-span-2">{children}</div>
      <div className="lg:pl-20">
        <ul role="list">
          {SiteConfig.linksSocial.map((link) => (
            <li
              key={link.name}
              className="flex mt-4 transition text-zinc-600 hover:text-teal-500 dark:text-zinc-200 dark:hover:text-teal-500"
            >
              <Link href={link.href} className="group flex text-sm font-medium ">
                {link.icon && <link.icon className="h-6 w-6 flex-none" />}
                <span className="ml-4">Follow on {link.name}</span>
              </Link>
            </li>
          ))}
          <li className="flex mt-8 border-t pt-8  transition text-zinc-600 hover:text-teal-500 dark:text-zinc-200 dark:hover:text-teal-500">
            <Link href={`mailto:${SiteConfig.email}`} className="group flex text-sm font-medium ">
              <FontAwesomeIcon icon={faEnvelope} className="h-6 w-6 flex-none" />
              <span className="ml-4">{SiteConfig.email}</span>
            </Link>
          </li>
        </ul>
      </div>
    </div>
  );
}
