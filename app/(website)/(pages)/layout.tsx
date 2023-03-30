import { Content } from '@/lib/website/components/layout/content';

export default function PagesLayout({ children }: { children: React.ReactNode }) {
  return <Content className="mt-24 text-zinc-800 dark:text-zinc-100">{children}</Content>;
}
