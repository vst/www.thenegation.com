import { formatDate } from '@/lib/commons/zeitgeist';
import { TheBlog } from '@/lib/website/blog';
import { Prose } from '@/lib/website/components/layout/prose';
import ReactMarkdown from 'react-markdown';
import rehypePrism from 'rehype-prism-plus';
import remarkGfm from 'remark-gfm';

export default async function Page({ params }: { params: { slug: string } }) {
  const post = TheBlog.getPost(params.slug);

  return (
    <div className="xl:relative">
      <div className="mx-auto max-w-2xl">
        <article>
          <header className="flex flex-col">
            <h1 className="mt-6 text-4xl font-bold tracking-tight text-zinc-800 dark:text-zinc-100 sm:text-5xl">
              {post.title}
            </h1>

            <time
              dateTime={`${post.date}`}
              className="order-first flex items-center text-base text-zinc-400 dark:text-zinc-500"
            >
              <span className="h-4 w-0.5 rounded-full bg-zinc-200 dark:bg-zinc-500" />
              <span className="ml-3">{formatDate(post.date)}</span>
            </time>
          </header>

          <Prose className="mt-8">
            <ReactMarkdown skipHtml={true} remarkPlugins={[remarkGfm]} rehypePlugins={[rehypePrism]}>
              {post.content}
            </ReactMarkdown>
          </Prose>
        </article>
      </div>
    </div>
  );
}

export async function generateStaticParams() {
  return TheBlog.getArchive().map(({ slug }) => ({ slug }));
}
