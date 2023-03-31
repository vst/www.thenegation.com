import { formatDate } from '@/lib/commons/zeitgeist';
import { Post, TheBlog } from '@/lib/website/blog';
import Link from 'next/link';
import ReactMarkdown from 'react-markdown';

export default async function Page() {
  const posts = TheBlog.getArchive();

  return (
    <div className="md:border-l md:border-zinc-100 md:pl-6 md:dark:border-zinc-700/40">
      <div className="flex max-w-3xl flex-col space-y-16">
        {posts.map((post) => (
          <PostSummary key={post.slug} post={post} />
        ))}
      </div>
    </div>
  );
}

export function PostSummary({ post }: { post: Post }) {
  return (
    <article className="md:grid md:grid-cols-4 md:items-baseline">
      <div className="md:col-span-3 group relative flex flex-col items-start">
        <h2 className="text-base font-semibold tracking-tight text-zinc-800 dark:text-zinc-100">
          <div className="absolute -inset-y-6 -inset-x-4 z-0 scale-95 bg-zinc-50 opacity-0 transition group-hover:scale-100 group-hover:opacity-100 dark:bg-zinc-800/50 sm:-inset-x-6 sm:rounded-2xl"></div>
          <Link href={`/posts/${post.slug}`}>
            <span className="absolute -inset-y-6 -inset-x-4 z-20 sm:-inset-x-6 sm:rounded-2xl"></span>
            <span className="relative z-10"> {post.title}</span>
          </Link>
        </h2>

        <time
          className="md:hidden relative z-10 order-first mb-3 flex items-center text-sm text-zinc-400 dark:text-zinc-500 pl-3.5"
          dateTime={`${post.date}`}
        >
          <span className="absolute inset-y-0 left-0 flex items-center" aria-hidden="true">
            <span className="h-4 w-0.5 rounded-full bg-zinc-200 dark:bg-zinc-500"></span>
          </span>
          {formatDate(post.date)}
        </time>

        <div className="relative z-10 mt-2 text-sm text-zinc-600 dark:text-zinc-400">
          <ReactMarkdown skipHtml={true} remarkPlugins={[]}>
            {post.summary.orDefault('*no summary*')}
          </ReactMarkdown>
        </div>

        <div aria-hidden="true" className="relative z-10 mt-4 flex items-center text-sm font-medium text-teal-500">
          Read article &raquo;
        </div>
      </div>

      <time
        dateTime={`${post.date}`}
        className="mt-1 hidden md:block relative z-10 order-first mb-3 flex items-center text-sm text-zinc-400 dark:text-zinc-500"
      >
        {formatDate(post.date)}
      </time>
    </article>
  );
}
