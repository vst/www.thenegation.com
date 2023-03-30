import { formatDate } from '@/lib/commons/zeitgeist';
import Link from 'next/link';
import ReactMarkdown from 'react-markdown';
import { getPosts, Post } from './helpers';

export default async function Page() {
  const posts = getPosts().reverse();

  return (
    <div className="md:border-l md:border-zinc-100 md:pl-6 md:dark:border-zinc-700/40">
      <div className="flex max-w-3xl flex-col space-y-16">
        {posts.map((post) => (
          <PostSummary key={post.path.slug} post={post} />
        ))}
      </div>
    </div>
  );
}

function PostSummary({ post }: { post: Post }) {
  return (
    <article className="md:grid md:grid-cols-4 md:items-baseline">
      <div className="md:col-span-3 group relative flex flex-col items-start">
        <h2 className="text-base font-semibold tracking-tight text-zinc-800 dark:text-zinc-100">
          <div className="absolute -inset-y-6 -inset-x-4 z-0 scale-95 bg-zinc-50 opacity-0 transition group-hover:scale-100 group-hover:opacity-100 dark:bg-zinc-800/50 sm:-inset-x-6 sm:rounded-2xl"></div>
          <Link href={`/posts/${post.path.slug}`}>
            <span className="absolute -inset-y-6 -inset-x-4 z-20 sm:-inset-x-6 sm:rounded-2xl"></span>
            <span className="relative z-10"> {post.meta.title}</span>
          </Link>
        </h2>

        <time
          className="md:hidden relative z-10 order-first mb-3 flex items-center text-sm text-zinc-400 dark:text-zinc-500 pl-3.5"
          dateTime={`${post.meta.date}`}
        >
          <span className="absolute inset-y-0 left-0 flex items-center" aria-hidden="true">
            <span className="h-4 w-0.5 rounded-full bg-zinc-200 dark:bg-zinc-500"></span>
          </span>
          {formatDate(post.meta.date)}
        </time>

        <p className="relative z-10 mt-2 text-sm text-zinc-600 dark:text-zinc-400">
          <ReactMarkdown skipHtml={true} remarkPlugins={[]}>
            {post.meta.summary || '*no summary*'}
          </ReactMarkdown>
        </p>

        <div aria-hidden="true" className="relative z-10 mt-4 flex items-center text-sm font-medium text-teal-500">
          Read article
          <svg viewBox="0 0 16 16" fill="none" aria-hidden="true" className="ml-1 h-4 w-4 stroke-current">
            <path
              d="M6.75 5.75 9.25 8l-2.5 2.25"
              stroke-width="1.5"
              stroke-linecap="round"
              stroke-linejoin="round"
            ></path>
          </svg>
        </div>
      </div>

      <time
        className="mt-1 md:block relative z-10 order-first mb-3 flex items-center text-sm text-zinc-400 dark:text-zinc-500"
        dateTime={`${post.meta.date}`}
      >
        {formatDate(post.meta.date)}
      </time>
    </article>
  );
}
