import SiteConfig from '@/config';
import { TheBlog } from '@/lib/website/blog';
import { Content } from '@/lib/website/components/layout/content';
import clsx from 'clsx';
import Image from 'next/image';
import Link from 'next/link';
import { PostSummary } from './(blog)/posts/archive/[page]/page';

export default function Home() {
  const posts = TheBlog.getArchive().slice(0, 5);

  return (
    <>
      <Tagline />

      <CrookedGallery images={SiteConfig.images.gallery.crooked} />

      <Content className="mt-20 mb-16 lg:mt-32 lg:mb-20">
        <div className="md:border-l md:border-zinc-100 md:pl-6 md:dark:border-zinc-700/40">
          <div className="flex max-w-3xl flex-col space-y-16">
            {posts.map((post) => (
              <PostSummary key={post.slug} post={post} />
            ))}
          </div>
        </div>

        <div className="mt-12">
          <Link href="/posts/archive/1" className="text-lg font-medium text-teal-500">
            See all posts &raquo;
          </Link>
        </div>
      </Content>
    </>
  );
}

function Tagline() {
  return (
    <Content className="mt-20 mb-16 lg:mt-32 lg:mb-20">
      <div className="max-w-xl">
        <p className="text-xl font-light uppercase text-zinc-700 dark:text-zinc-200">Current Status</p>
        <h1 className="text-3xl uppercase font-semibold sm:text-4xl text-zinc-800 dark:text-zinc-100">
          Trying to avoid <br /> success at all costs
          <Link
            href="https://link.springer.com/chapter/10.1007/978-1-4302-1949-1_7"
            className="font-thin"
            target="_blank"
            rel="noopener"
          >
            *
          </Link>
        </h1>
      </div>
    </Content>
  );
}

function CrookedGallery({ images }: { images: string[] }) {
  let rotations = ['rotate-2', '-rotate-2', 'rotate-2', 'rotate-2', '-rotate-2'];

  return (
    <div className="mt-16 sm:mt-20">
      <div className="-my-4 flex justify-center gap-5 overflow-hidden py-4 sm:gap-8">
        {images.map((image, imageIndex) => (
          <div
            key={image}
            className={clsx(
              'relative aspect-[9/10] w-44 flex-none overflow-hidden rounded-xl bg-zinc-100 dark:bg-zinc-800 sm:w-72 sm:rounded-2xl',
              rotations[imageIndex % rotations.length]
            )}
          >
            <Image
              src={image}
              alt=""
              sizes="(min-width: 640px) 18rem, 11rem"
              className="absolute inset-0 h-full w-full object-cover"
              width="800"
              height="600"
            />
          </div>
        ))}
      </div>
    </div>
  );
}
