import fs from 'fs';
import matter from 'gray-matter';

export interface PostPath {
  slug: string;
  path: string;
}

export interface Post {
  path: PostPath;
  meta: {
    title: string;
    date: Date;
    summary?: string;
  };
  content: string;
}

export function getPostPaths(): PostPath[] {
  const directory = './content/website/blog';
  return fs.readdirSync(directory).map((x) => ({ slug: getSlug(x), path: `${directory}/${x}` }));
}

export function getSlug(x: string): string {
  return x.replace('.md', '');
}

export function getPosts(): Post[] {
  const posts = getPostPaths().map(getPost);
  posts.sort((a, b) => a.meta.date.getTime() - b.meta.date.getTime());
  return posts;
}

export function getPost(path: PostPath): Post {
  const file = fs.readFileSync(path.path, 'utf-8');
  const { data: meta, content, excerpt } = matter(file, { excerpt: true, excerpt_separator: '<!-- more -->' });

  return {
    path,
    meta: {
      title: meta.title,
      date: meta.date,
      summary: excerpt || undefined,
    },
    content,
  };
}
