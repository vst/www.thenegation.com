import fs from 'fs';
import matter from 'gray-matter';
import pathlib from 'path';
import { Maybe } from 'purify-ts';

export interface Post {
  path: string;
  slug: string;
  title: string;
  date: Date;
  summary: Maybe<string>;
  content: string;
}

export class Blog {
  private directory: string;
  private posts: Map<string, Post>;
  private pageSize: number;

  constructor(directory: string, pageSize: number) {
    // Keep the directory:
    this.directory = directory;

    // Keep the page size:
    this.pageSize = pageSize;

    // Initialize a lookup table for <slug, Post>:
    this.posts = new Map();

    // Read all posts:
    const posts = scanPosts(this.directory);

    // Iterate over posts and add them to the lookup table:
    posts.forEach((post) => {
      // Get the slug
      let slug = post.slug;

      // Make sure that the slug is not taken:
      let counter = 1;
      while (this.posts.has(slug)) {
        slug = `${post.slug}-${counter}`;
        counter = counter + 1;
      }

      // Re-set the post slug:
      post.slug = slug;

      // Add the <slug, Post> pair to the lookup table:
      this.posts.set(slug, post);
    });
  }

  /**
   * Returns all posts in descending date order.
   */
  getArchive(): Post[] {
    return Array.from(this.posts.values()).sort((a, b) => b.date.getTime() - a.date.getTime());
  }

  /**
   * Attempts to find and return a post by the given slug.
   *
   * @param slug Slug of the post we are looking for.
   */
  findPost(slug: string): Maybe<Post> {
    return Maybe.fromNullable(this.posts.get(slug));
  }

  /**
   * Unsafe version of [findPost].
   *
   * @param slug Slug of the post we are looking for.
   */
  getPost(slug: string): Post {
    return this.findPost(slug).caseOf({
      Nothing: () => {
        throw new Error(`No post is identified by ${slug}`);
      },
      Just: (x) => x,
    });
  }

  getPostCount(): number {
    return this.posts.size;
  }

  getPageSize(): number {
    return this.pageSize;
  }

  getPageCount(): number {
    return Math.floor(this.posts.size / this.pageSize) + 1;
  }

  getPages(): number[] {
    return Array.from(Array(this.getPageCount() - 1).keys()).map((x) => x + 1);
  }

  getPage(page: number): Post[] {
    const offset = (page - 1) * this.pageSize;
    return this.getArchive().slice(offset, offset + this.pageSize);
  }
}

export function scanPosts(directory: string): Post[] {
  return fs
    .readdirSync(directory)
    .map((x) => `${directory}/${x}`)
    .filter((x) => fs.statSync(x).isFile() && pathlib.extname(x).toLowerCase() === '.md')
    .map(getPost);
}

export function getPost(path: string): Post {
  // Read post contents:
  const file = fs.readFileSync(path, 'utf-8');

  // Parse post contents:
  const { data, excerpt, content } = matter(file, { excerpt: true, excerpt_separator: '<!-- more -->' });

  // Get the slug:
  const slug = Maybe.fromNullable(data.slug)
    .filter((x) => x instanceof String || typeof x === 'string')
    .map((x) => (x as string).trim())
    .filter((x) => x.length > 0)
    .orDefaultLazy(() => getSlugFromPath(path));

  // Get the title:
  const title = Maybe.fromNullable(data.title)
    .filter((x) => x instanceof String || typeof x === 'string')
    .map((x) => (x as string).trim())
    .filter((x) => x.length > 0)
    .orDefaultLazy(() => slug);

  // Get the date:
  const date = Maybe.fromNullable(data.date)
    .filter((x) => x instanceof Date || x instanceof String || typeof x === 'string')
    .map((x) => (x instanceof Date ? x : new Date(`${x}T00:00:00Z`)))
    .filter((x) => !isNaN(x.getTime()))
    .orDefaultLazy(() => fs.statSync(path).ctime);

  // Get the summary:
  const summary = Maybe.fromNullable(excerpt)
    .map((x) => x.trim())
    .filter((x) => x !== '');

  // Done, construct the post and return:
  return {
    path,
    slug,
    title,
    date,
    summary,
    content,
  };
}

export function getSlugFromPath(path: string): string {
  return pathlib.basename(path, pathlib.extname(path)).replace(/^.+?(_)/, '');
}

export const TheBlog = new Blog('./content/posts', 5);
