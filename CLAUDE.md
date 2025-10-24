# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

## Project Overview

This is a personal website built with **Hugo** (static site generator) and
**Tailwind CSS v4** (styling framework). The site features a blog with search
functionality powered by Pagefind, dark mode support via Alpine.js, and is
deployed on Vercel.

## Development Environment

This project uses **Nix flakes** for reproducible development environments.

### Setup

1. Enter the Nix development shell:

   ```sh
   nix develop
   ```

2. For direnv users (`.envrc` is already configured):

   ```sh
   direnv allow
   ```

3. Install Node.js dependencies:
   ```sh
   npm install
   ```

### Common Commands

**Development server:**

```sh
npm run dev
```

Runs Hugo server with live reload at http://localhost:1313

**Build for production:**

```sh
npm run build
```

This runs two steps:

1. `npm run build:hugo` - Builds the Hugo site with GC and minification
2. `npm run build:pagefind` - Generates the search index using Pagefind

**Formatting:**

```sh
npm run format
```

Formats code using Prettier (with Go template and Tailwind plugins) and Taplo
(TOML)

**Linting:**

```sh
npm run lint
```

Runs Taplo linting for TOML files

**Check everything:**

```sh
npm run check
```

Runs format checks, linting, and builds the site (used in CI)

## Architecture

### Directory Structure

- **`content/`** - Markdown content files
  - `content/posts/` - Blog posts (each in its own directory with `index.md`)
  - `content/_index.md` - Homepage content
  - `content/about.md` - About page

- **`layouts/`** - Hugo templates (Go templates)
  - `layouts/baseof.html` - Base template with Alpine.js dark mode integration
  - `layouts/_partials/` - Reusable template components (header, footer, head,
    etc.)
  - `layouts/_shortcodes/` - Custom shortcodes for content (columns,
    leaflet_world)
  - `layouts/tags/` - Tag taxonomy templates

- **`assets/`** - Source assets processed by Hugo
  - `assets/css/main.css` - Tailwind CSS v4 with custom styles and dark mode
    variants
  - `assets/js/main.js` - Custom JavaScript

- **`static/`** - Static files copied as-is to output

- **`public/`** - Generated site output (gitignored)

- **`var/tools/`** - Custom Nix-packaged development tools
  - `dev-md-format` - Markdown formatting utility
  - `dev-cross-post-devto` - Dev.to cross-posting tool
  - `dev-cross-post-hashnode` - Hashnode cross-posting tool

### Hugo Configuration

Configuration is in `hugo.toml`:

- Site metadata (title, baseURL, language)
- Menu structure (Home, Blog, About)
- Syntax highlighting config (Catppuccin Mocha theme)
- Build stats enabled for Tailwind CSS purging
- Cache busters for CSS watching `hugo_stats.json`

### Styling Architecture

The site uses **Tailwind CSS v4** with:

- Custom CSS in `assets/css/main.css` using the new `@import "tailwindcss"`
  syntax
- Custom dark mode variant: `@custom-variant dark (&:where(.dark, .dark *))`
- Typography plugin for prose content
- CSS source tracking via `hugo_stats.json` for automatic class purging
- Google Fonts: Inter (sans) and JetBrains Mono (monospace)
- Dark mode toggled by Alpine.js `x-data` on `<body>` with localStorage
  persistence

### JavaScript & Interactivity

- **Alpine.js** - Lightweight framework for dark mode toggle and interactive
  components
  - Alpine Persist plugin for dark mode state persistence
  - Dark mode controlled via `x-data="{darkMode: $persist(false)}"` on body
    element
- **Pagefind** - Client-side search initialized on page load
- **Plausible Analytics** - Privacy-friendly analytics at `/vendor/p/`

### Template Patterns

Hugo templates follow a hierarchical structure:

- `baseof.html` defines the base structure with `{{ block "main" . }}`
  placeholder
- Page templates (`home.html`, `page.html`, `section.html`, etc.) define their
  `main` block
- Partials are included with `{{ partial "name.html" . }}`
- Partials use `partialCached` for performance where appropriate

### Nix Development Shells

Two shells defined in `flake.nix`:

1. **default** - Full development environment with Hugo, Node.js, Taplo, and
   custom cross-posting tools
2. **ci** - Minimal CI environment with just Hugo, Node.js, and Taplo

## CI/CD

GitHub Actions workflow (`.github/workflows/check.yaml`):

- Runs on PRs and manual dispatch
- Uses Nix CI shell: `nix develop .#ci`
- Executes: `npm ci && npm run check && npm run build`

Deployment happens automatically via Vercel (configured in `vercel.json`).

## Content Guidelines

Blog posts are stored in `content/posts/YYYY-MM-DD_slug/index.md` format with
frontmatter for metadata (title, date, tags, summary).

## Prettier Configuration

Prettier is configured in `package.json` with:

- Go template parser for `.html` files (Hugo templates)
- Tailwind CSS class sorting plugin
- 80 character print width
- Prose wrapping enabled for markdown
