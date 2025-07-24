# My Personal Website

This Website is built using [Hugo] and [Tailwind]. It is built and deployed
using GitHub Actions. The Website _should_ be live on <https://thenegation.com>
and <https://vst.github.io>.

## Development

Enter the Nix shell provisioned in the repository:

```sh
nix-shell -A shell
```

> **Note**
>
> If you want, you can use [direnv] integration, too:
>
> ```sh
> echo "use nix -A shell" > .envrc
> direnv allow
> ```

Build stylesheet:

```sh
tailwindcss --minify --input styles/main.css --output static/styles/main.css
```

Run development server:

```sh
hugo server
```

Build the Website:

```sh
hugo build --gc --minify
```

The output is generated under the `public/` directory.

Lint codebase:

```sh
dev-check
```

Format codebase:

```sh
dev-format
```

Build the Website:

```sh
dev-build
```

## License and Copyrights

This Website, its content and source code by Vehbi Sinan Tunalioglu are licensed
under [CC BY-SA 4.0] unless otherwise noted.

<!-- REFERENCES -->

[Hugo]: https://gohugo.io
[Tailwind]: https://tailwindcss.com
[CC BY-SA 4.0]: https://creativecommons.org/licenses/by-sa/4.0
