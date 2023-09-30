# My Personal Website

This Website is built using [Zola] and [Tailwind]. It is built and deployed
using GitHub Actions. The Website *should* be live on <https://thenegation.com>
and <https://vst.github.com>.

## Development

Enter the Nix shell provisioned in the repository:

```sh
nix develop
```

Build stylesheet:

```sh
tailwindcss --minify --input styles/main.css --output static/styles/main.css
```

Run development server:

```sh
zola serve
```

Build the Website:

```sh
zola build
```

The output is generated under the `public/` directory.

## License and Copyrights

This Website, its content and source code by Vehbi Sinan Tunalioglu are licensed
under [CC BY-SA 4.0] unless otherwise noted.

<!-- REFERENCES -->

[Zola]: https://www.getzola.org
[Tailwind]: https://tailwindcss.com
[CC BY-SA 4.0]: https://creativecommons.org/licenses/by-sa/4.0
