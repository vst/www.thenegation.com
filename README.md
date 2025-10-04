# My Personal Website

This Website is built using [Hugo] and [Tailwind]. The Website _should_ be live
on <https://www.thenegation.com>.

## Development

Enter the Nix shell provisioned in the repository:

```sh
nix develop
```

For [direnv] integration, first create a `.envrc` file in the root of the
repository with the following content:

```sh
echo "use flake" > .envrc
```

... and then allow it:

```sh
direnv allow
```

Install Node dependencies:

```sh
npm install
```

Run the Website in development mode:

```sh
npm run dev
```

Format the codebase:

```sh
npm run format
```

Check and build the codebase:

```sh
npm run check
```

Build the Website in production mode:

```sh
npm run build
```

## License and Copyrights

This Website, its content and source code by Vehbi Sinan Tunalioglu are licensed
under [CC BY-SA 4.0] unless otherwise noted.

<!-- REFERENCES -->

[Hugo]: https://gohugo.io
[Tailwind]: https://tailwindcss.com
[CC BY-SA 4.0]: https://creativecommons.org/licenses/by-sa/4.0
