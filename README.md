# Personal Website

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
