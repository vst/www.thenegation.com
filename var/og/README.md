# OpenGraph Stuff

Create build directory:

```sh
mkdir -p build
```

Generate CSS file:

```sh
tailwindcss -i ./input.css -o ./build/output.css
```

Render default OpenGraph image template:

```sh
echo '{"title": "the negation", "subtitle": "@vst", "url": "https://thenegation.com", "author": "Vehbi Sinan Tunalioglu", "avatar": "https://gravatar.com/avatar/eb4489c4e08af7eb9c2163234047df50"}' |
  mustache templates/default.html > build/default.html
```

Build default OpenGraph image:

```sh
puppeteer screenshot build/default.html ../../static/media/og.png --viewport 1200x630
```
