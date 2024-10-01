---
title: "Plot GeoJSON on Your Blog Posts"
date: 2024-08-22 21:50:38
description: How to plot GeoJSON data on your blog posts.
taxonomies:
  tags:
    - Technical Note
    - Hacking
    - Computing
---

If you wonder how to plot GeoJSON data on your blog posts, here is a simple
example. In this blog post, I will show how to plot Microsoft Azure regions on a
map using the Leaflet.js JavaScript library.

<!-- more -->

## Motivation

I am currently not using [Microsoft Azure]. But for a tool I am developing, I
wanted to check Azure regions. [Azure CLI] provides a command to list all
regions. And the output contains longitude and latitude information.

I thought it would be nice to plot these regions on a map. Better, I can use
this as an example to show how to plot GeoJSON data on a blog post.

We need:

1. A GeoJSON file containing the data we want to plot.
2. A JavaScript library to plot the data.
3. A Zola shortcode to embed the map in the blog post.

## GeoJSON Data

[GeoJSON] is a format for encoding a variety of geographic data structures as a
JSON value. You can use this format to plot points, lines, and polygons on a
map. You can play with GeoJSON using the [GeoJSON.io] website or [GeoJSONLint]
webtool.

In my case, I created a GeoJSON file named `azure.json` containing the Microsoft
Azure region coordinates. I used [Azure CLI] to list physical regions and
produced the GeoJSON file:

```sh
az account list-locations --query "[?metadata.regionType == 'Physical']" -o json |
  jq '{
        "type": "GeometryCollection",
        "geometries": [
          .[] | {type: "Point", coordinates: [(.metadata.longitude | tonumber), (.metadata.latitude | tonumber)]}
        ]
      }' > azure.json
```

You can see the [file] yourself. In fact, you can copy the content of this file
and paste it into the [GeoJSON.io] or [GeoJSONLint] to see the map yourself. A
bonus: GitHub [can render] GeoJSON and TopoJSON data in markdown files like it
does with Mermaid diagrams.

## Plotting the Data

I used the [Leaflet.js] JavaScript library to plot the data. It has [GeoJSON
support] and is easy to use. We will see how it is used in the next section.

## Zola Shortcode

My blog is built using [Zola]. I created a shortcode named `leaflet_world` to
embed a map in a blog post.

There are many other such static site generators, and they usually come with a
way to create custom _plugins_. You can adopt it for your case easily as it is
simple HTML, CSS and JavaScript.

Here is my Zola shortcode:

```html
<div class="leaflet" id="{{id}}" style="height: {{height}}px;">
  <link
    rel="stylesheet"
    href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"
    integrity="sha256-p4NxAoJBhIIN+hmNHrzRCf9tD/miZyoHS5obTRR9BMY="
    crossorigin=""
  />
  <script
    src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"
    integrity="sha256-20nQCchB9co0qIjJZRGuk2/Z9VM+kNiyxNV1lvTlZBo="
    crossorigin=""
  ></script>
  <script type="text/javascript">
    var map = L.map("{{id}}", {});

    var positron = L.tileLayer(
      "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png",
      {
        attribution:
          '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, &copy; <a href="https://carto.com/attribution">CARTO</a>',
      },
    ).addTo(map);

    L.control.scale().addTo(map);

    map.setZoom(1);

    map.setView([10, 0]);

    fetch("{{geojson | safe}}")
      .then((res) => res.json())
      .then((data) => L.geoJSON(data).addTo(map));
  </script>
</div>
```

This shortcode consumes the following parameters:

- `id`: The id of the `div` element.
- `height`: The height of the map in pixels.
- `geojson`: The URL or path of the GeoJSON file.

Then, I can use this shortcode in my blog post like this:

```markdown
{{/* leaflet_world(id="cloudmap", height=400, geojson="azure.json") */}}
```

.. which results in:

{{ leaflet_world(id="cloudmap", height=400, geojson="azure.json") }}

## Wrap Up

I enjoy adding some new tools in my toolbox:

- GeoJSON is quite simple and powerful. It is a pity that I never used GeoJSON
  before. There is [TopoJSON] as well, which is a more compact format, but I did
  not need it for this simple example.
- I used Leaflet.js before, but I did not know it has GeoJSON support. It is
  nice to know that I can use it to plot GeoJSON data.
- My blog now has a shortcode to embed maps. I can use it in the future for
  other purposes, like plotting Doner Kebab stalls near Azure datacenters.

<!-- REFERENCES -->

[GeoJSON]: https://geojson.org
[GeoJSON.io]: https://geojson.io
[GeoJSONLint]: https://geojsonlint.com
[Azure CLI]: https://docs.microsoft.com/en-us/cli/azure/
[file]: azure.json
[Leaflet.js]: https://leafletjs.com
[GeoJSON support]: https://leafletjs.com/examples/geojson/
[Microsoft Azure]: https://azure.microsoft.com
[Zola]: https://www.getzola.org
[can render]:
  https://docs.github.com/en/repositories/working-with-files/using-files/working-with-non-code-files#mapping-geojsontopojson-files-on-github
[TopoJSON]: https://github.com/topojson/topojson
