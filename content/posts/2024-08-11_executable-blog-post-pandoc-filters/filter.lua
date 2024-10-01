local base_url = "https://thenegation.com"

function fix_link(url)
    return url:sub(1, 1) == "/" and base_url .. url or url
end

function Link(link)
    link.target = fix_link(link.target)
    return link
end

function Image(img)
    img.src = fix_link(img.src)
    return img
end

return {
    {
        Link = Link,
        Image = Image
    }
}

