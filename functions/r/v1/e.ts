const buildPlausibleHeaders = (request: Request) => {
  const headers = new Headers();

  const accept = request.headers.get("Accept");
  if (accept) headers.set("Accept", accept);

  const acceptLanguage = request.headers.get("Accept-Language");
  if (acceptLanguage) headers.set("Accept-Language", acceptLanguage);

  const referer = request.headers.get("Referer");
  if (referer) headers.set("Referer", referer);

  const userAgent = request.headers.get("User-Agent");
  if (userAgent) headers.set("User-Agent", userAgent);

  const contentType = request.headers.get("Content-Type");
  if (contentType) headers.set("Content-Type", contentType);

  const connectingIp = request.headers.get("CF-Connecting-IP");
  if (connectingIp) headers.set("X-Forwarded-For", connectingIp);

  return headers;
};

export const onRequest: PagesFunction = async ({ request }) => {
  const upstreamRequest = new Request(request);
  upstreamRequest.headers.delete("cookie");

  const upstreamResponse = await fetch("https://plausible.io/api/event", {
    method: upstreamRequest.method,
    headers: buildPlausibleHeaders(upstreamRequest),
    body:
      upstreamRequest.method === "GET" || upstreamRequest.method === "HEAD"
        ? null
        : upstreamRequest.body,
  });

  return new Response(upstreamResponse.body, {
    status: upstreamResponse.status,
    headers: upstreamResponse.headers,
  });
};
