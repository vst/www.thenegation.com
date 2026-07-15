type Env = {
  PLAUSIBLE_SCRIPT_ID?: string;
};

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

  const connectingIp = request.headers.get("CF-Connecting-IP");
  if (connectingIp) headers.set("X-Forwarded-For", connectingIp);

  return headers;
};

export const onRequest: PagesFunction<Env> = async ({ request, env }) => {
  if (request.method !== "GET" && request.method !== "HEAD") {
    return new Response("Method Not Allowed", { status: 405 });
  }

  const scriptId = env.PLAUSIBLE_SCRIPT_ID?.trim();

  if (!scriptId) {
    console.error("Missing PLAUSIBLE_SCRIPT_ID environment variable.");
    return new Response("Server configuration error", { status: 500 });
  }

  const upstreamResponse = await fetch(
    `https://plausible.io/js/pa-${scriptId}.js`,
    {
      method: request.method,
      headers: buildPlausibleHeaders(request),
    },
  );

  const responseHeaders = new Headers(upstreamResponse.headers);
  responseHeaders.set("Content-Type", "application/javascript");

  return new Response(upstreamResponse.body, {
    status: upstreamResponse.status,
    headers: responseHeaders,
  });
};
