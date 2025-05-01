---
title: "Grafana Webhook Integration with ntfy"
date: 2025-05-01 11:16:05
description: |
  Send Grafana alert notifications to ntfy using Webhook integration and
  templates.
taxonomies:
  tags:
    - Technical Notes
    - Infrastructure
---

This post explains how to integrate Grafana alerts with the `ntfy` notification
service using Grafana's Webhook integration, notification templates, and
`ntfy`'s templating capabilities.

<!-- more -->

## Problem

Due to my profession and work, I deal with lots of data, logs and metrics.
Collecting them is one thing, processing and analyzing them is another. There
are many tools to work with such data, but I like to tinker with small but
powerful solutions that can be composed together to solve problems.

Recently, I have been investing in Grafana for monitoring and alerting purposes.
I like Grafana because it can consume data from many different sources and
produce good quality dashboards. Furthermore, its alerting capabilities are
quite powerful, too.

When using Grafana, what I usually do is:

- Setup one or more data sources,
- Create dashboards to visualise the information I am interested in, and
- Create alerts to notify me when something goes wrong.

This post focuses on the last part. Despite Grafana's many built-in notification
channels (_Contact Point_ in Grafana's terminology), I am more interested in
using [ntfy], a small but powerful notification service that works as a pub/sub
tool with a decent API, Web frontend and mobile application.

The problem is that Grafana does not have built-in support for `ntfy`, and
community solutions require spinning a middleware service to convert Grafana
notifications to `ntfy` notifications. However, we can achieve similar effects
using:

1. Grafana's _Webhook_ integration,
1. Grafana's _Notification Templates_, and
1. `ntfy`'s templating capabilities.

I am not going to explain how to set up Grafana or configure it. We will use a
typical Grafana alert payload in JSON format, and use it to demonstrate the
Grafana Webhook integration for `ntfy`.

Let's start with `ntfy` first.

## Getting Started with `ntfy`

Launching a `ntfy` server instance is quite straightforward. We will use the
Docker approach:

```sh
docker run --detach --publish 40001:80 --name my-ntfy binwiederhier/ntfy serve
```

> **NOTE:** I will not go into details about a production setup for `ntfy` here.
> Its documentation is quite good. It explains how to set up a self-hosted
> version of `ntfy`. Alternatively, you can use the public instance for free or
> as a paid service.

You can now browse to <http://localhost:40001> and see the `ntfy` web interface.
Try it out:

1. Enable notifications: Your browser will alert you when a new notification
   arrives.
1. Subscribe to topic: Use `testing` as topic name.
1. Send test notification: Use the form at the bottom on the topic page to send
   a test notification.

## Publishing Notifications to `ntfy`

To understand `ntfy`'s publishing features, head to
<https://ntfy.sh/docs/publish> and try provided examples. Let's use `curl` to
publish a simple notification:

```sh
curl http://localhost:40001/testing -d 'Hello World!'
```

You can use HTTP headers to customize the notification. For example, you can set
the `title` and `priority`:

```sh
curl http://localhost:40001/testing \
  -H 'Title: Hello World!' \
  -H "Priority: 5" \
  -d 'Nice to see you here!'
```

When integrating with other tools, I find the JSON publishing format more
convenient. Let's try it out:

```sh
curl http://localhost:40001 \
  -d '{"topic": "testing", "title": "Hello World!", "message": "Nice to see you here!"}'
```

Note that the `topic` is now part of the JSON payload instead of the URI. You
can also set the `priority` and other fields in the JSON payload. For a complete
list of available fields, check the [ntfy - Publish as JSON] documentation.

## Idiosyncrasies of Grafana and `ntfy`

We know that we have a JSON payload from Grafana and we can send it to `ntfy`.
Using Grafana's notification templates, we can set the `title` and `message`
fields. We need to set the `topic` either in the URL or in the JSON payload.

Unfortunately, we can not use the `topic` in the path segment when we use JSON
publishing approach of `ntfy`. So, we need a `topic` property in the JSON
payload.

First, take a note of this typical Grafana Webhook payload:

```json
{
  "receiver": "MyWebhook@Grafana",
  "status": "firing",
  "alerts": [
    {
      "status": "firing",
      "labels": {
        "alertname": "High CPU Usage",
        "instance": "localhost:9090"
      },
      "annotations": {
        "summary": "High CPU usage detected",
        "description": "CPU usage is above 80% for the last 5 minutes"
      }
    }
  ],
  "groupLabels": {
    "alertname": "High CPU Usage"
  },
  "commonLabels": {
    "alertname": "High CPU Usage",
    "instance": "localhost:9090"
  },
  "commonAnnotations": {
    "summary": "High CPU usage detected",
    "description": "CPU usage is above 80% for the last 5 minutes"
  },
  "externalURL": "http://localhost:3000",
  "version": "1",
  "groupKey": "A:0",
  "truncatedAlerts": 0,
  "orgId": 1,
  "state": "alerting",
  "title": "Alert: High CPU Usage",
  "message": "High CPU usage detected that needs to be investigated."
}
```

Now, if we could just get Grafana to set a custom `topic` property, we would be
done. Although `title` and `message` arrive the way we want (once configured via
Grafana templates), there is no way for us to set the `topic` in Grafana
directly at the top-level. [Here][grafana-webhook] is what Grafana documentation
says about the Webhook data structure:

> **NOTE**
>
> ...
>
> However, you cannot customize the webhook data structure, such as adding or
> changing other JSON fields and HTTP headers, or sending data in a different
> format like XML.
>
> If you need to format these fields as JSON or modify other webhook request
> options, consider sending webhook notifications to a proxy server that adjusts
> the webhook request before forwarding it to the final destination.

So, there is not an obvious and straighforward approach due to the
idiosyncrasies of Grafana and `ntfy`. We need another solution. Here comes the
templating part.

## Templating with `ntfy`

Let's say, we have the following payload:

```json
{
  "subject": "Hello World!",
  "body": "Nice to see you here!"
}
```

`ntfy` allows us to use the `template` URL parameter along with `title` and
`message` URL parameters to set templates for the `title` and `message` fields.
Simplest example would be:

```sh
curl "http://localhost:40001/testing?template=yes&title={{.subject}}&message={{.body}}" \
  --globoff \
  -d '{"subject": "Hello World!", "body": "Nice to see you here!"}'
```

> **Note:** For simplicity, I left the template as is instead of URL-encoding
> it. The `--globoff` option is needed to be able to use `{{` and `}}` in the
> URL.

We can add further parameters to the URL, such as `priority`, `tags`, etc. For
example, to set the `priority` to `5` and set the `tags` to `grafana` and a
`warning`:

```sh
curl "http://localhost:40001/testing?template=yes&title={{.subject}}&message={{.body}}&priority=5&tags=grafana,warning" \
  --globoff \
  -d '{"subject": "Hello World!", "body": "Nice to see you here!"}'
```

## Working with Grafana Webhook Integration

We can now use the Grafana Webhook integration to send notifications to `ntfy`,
with a lengthy, messy URL.

For convenience, save the sample Grafana webhook payload above to a file named
`grafana_alert.json`.

If you are fine with using [Grafana's _Notification Templates_][grafana-tmpl],
you can make the title and message even more informative. So, I leave it up to
you to decide whether you want to use Grafana's templating capabilities and use
simple `ntfy` templates, or use a more complex `ntfy` templating approach.

I am going to stick to the simple `ntfy` templating approach and use the `title`
and `message` fields as templated and provided by Grafana. The URL data
represented as a JSON object is:

```json
cat <<EOF>url.json
{
  "template": "yes",
  "title": "{{.title}}",
  "message": "{{.message}}",
  "priority": 5,
  "tags": "grafana,warning"
}
EOF
```

You can URL-encode it using [jq]:

```sh
jq --raw-output '[to_entries[] | (@uri "\(.key)" + "=" + @uri "\(.value)")] | join("&")' url.json
```

... and the result is:

```txt
template=yes&title=%7B%7B.title%7D%7D&message=%7B%7B.message%7D%7D&priority=5&tags=grafana%2Cwarning
```

The final URL would look like this:

```txt
http://localhost:40001/testing?template=yes&title=%7B%7B.title%7D%7D&message=%7B%7B.message%7D%7D&priority=5&tags=grafana%2Cwarning
```

Let's test it out:

```sh
curl "http://localhost:40001/testing?template=yes&title=%7B%7B.title%7D%7D&message=%7B%7B.message%7D%7D&priority=5&tags=grafana%2Cwarning" \
  -d @grafana_alert.json
```

## Conclusion

We have successfully set up a Grafana alert notification to be sent to `ntfy`
with the help of Grafana's Webhook integration and notification templating
capability, and `ntfy`'s templating capability.

I strongly recommend to invest in Grafana's templating capabilities instead of
overloading the URL with too many and messy parameters. Same Grafana Webhook
payload can be used to send notifications to other services, too. So, stick to
simple and clean URLs and use Grafana's templating capabilities to make the
payload more informative.

<!-- REFERENCES -->

[ntfy]: https://ntfy.sh
[Grafana]: https://grafana.com/grafana/
[ntfy - Publish as JSON]: https://docs.ntfy.sh/publish/#publish-as-json
[grafana-tmpl]:
  https://grafana.com/docs/grafana/latest/alerting/configure-notifications/template-notifications/
[grafana-webhook]:
  https://grafana.com/docs/grafana/latest/alerting/configure-notifications/manage-contact-points/integrations/webhook-notifier/#optional-settings-using-templates
[jq]: https://stedolan.github.io/jq/
