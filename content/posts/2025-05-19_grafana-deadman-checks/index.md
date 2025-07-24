---
title: "Deadman Checks in Grafana"
date: 2025-05-19 22:17:05
description: >
  A quick note on how to set up deadman checks in Grafana with InfluxDB as the
  data source.

slug: grafana-deadman-checks
tags:
  - Technical Notes
  - Observability
  - Grafana
---

This post is a quick, technical note on how to set up deadman checks in Grafana
with InfluxDB as the data source, and how to deal with a peculiar case when the
host is not reporting and the alert enters the _resolved_ state.

<!--more-->

## Background

After using [InfluxDB] for over four years, I am slowly migrating to [Grafana]
with various data sources. One of the most useful features of the InfluxDB stack
is [Telegraf]. I am still using Telegraf to collect metrics from my hosts with
InfluxDB as the persistence layer. Although I am not using the InfluxDB UI
anymore, I still rely on InfluxDB alerts.

Recently, I set up Grafana alerts with InfluxDB as my data source, except for
[Deadman Checks]. Then, I decided to set up deadman checks in Grafana so that I
could run Grafana and InfluxDB alerts in parallel until I was sure everything
was working as expected, to eventually drop InfluxDB alerts.

## Setting Up the Deadman Check

It turned out that Grafana does not have a "deadman check" out-of-the-box. This
makes sense, as it is rather the _job_ of the data source: we do not want to
pull all the data from InfluxDB into Grafana to perform filtering and
transformation. Luckily, I figured out how to use InfluxDB's [monitor.deadman]
function to create a deadman check in Grafana:

```flux
import "date"
import "influxdata/influxdb/monitor"

// You can read it as follows: If the host does not report for `90s`, ...
x_duration = 90s

from(bucket: "my-bucket")
    |> range(start: v.timeRangeStart)
    |> filter(fn: (r) => r._measurement == "system")
    |> filter(fn: (r) => r._field == "uptime")
    |> group(columns: ["host"])
    |> monitor.deadman(t: date.sub(d: x_duration, from: now()))
    |> map(fn: (r) => ({
      time: r._time,
      host: r.host,
      dead: if r.dead == true then 0 else 1
    }))
```

The most difficult part was understanding why the code above was not working
without the final `map` operation. It was needed to convert the result into a
format that Grafana could understand.

In essence, the code above scans data for a window of `X` minutes that is set as
an option on the query, for example `10m`. The `monitor.deadman` function then
checks if a host has reported within the last `10m` but not in the last `90s`.

This has worked well so far and is encouraging.

## Pitfall: False Resolution

However, I encountered a misleading behavior:

- If a host stops reporting and the alert enters _firing_ state, once the `10m`
  window expires and no new data arrives, a new alert is triggered in the
  _resolved_ state.
- This happens because the host's series is missing from the query result, which
  Grafana interprets as normal -- even if the host is still down.

Apparently, the "Configure no data and error handling" configuration for the
alert rule applies to the entire query, not to a missing series in the query
result. This appears to be the [intended behavior]. The only thing we can do in
Grafana v12 is to set the number of evaluations to a higher number before it
enters the _resolved_ state.

## Workarounds and Final Thoughts

I thought this was not ideal, but when I think about it, there is nothing
Grafana can do about it. Consider this: what if we decommission the host? It
would stay in an alerting state forever.

The only solution is to source information about which hosts are commissioned
and expected to report, and then join that with the deadman check results.

I do not know how to source data from two different sources and join them in
Grafana. This can probably be done with the help of the [requests] package in
Flux. But I am simply going to ignore deadman check alerts in the `resolved`
state for now. There is even an option in the "Contact Points" configuration to
stop sending notifications for resolved alerts.

## Conclusion

This setup is good enough for now, but I will explore ways to cross-reference
data sourced from different providers. Before that, I need to make sure that I
can decommission the InfluxDB alerts and rely solely on Grafana.

<!-- REFERENCE -->

[Deadman Checks]:
  https://docs.influxdata.com/influxdb/v2/monitor-alert/checks/create/#deadman-check
[monitor.deadman]:
  https://docs.influxdata.com/flux/v0/stdlib/influxdata/influxdb/monitor/deadman/
[intended behavior]:
  https://community.grafana.com/t/firing-alerts-get-resolved-with-no-data/146947/4
[requests]: https://docs.influxdata.com/flux/v0/stdlib/http/requests/
[Grafana]: https://grafana.com/grafana/
[InfluxDB]: https://www.influxdata.com/
[Telegraf]: https://www.influxdata.com/time-series-platform/telegraf/
