+++
title = "job-watcher: Monitoring background jobs in Rust"
description = "Announcing job-watcher, a small Rust library we’ve been using in production for years to run and monitor background jobs."
path = "/blog/2026/08/announcing-job-watcher"
date = 2026-08-31

[extra]
ogimage = "announcing-job-watcher.png"
+++

[Sibi Prabakaran](https://psibi.in/) just published a nice write-up of [`job-watcher`](https://psibi.in/posts/job-watcher/), a small Rust library we've been using in production for the past few years to run and monitor background jobs. Sibi and I have used this code across multiple clients, and I think it may be a good choice for others as well.

The original code started life inside a futures trading platform we were building. It was important to know not just that a process was alive, but that the background work inside it was actually succeeding and making progress. Sibi later extracted the code into a standalone crate and has continued evolving it across multiple projects and clients.

The basic problem sounds simple: run some work every so often. Then production happens. You need retries. You need to detect jobs that are stuck instead of simply failed. You want useful status information. You want alerts, but you don't want an alert for every retry of the same failure. And ideally you don't want to rebuild all of that machinery for every service.

That's the niche `job-watcher` fills. It owns the lifecycle of the jobs it runs, tracks their state, supports retries and heartbeats, exposes a status page, and can integrate with external health checks and alerting.

Sibi's post does a much better job of walking through the design and production setup than I will here, so rather than duplicate it: [go read the post](https://psibi.in/posts/job-watcher/).

I always enjoy seeing small bits of application code survive long enough, and prove useful enough, to turn into reusable infrastructure.
