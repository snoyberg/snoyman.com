+++
title = "Programmer learning list"
description = "This post is geared at new programmers looking to improve their skills and work in the field. What should you focus on learning to become a great programmer?"
path = "/blog/2021/03/programmer-learning-list"
date = 2021-03-24

[extra]
ogimage = "programmer-learning-list.png"
+++
My friend has a son who's graduating highschool soon. He's been learning some programming and is considering it for his career. He asked me a question I hear often: what should I learn next?

When I was first learning to code, I always assumed the answer to "what should I learn next" would be a new programming technique, a new language, a new library, or something along those lines. As I've progressed through my career, and especially as I've been on the [other side of the interview desk](https://www.fpcomplete.com/jobs/), I've changed my tune significantly. My standard response to new programmers is that, in addition to honing their coding skills and learning new languages, they should **cross train in related fields** (which I'll explain below).

But even that isn't a complete answer. In this blog post, I want to collect together a list of skills I would recommend programmers conquer. My goal isn't to provide the actual knowledge here, or even necessarily link to good tutorials. Instead, I want to help make newcomers aware of the vast array of tools, techniques, and skills that can help them in writing software.

And I will get back to actual coding at the end.

## The command line

I guess I now count as "old" by some standards. Back in my day, using a computer meant sitting at a DOS prompt and typing in commands. Things have changed a lot since I was a kid. And I was surprised to find out just how many developers these days are able to code without ever hitting a shell.

Learning how to use a command line is vital. Many tools only expose a command line interface. You can be faster in some cases on the command line. I'm not telling you that you must live in the shell, but you should be able to do basic things like:

* Directory traversal
* File management (copy/move/delete)
* Compile and/or run your source code

If you're on Windows, I'd recommend getting familiar with Linux, possibly using WSL2 instead of a full virtual machine. If you're on Mac, you can use the shell built into the OS, but you may get a better educational experience installing a Linux VM.

## Version control

You need to learn how to keep track of your source code using version control. Version control software lets you keep track of the change history of your project, which can be vital for debugging. It also makes it much easier for a team to collaborate on a codebase.

[GitHub](https://github.com/) and [GitLab](https://gitlab.com/) are two popular sites for hosting open source projects, and they use Git (kind of implied by the names I guess). Git is the most popular option these days, so I would recommend getting comfortable with using Git on the command line with either GitHub or GitLab as your repository.

There are _lots_ of great tutorials out there.

## Networking

Most software today needs at least some level of network interaction. Understanding how networks work is important. At the very least, you should understand the basics, like IP addresses, ports, and protocols. But learning about firewalls, load balancing, proxies, and more, will all pay off. And not just in your programming career. It's great to be able to debug "why isn't the WiFi working?"

Ultimately, I would recommend learning the basics of how clouds like AWS and Azure work. Trying to set up an AWS Virtual Private Cloud with subnets, ingress rules, gateways, and more, can all be a really educational experience that nails down details of how networks operate that may otherwise take you months or years to glean.

## Testing

Part and parcel of writing good software is learning how to test software. Many of us learn how to program and then "test" our code by running it. Unit and integration testing are skills you should have and use on virtually every piece of software you write. While it slows you down initially and feels tedious, a good test suite will very quickly pay off and let you work faster and with more certainty.

## Continuous Integration

Continuous Integration, or CI, combines testing and version control. The idea is that, every time you create a new iteration of your code, you have a set of tests that are run for you automatically. In the past 10 years, the tooling around CI has improved drastically. Providers like GitHub and GitLab have built-in CI solutions (GitHub Actions and GitLab CI, respectively), and they're easy to get started with.

As you get more advanced, CI can be used to run code analysis tools, produce artifacts like compiled executables, and even deploy or release new versions of applications (often termed Continuous Deployment, or CD).

Like testing, getting started with CI is a bit slow. But once you learn the basics, it's trivial to add to a project, and helps you discover issues much more quickly.

I'd recommend looking for a detailed tutorial with full examples for the programming language you're using.

## Writing

This is potentially the area I most underappreciated when getting started as a programmer. I'm not exaggerating with this statement: I believe the strongest skill a programmer can add to their arsenal is being good at writing. Good writing means that you can clearly explain an idea, in correct language, in the least amount of words necessary. When you're learning to program, you're typically working on your own, so writing isn't essential. But when you start working on teams, you'll need to write:

* Documentation
* Bug reports
* Feature requests
* Customer proposals
* Requirements documents
* Emails (so many emails!)
* Text messaging
* Potentially: blog posts

Get good at writing. Practice. Take writing courses. It will pay dividends for the rest of your life.

As a corollary: get good at reading too. When I first started professional development, I was daunted by requirements documents. At this point, I realize that spending a few hours carefully reading through such information will save months of wasted effort building the wrong thing.

## Various programming languages

It's not enough these days to know just a single programming language. Virtually every programmer needs to know at least a few. Beyond what specific languages you _need_ to know, it's important to learn other languages to learn the techniques they offer. I recommend learning a few different _categories_ of language to learn those techniques. In descending order of priority from me:

* **Functional programming** Most schools are still not teaching functional programming (FP). FP is a powerful approach that makes many kinds of code easier to write. I'm biased, but I would recommend Haskell as the best FP language to learn, since it forces you to understand FP better than many other languages. It's also valuable to learn a language in the LISP family. Learning FP will help you write better code in almost any language.
* **Systems programming** Systems languages are lower level and allow more control of how the program operates. Learning them teaches you more about how the program runs on the system, and can be helpful in understanding and debugging problems in other languages. My biggest recommendation is to [learn Rust](https://www.beginrust.com/), though C and C++ are other languages in this family.
* **Object oriented** Java and C# are the two most popular object oriented (OO) languages in this family. (Yes, Python and C++ are popular too, but I'm categorizing them separately.) OO introduced many new paradigms and is still likely the most popular programming approach today, though I would personally favor FP approaches most of the time. Still, there's a lot to learn from OO, and odds are you'll be writing some OO code in your career.
* **Scripting** Python and Ruby are two popular scripting languages in the object oriented family. Python in particular has a lot of usage in related fields like DevOps and data science. It's also a simple language, so pretty easy to get started with.

## Programming techniques

Regardless of what programming language you use, it's worth becoming familiar with some additional techniques that transcend specific language, including:

* Database programming. I'd strongly recommend learning SQL. SQLite and PostgreSQL are two great open source databases to learn with.
* Concurrency and asynchronous programming. This is becoming ever more vital today.
* Network programming, especially how to make HTTP servers and clients.
* Web frontend coding with HTML/CSS/JavaScript.
* Data serialization, with formats like JSON, YAML, and binary files.

## Conclusion

The information above may seem daunting. "I need to learn all of that to be a programmer?" No, you don't. But you need to learn a lot of it to become a *great* programmer. Learning all of it takes time, and goes hand-in-hand with starting your career. If you're not yet at the point of professionally programming, I'd recommend getting started with some hobby projects. Consider contributing to open source projects. Many of them will indicate whether they are open to newbie contributions. And many programmers love to get to share knowledge with new programmers.

Did I miss anything in this list? Do you have any follow-up questions? Ping me in the comments below or [on Twitter](https://twitter.com/snoyberg).
