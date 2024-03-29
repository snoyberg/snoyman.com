+++
title = "Dysfunctional Programming: FindMimeFromData"
path = "/blog/2012/03/ie-mimetype-png"
date = 2012-03-22

[taxonomies]

[extra]
+++
I've been a bit torn about what exactly to put on this blog. Short comments go
to Google+. Anything Haskell related goes to the [Yesod blog](http://www.yesodweb.com/blog).
So what goes here?

For now, I've decided to start a series on dysfunctional programming. In other
words, horror stories from the non-Haskell world.

Today's tale begins with a strange bug report. One of our clients says that all
of the CSS changes we put on their server have suddenly disappeared. Which is
especially strange, because I don't have any write access to their production
server. There were two aspects to this report: bullets were missing, and the
title image didn't display in IE. By the way, there's a separate title image,
provided by the client, for each publication.

Forget about the bullets for now, that one wasn't interesting. The title image
was truly a conundrum. I started off by loading the output in Chrome. No
problem. Internet Explorer? Red X. At first I thought it was a URL mangling
issue. But when I tried moving the title image to a simple path on the system
and loading it up, it *still* displayed as a red X.

I started a local Warp server and accessed the site from there. No problem.

So... the file itself is correct, and the name of the file isn't the problem.
What's the issue? I spent a good 30 minutes looking for PNG bugs in IE.
Unfortunately, with IE's sordid history of PNG bugs, it was like looking for a
needle in a haystack. However, I saw one off-hand comment:

> Try right-clicking the image, choosing properties, and look at the type.

I tried it and, sure enough, the type was "Unknown," not "PNG". So it turns out
that when served from a web server, IE was delivered the correct mimetype via a
response header. But when opening from the local filesystem, it needs to detect
the mimetype automatically.

Looking at a file extension is too simple. IE needs to outsmart us. The
solution?
[FindMimeFromData](http://msdn.microsoft.com/en-us/library/ie/ms775147%28v=vs.85%29.aspx).
This wonderfully constructed function inspects the contents of the file in
question and determines the mimetype. For example, this nifty little program
will print the mimetype of the title.png file:

    #include <urlmon.h>
    #include <stdio.h>

    int main(int argc, char* argv[])
    {
        char buff[256];
        LPWSTR out;

        FILE *in = fopen("title.png", "rb");

        fread(buff, 1, 256, in);

        FindMimeFromData(NULL, NULL, buff, 256, NULL, FMFD_DEFAULT, &out, 0);

        printf("%ls\n", out);

        return 0;
    }

The result? `image/pjpeg` of course! Never mind that that's an invalid mimetype, but the Win32 function is giving the __wrong mimetype for a PNG file__!!! I'm not quite certain how Microsoft screwed this up so royally. There's a very explicit set of bytes at the beginning of the file indicating that it's a PNG, and [imagesize-conduit](http://hackage.haskell.org/package/imagesize-conduit) detects it just fine:

    import Data.Conduit
    import Data.Conduit.Binary
    import Data.Conduit.ImageSize

    main :: IO ()
    main = do
        f <- runResourceT $ sourceFile "title.png" $$ sinkImageInfo
        print f

In other words: Internet Explorer, since version 4, can't even display some
simple images.

I had to tell the client that we have no workaround, and they need to either
"fix" their perfectly valid PNG or always serve their content from a webserver.
