+++
title = "XSLT Rant Explained"
path = "/blog/2012/04/xslt-rant-explained"
date = 2012-04-09

[taxonomies]

[extra]
+++
My previous blog post got a bit more attention than I'd anticipated. I hadn't
really intended it as more than a place to link to and let the XSLT people I
work with know that the language isn't representative of functional languages.
However, a number of people have asked for more details. That's the purpose of
this post.

Before getting into the claims of my post itself, let me address some of the
counter-claims I saw in some of the discussions:

* <b>XSLT isn't *functional*, it's *declarative*.</b> I can agree with this distinction, and think the XSLT world needs to accept it.
* __XSLT is actually very concise.__ No, it's not. XPath- the query language used by XSLT- is incredibly concise, and I have no issue with it. In fact, when designing [xml-conduit](http://hackage.haskell.org/package/xml-conduit), Aristid and I designed the combinators after XPath.
* __XSLT 2.0 fixes a lot of this stuff.__ XSLT 2.0 is just lipstick on a hog. It changes none of the underlying problems.

One last bit of explanation: I don't think most people realize the level to
which XSLT is used in some projects. When used as a client-side technology to
convert some simple XML into simple HTML, XSLT can work just fine. I *still*
think it's a horrible language, but it's passable. The real problem is that
*XSLT doesn't scale*. Here's the issue I was alluding to in my previous blog
post. The DITA-OT has some code that looks like:


    <!-- Copy @id attributes verbatim -->
    <xsl:template match="@id">
        <xsl:attribute name="id" select="."/>
    </xsl:template>
    
    <xsl:template match="some-element">
        <xsl:apply-templates select="@id"/> <!-- Applies the default template to the ID attribute -->
        <!-- A bunch more ugly code -->
    </xsl:template>

Then, one of my coworkers unwittingly added:

    <!-- Some special case requires printing out the attributes verbatim. -->
    <xsl:template match="@*">
        <xsl:if test="$some-special-case">
            <xsl:value-of select="."/>
        </xsl:if>
    </xsl:template>

That second block says to print out the raw value of the attribute in question.
The problem is, it overrides the definition of the @id template in the first
block, and now all the ids for some-element are being printed out verbatim,
which is *not* what we wanted!

So which of those code blocks is wrong? Both of them. You shouldn't be
polluting the global namespace with these kinds of specific templates, they
each should have been put in their own `mode`. But I have two points here:

* `XSLT` is encouraging people to write bad code by using a global namespace by default.
* It's horribly difficult to find a bug like this. This project is well over 8000 lines of XSLT code, spread across some 40 files. It took me an hour to debug this. Sure, the people writing the original code wrote it badly, but a language shouldn't punish maintainers like this!

The issue here is one that many XSLT proponents contend is a strength: you can
go ahead and modify the behavior of existing templates in a later file. This
can actually be very convenient. Imagine you're converting DocBook to HTML, and
you have:

    <xsl:template match="para">
        <p>
            <xsl:apply-templates/>
        </p>
    </xsl:template>

And now you decide that you would like to change this so that a `para` tag is
always given a `class` of `paragraph` in the HTML. You can go ahead and write a
customization:

    <xsl:template match="para">
        <p class="paragraph">
            <xsl:apply-templates/>
        </p>
    </xsl:template>

And all of the existing code will automatically use this new template. I won't
argue that this isn't convenient. It certainly is. But it flies in the face of
all good engineering practice. Suddenly, I have no idea what an
`apply-templates` will do. I describe this as:

> You have no idea what a single line of code will do without analyzing every
> other line of code in your program.

There are plenty of ways to do this properly in real programming languages. In
my Haskell-based DITA processing code, for example, there is a setting allowing
you to specify specific handling for individual elements. Then in the calling
code, you are explicitly calling into a function for which you don't know what
the output will necessarily be. Everything is properly namespaced and
segregated, and you can know by looking at the code in front of you just how
it's going to be dispatched.

I'm out of time for now, but if anyone wants to see examples of XML processing
done right, let me know. I will say that for some of my company's newer
products, I've completely reimplemented DITA-to-HTML transforms, and it's
likely a tenth of the size of the DITA-OT's HTML transforms.
