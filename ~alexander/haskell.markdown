---
title: Recommended Haskell reading and watching
---

It seems everyone who has learned Haskell are also required to compile a list
of references for other people to learn Haskell too. (Also, I believe I am
required to write a monad tutorial at some point.) I once read that Haskellers
would invite Jehova's Witnesses in for tea just to proselytise Haskell to
them.

In any event, after doing a Haskell-related Master Thesis I have now read over
one hundred plt-related papers and some books, and watched a bunch of
lecture videos. So I'd like to think I know a little bit of what I'm talking
about. I hope this will be useful to someone.

This compilation will be updated from time to time (timestamp at the bottom of
the page, and I would love to read more papers and books, watch more lecture
videos, and in general improve the collection. You can send feedback via
[email](mailto:alexander@plaimi.net). You can also send me a patch (which is
the express-lane). If you want to write a patch, this document is on
[GitHub](https://github.com/plaimi/www). Send your formatted patches to the
same [email address](mailto:alexander@plaimi.net).

The entries are colour-coded. Green means readable by Haskell novices, yellow
means that it's most suitable for experienced Haskell hackers, and red means
that it bight be quite difficult stuff to wrap one's head around indeed.

For the colour blind, look to the stars. \* means easy, \*\* means for
experienced hackers, and \*\*\* means difficult material. This is also
suitable for grep and similar.

Cursive entries are videos, and have a % following their star(s). Note that
all videos are possible to get simply by using
[youtube-dl](https://github.com/rg3/youtube-dl/). No Adobe Flash or similar is
necessary.

<div id="dontmarginpls">
* * *
# Concepts
* <div id="green">[*Simon Peyton Jones: Haskell is useless*](https://www.youtube.com/watch?v=iSmkqocn0oQ) * % </div>
  A quick chat with Simon, as he tells us the difference between Haskell and e.g. C, in terms of type safety and having effects.

* <div id="green">[Gabriel Gonzalez: Equational reasoning](http://www.haskellforall.com/2013/12/equational-reasoning.html) * </div>
  The substitutions of equals for equals is in my opinion the most powerful thing about Haskell and purely functional programming languages. This article explains how to do it, in a simple yet comprehensive way.

* <div id="green">[Aditya Bhargava: Functors, Applicatives, And Monads In Pictures](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html) * </div>
  A pleasant intro to functors, applicatives and monads. Too many simplifications for my taste, but still useful. "Values wrapped in context" is not a good way to consider monads. Instead, simply think of them of a way to get a → m b.

* <div id="green">[Aditya Bhargava: Three Useful Monads](http://adit.io/posts/2013-06-10-three-useful-monads.html) * </div>
  A pleasant intro to the writer, reader and state monads. Builds on the previous one, and continues the simplifications, unfortunately.

* <div id="green">[Chris Smith: Why Do Monads Matter?](http://cdsmith.wordpress.com/2012/04/18/why-do-monads-matter/) * </div>
  A much more correct intro to why monads matter to programmers, aimed at programmers or mathematicians. It's very gentle, and, in my opinion, very good. Highly recommended!

* <div id="yellow">[Gabriel Gonzalez: Equational reasoning at scale](http://www.haskellforall.com/2014/07/equational-reasoning-at-scale.html) * </div>
  Follow up to Gabriel's equational reasoning article. Here he explains how this scales. Very useful article, but you should at least be vaguely familiar with monoids and applicatives before diving in.

* <div id="yellow">[Graham Hutton: A tutorial on the universality and  expressiveness of fold](http://www.cs.nott.ac.uk/~gmh/fold.pdf) * * </div>
  Comprehensive introduction to fold (AKA reduce, insert, inject, etc.), using lists as an example data structure. The reader is expected to know basic functional programming, and I'd recommend not being a stranger to arithmetic and algebra. Haskell is used as an example language, but the code is intended to be generalisable. The paper teaches you to derive functions using fold. This is slightly nontrivial, and I don't recommend this as an introduction to practical use of fold per se. That said, fold (λx g → fold (λy → g) (g [1])) (1 :) is the scariest function you'll encounter here, so it's not *that* scary.

* <div id="red">[Philip Wadler: Comprehending Monads](http://www.diku.dk/hjemmesider/ansatte/henglein/papers/wadler1992.pdf) *** </div>
  Maybe the ultimate guide to comprehending monads? Bit difficult material though -- best understand how to use them. See the imperative programming section. This paper is kind of like to monads what Type classes in Haskell are to type classes.

* <div id="red">[Philip Wadler: Theorems for free!](http://www.cs.sfu.ca/CourseCentral/831/burton/Notes/July14/free.pdf) *** </div>
  Deriving theorems that satisfy polymorphic functions. Interesting. Best avoided by novices.

* * *
# Denotational programming
* <div id="green">[John Hughes: Why functional programming matters](http://ipaper.googlecode.com/git-history/8070869c59470de474515000e3af74f8958b2161/John-Hughes/The%20Computer%20Journal-1989-Hughes-98-107.pdf) * </div>
  Sort of a quintessential FP text. Shows us that taking away things (assignment, side-effects) is no good by itself. We need expressive power to make up for a lack of side-effects.

* <div id="yellow">[P. J. Landin: The Next 700 Programming Languages](http://www.thecorememory.com/Next_700.pdf) ** </div>
  Obligatory read on denotational programming. The paper that defined denotative.

* * *
# GHC
* <div id="yellow">[The Architecture of Open Source Applications (Volume 2): The Glasgow Haskell Compiler](http://www.aosabook.org/en/ghc.html) ** </div>
  <p>A comprehensive and insightful, yet accessible introduction to the architecture of GHC.</p>

* * *
# Imperative programming
* <div id="green">[Simon Peyton Jones: Tackling the Awkward Squad: monadic input/output, concurrency, exceptions, and foreign-language calls in Haskell](http://research.microsoft.com/en-us/um/people/simonpj/Papers/marktoberdorf/mark.pdf.gz) * </div>
  How to do IO, concurrency, exceptions and FFI in practice in Haskell. Great introduction.

* <div id="yellow">[Simon L. Peyton Jones & Philip Wadler: Imperative functional programming](http://cse.iitk.ac.in/users/karkare/courses/2010/cs653/Papers/imperative.pdf) ** </div>
  Now that you know how to use monadic IO, you might want to know how it works and why we use monads. This is the paper for you. This is a must-read. A great paper. Consider reading How to make ad-hoc polymorphism less ad-hoc as well, as it is of comparable quality and insight.

* <div id="yellow">[Philip Wadler: How to Declare an Imperative](https://wiki.ittc.ku.edu/lambda/images/3/3b/Wadler_-_How_to_Declare_an_Imperative.pdf) ** </div>
  More comprehensive tutorial of using monads, with alternative solutions to monads. Enlightening, but not for novices.

* <div id="red">[Philip Wadler: Monads for functional programming](http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf) *** </div>
  Three detailed case studies of using monads. Very detailed. Thus not suitable for newbies.

* * *
# Learn Haskell
* <div id="green">[Miran Lipovaĉa: Learn You a Haskell for Great Good!](http://learnyouahaskell.com/) * </div>
  Fun Haskell book for complete Haskell newbies that have been coding at least a bit in an imperative language. Well recommended as a first stop.

* <div id="green">[Brian O'Sullivan, Don Stewart & John Goerzen: Real World Haskell](http://book.realworldhaskell.org/) * </div>
  Example-heavy introductionary book. A few of the simplifications and wordings annoyed me, but those are minor complaints. It's a good read overall.

* <div id="yellow">[*Simon Peyton Jones: A Taste of Haskell*](http://youtu.be/jLj1QV11o9g) ** % </div>
  Simon giving us a whirlwind tour. Might pique your interest, and as such might act as a first stop. Can be watched in tandem with reading Learn You a Haskell for Great Good!, to get a sneak peek at what's coming up.

* * *
# Meta
* <div id="green">[*Simon Peyton Jones: A History of Haskell: Being Lazy With Class (Part 1)*](https://www.youtube.com/watch?v=7NPBrWDzO2A) * % </div>
* <div id="green">[*Simon Peyton Jones: A History of Haskell: Being Lazy With Class (Part 2)*](https://www.youtube.com/watch?v=ILSFJXeNSHw) * % </div>
* <div id="green">[*Simon Peyton Jones: A History of Haskell: Being Lazy With Class (Part 3)*](https://www.youtube.com/watch?v=_61OhYbA36w) * % </div>
* <div id="green">[*Simon Peyton Jones: A History of Haskell: Being Lazy With Class (Part 4)*](https://www.youtube.com/watch?v=klybNF-x6yk) * % </div>
* <div id="green">[*Simon Peyton Jones: A History of Haskell: Being Lazy With Class (Part 5)*](https://www.youtube.com/watch?v=DNOChsIOHqU) * % </div>
* <div id="green">[*Simon Peyton Jones: A History of Haskell: Being Lazy With Class (Part 6)*](https://www.youtube.com/watch?v=19FsWQBprmM) * % </div>
  Short-ish (less than an hour) talk about the history of Haskell so far. Entertaining and interesting.

* <div id="green">[Paul Hudak, John Hughes, Simon Peyton Jones, & Philip Wadler: A History of Haskell: Being Lazy With Class](http://www.iro.umontreal.ca/~monnier/2035/history.pdf) * </div>
  A paper which acts as a more comprehensive history of Haskell. Very interesting read.

* <div id="green">[Philip Wadler: Why no one uses functional languages](http://www.ugcs.caltech.edu/~lordkaos/why-no-one.pdf) * </div>
  Still eerily relevant.

* <div id="yellow">[Haskell 98 language and libraries: the revised report](https://webmail.bmc.uu.se/andlov/dev/books/haskell98-report.pdf) ** </div>
  The language report. Best read it sometime. Includes source code for some libraries. It fills in some blanks.

* <div id="yellow">[Haskell 2010 language report](http://www.f4.fhtw-berlin.de/people/hansen/FHTW-AI/Lehre/2012SS/PProg/Uebungen/Uebung3/Haskell%20Languages%20Report%202010.pdf) ** </div>
  The updated language report.

* * *
# Object-oriented programming
* <div id="red">[Oleg Kiselyov & Ralf Lämmed: Haskell's overlooked object system](http://arxiv.org/pdf/cs.PL/0509027) *** </div>
  <p>I'm not sure you should read this. No, seriously. It's object-oriented programming with full subtype polymorphism. I hate every example in this paper with every fibre of my being. Every cell in my body screams, "this is so wrong!", and yet, it is so so cool. Not for novices of Haskell nor OOP. At all.</p>

* * *
# Parallelism & concurrency
* <div id="green">[Anthony Discolo, Tim Harris, Simon Marlow, Simon Peyton Jones & Satnam Singh: Lock Free Data Structures using STM in Haskell](https://research.microsoft.com/en-us/um/people/simonpj/papers/stm/lock-free-flops06.pdf) * </div>
  Accessible introduction to using STM to avoid locks. Illuminating if you do not know how this works. You should have worked with parallel programming using locks to really understand this.

* <div id="yellow">[*Simon Peyton Jones: The Future is Parallel, and the Future of Parallel is Declarative*](http://youtu.be/hlyQjK1qjw8) ** % </div>
  Simon gives us a whirlwind tour of how Haskell tries to provide you tools for all the different cost-models of concurrent and parallel programming. Relatively gentle introduction, but you should know parallel programming somewhat well before watching it.

* * *
# Types
* <div id="green">[*Brian Hurt: A Pragmatic Case for Static Typing*](http://vimeo.com/72870631) * % </div>
  Why strong static typing matters in practice, i.e. how it makes us get to working code more quickly. Also includes a bit about how clever STM is in Haskell.

* <div id="yellow">[Philip Wadler & Stephen Blott: How to make ad-hoc polymorphism less ad-hoc](http://202.3.77.10/users/karkare/courses/2010/cs653/Papers/ad-hoc-polymorphism.pdf) ** </div>
  The introduction of type classes. Great paper; a must-read. Imperative functional programming is its counterpart for doing IO.

* <div id="yellow">[*Simon Peyton Jones: Adventures with Types in Haskell (Lecture 1)*](https://www.youtube.com/watch?v=6COvD8oynmI) ** % </div>
* <div id="yellow">[*Simon Peyton Jones: Adventures with Types in Haskell (Lecture 2)*](https://www.youtube.com/watch?v=brE_dyedGm0) ** % </div>
* <div id="yellow">[*Simon Peyton Jones: Adventures with Types in Haskell (Lecture 3)*](https://www.youtube.com/watch?v=2IZQx7WNOMs) ** % </div>
* <div id="yellow">[*Simon Peyton Jones: Adventures with Types in Haskell (Lecture 4)*](https://www.youtube.com/watch?v=XtogTwzcGcM) ** % </div>
  Simon shows us how Haskell and GHC are essentially crazy laboratory playgrounds. Very enlightening lecture series that introduces lots of cool things on both the type and kind level. For experienced Haskellers only! Simon moves very quickly here.

* <div id="yellow">[Sam Lindley & Conor McBride: Hasochism: The Pleasure and Pain of Dependently Typed Haskell Programming](http://homepages.inf.ed.ac.uk/slindley/papers/hasochism.pdf) ** </div>
  Haskell is a crazy type laboratory. Let's make it crazier. Hasochism simulates dependent typing in Haskell, and delivers code that the authors envy as Agda programmers! Before reading this, one should be familiar with the point of dependent types. So maybe check out an Agda or Idris tutorial first.

* <div id="red">[Cordelia Hall, Kevin Hammond, Simon Peyton Jones & Philip Wadler: Type classes in Haskell](http://ropas.snu.ac.kr/lib/dock/HaHaJoWa1996.pdf) *** </div>
  A precise definition of type classes. Kind of like a Comprehending type classes. See Comprehending monads for a near-equivalent paper for monads.

</div>

* * *
2014-06-10-17-01-10
