---
title: Let's talk about tempuhs
---
We worked quite a bit on a project called [tempuhs](/works/tempuhs.html). It's 
a chronicler. What that means, and more, is discussed in this news item.

The tl;dr is that tempuhs is a system for chronicling -- i.e. organising 
events in time. This means keeping track of order as well as resolution and 
difference of time. So from a bird's perspective, tempuhs is in fact very 
simple.

Before we dive into the hairy details -- we've put up an interactive
[tempuhs timelines demo](https://tempuhsdemo.plaimi.net) for you to play
around with. It lets you author visual timelines by recording events and then
viewing your timeline. Feel free to just make some stuff and have some fun 
before you come back here and read more about tempuhs.

What is it used for? Anything. And this is where it starts to get interesting. 
tempuhs is meant to be decentralised to the point of letting you run your own
server, and connect your own client -- or using someone else's server 
connected to your client, your server with someone else's client connected, 
and so on.

In addition to a generic architecture, we also want a generic use case. What's 
an event? You tell us. It could be anything. Our idea is just to keep track of 
stuff and the order stuff happens in, and relevant artifacts to this 
information. What you want to do with it is up to you.

This means we want to be context-sensitive without enforcing any context, and 
resolution-sensitive without enforcing a resolution. You can even write your 
own "clocks" and the conversion between them.

Being context aware has huge implications. An easy way to explain this is 
with an example. Imagine having a news archive of World War II newspapers, and 
jumping seamlessly between the Soviet, American, and German papers, to see all 
sides of how the war was perceived at the time.

Resolution is similarly mind-boggling. Another example is useful. Imagine the
history of the universe on the scale of milliards of years, and the big bang
on the scale of Planck time, in the same chronology (event order) with 
seamless and *precise* conversion between them.

tempuhs is *very* abstract and may be used for *a lot* of things. To help you 
wrap your head around just how abstract it is, let's take a few more examples.

  * Visualise your company or product history on an interactive timeline where 
    you can scroll and zoom.
  * Teach your students literature history by showing them a big interactive 
    timeline of released novels in each literary epoch.
  * Make your students learn better by allowing them to see cross-curriculum 
    connections by showing them how the history teacher's visualisation of 
    modern history corresponds to your timeline's modern history literature.
  * Organise your photography based on time, morphing images based on the 
    (possibly non-linear) amount of time between them.
  * Make an educational game where players score points for adding and 
    connecting events on a timeline.
  * Visualise concerts in your nearby area (i.e. filter the concerts on 
    location metadata) within the next n-days.
  * Set up a bunch of batch jobs that you want to schedule for your machines 
    to perform.

tempuhs is nowhere near this stage yet. But we have some ideas on how to get 
it there. And we have some ideas not discussed here. And, of course, we have 
some actual working (Haskell) code as well.

Do you think you could use tempuhs for something? If yes -- would you like to
fund tempuhs development? We sure would like you to! [Get in 
touch](/contact.html)!
