---
title: Learn By Teaching, or, Imitation Is the Sincerest Form of Flattery, or, Reinforced Learning (By Turning Students Into Authors (of Educational Software))
type: paper
author: Alexander Berntsen & Emil Henry Flakk
---
[Download full paper with table of contents, references, etc.](https://secure.plaimi.net/papers/2015-04-11-learn-by-teaching.pdf)

In an effort to foster learning by teaching, we propose the development
of a canvas system that makes authoring e-learning modules intuitive. We
empower and liberate non-technical module users by turning them into
module authors, which in turn stimulates learning through teaching. By
making a damn fine piece of software, we furthermore make module
authoring more pleasant for experienced authors as well. We propose a
system that initially enables users to easily author H5P modules. These
modules are successively easy to share and modify. Through gamification
we encourage authors to share their work, and to improve the works of
others.

Introduction
============

#### Motivation: The economics of educational services.

The “Bottom Of the Pyramid” (BOP) is a term central to the development
of value-chains in emerging markets in developing countries. It is
understood as the largest, although poorest segment of the population.
Numerous works, in particular Coimbatore Krishnarao Prahalad’s seminal
work “The Fortune at the Bottom of the Pyramid”, suggests that the key
to economic development then lies in activating this segment of the
population, enabling them not only to use modern (digital) services, but
to author them as well@prahalad2009fortune.

The inspiration for our idea comes from applying this general framework
to the field of education software.

The problem
===========

Alas, most software is written for only a small subset of the
population. H5P[^1] is one of many novel examples of software providing
added-value to tech-savvy teachers and educational software developers.
However, the BOP would in this case be the students.

By providing the correct tooling and educational framework, we enable
students to act not only as users of off-the-shelf (OTS) educational
software, but also as authors of their own or their co-students’
learning experience. By doing so, we allow the student to feel some
degree of ownership to the coursework, allowing for reinforced learning,
as well as sharing (viral) effects (perhaps due to pride), a major force
in the modern network economy.

Such a framework would also encourage the students to create services
and tools more adapted to their particular learning situation, allowing
for an overall better user experience, as well as a valuable source of
inspiration (and usage data/analytics) for educational software
developers and user interface designers.

Our idea
========

We suggest developing an engaging “canvas” for authoring e-learning
modules, taking cues from

<span>UTF8</span><span>min</span>RPGツクール

[^2], Game Maker[^3], and other similar software that succeed in take
advantage of an intuitive interface with a capacious featureset. By
“canvas” (or “scene graph”), we mean an easy-to-learn system of widgets
and composite stores (with rich content), and flows connecting them
together to a cohesive system.

An example of such a system might be a quiz leveraging media like GNU
Mediagoblin, Nasjonal Digital Læringsarena[^4], Youtube, NRK, and
Twitter to showcase the issue at hand, before testing the user’s
comprehension at the end. Thus, the student-come-software-developer is
able to prove their comprehension of multimedia (as mandated by the
curriculum), and other students allowed to take part in the learning
experience. Incentives for doing so might be given by the teacher, or by
arranging competitions that invite the students to share their best
ideas.

Taking cue from Minecraft, we also propose another viable incentive: The
student is provided with ready-made canvases with actors and stores, and
encouraged to make the application perform certain actions
(“gamification”). This is akin to the redstone system found in
Minecraft, where even young users are able to construct discrete logic
circuits providing useful functionality like opening doors, or switching
on lights@brand2013crafting.

Through use of our canvas, we eliminate the BOP by liberating and
empowering it to make its own user experience. As an added bonus, our
canvas may spark some latent creative souls, or inspire technological
awareness and interest. In our increasingly computerised society, this
is in itself a noble cause.

Thus, this canvas might prove to be a force for bridging the gap between
just being a computer user, and having a promising future career in
computer software. While Computing At School[^5] have had great success
in the UK, there is as of today no readily-available path for acquiring
the advanced knowledge needed to develop modern systems given the
current education system in Norway. Grassroots organisations such as Lær
Kidsa Koding[^6] are doing good work, but have yet to strongly influence
the education system. If our canvas is picked up by prominent e-learning
providers like Nasjonal digital læringsarena we can liberate and empower
users through direct action, circumventing bureaucracy.

In addition to users teaching themselves technology, they also teach
themselves the curriculum more effectively. The student becomes the
teacher, and we achieve learning by teaching, an often sought-after
method of reinforced learning. By only being e-learning module *users*,
students are limited to learning through observation, experimentation,
and (to some extent) mistakes. Learning through teaching offers
advantages not possible to fully realise through either of these;
advantages that won’t manifest if the student is exclusively relying on
an external teacher@cortese2005learning.

We aim to make our canvas the easiest to use way of authoring e-learning
modules, and at the same time making it powerful enough to entice power
users and established e-learning module authors. Consequently, the
target demographic of our canvas is not limited to the BOP, but extends
to include current e-learning module authors.

To further underline our empowering of the BOP, we make sharing of works
very simple, and make it equally simple to author derivatives (forks)
and meta-works (collections). Incentive for sharing your work, and
improving or remixing the work of others is provided through
gamification of the canvas. As an example, there may be a reward system
for users whose modules are often remixed, and for users who make an
improvement to a module that then gets integrated back into the original
module itself.

With gamification, we can also help ensure high quality modules. With a
rating system and achievements for obtaining a high rating, we realise a
self-regulating community.

Initially we aim to support authoring H5P modules, focussing our
development at H5P integration. But with a good modular design we can
extend our canvas to support other standards as well in the future.

Related work
============

H5P has rudimentary editing support@h5pcontent. Our described idea is
orders of magnitude more sophisticated. We should take care to ensure
that our canvas is not more unfriendly to newbies than H5P’s own
solution.

Moreover, one of our goals is to have a flexible design that permits
extending the system to support other standards than H5P in the future.

Conclusions and further work
============================

Our idea is a canvas for authoring e-learning modules, initially
targeting H5P. It should be intuitive to use, yet potent enough to make
sufficiently refined modules. A simple user interface is vital to
construct a ladder out from the BOP, whilst a capable set of features is
important to attract users in general. Via gamification, users are
encouraged to share their modules, and to improve the modules of others.
We encourage a self-regulating community that promotes quality through a
rating system for modules.

By authoring modules, the student becomes the teacher, which leads to
more effective learning. Learning by teaching is a powerful concept that
enhances the self-efficacy of students.

[^1]: <http://h5p.org/>

[^2]: <http://www.rpgmakerweb.com/>

[^3]: <https://www.yoyogames.com/studio>

[^4]: <http://ndla.no/>

[^5]: <http://www.computingatschool.org.uk/>

[^6]: <http://www.kidsakoder.no/>
