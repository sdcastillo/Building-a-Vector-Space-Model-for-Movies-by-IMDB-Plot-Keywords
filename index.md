---
layout: default
title: Movie Keyword Vector Space
description: Cosine similarity and Euclidean distance on films and actors, read from IMDB plot keywords.
samwiki: true
---

<div class="sw-lede">
  <div class="sw-lede-copy">
    <h2>Films and actors as keyword vectors</h2>
    <p>The table in this repository is the IMDB 5000 extract: 5,043 films, each with a <code>plot_keywords</code> field of tags separated by a pipe. Splitting those fields yields a vocabulary V of 8,086 distinct tags. A film becomes the binary vector x in {0, 1}<sup>V</sup> whose k-th coordinate is 1 exactly when tag k is on that film, and 0 otherwise. The only cast columns used are the three billed leads, <code>actor_1_name</code>, <code>actor_2_name</code>, and <code>actor_3_name</code>.</p>
    <p>An actor profile is the sum of those binary vectors over the films in which the actor is one of the three leads. Coordinate k is then a count: how many of those films carry tag k. It is not binarized again. Actors with ten or fewer credits in those columns are dropped, so a single film cannot define the profile. The actor list in the Shiny app is that restricted set.</p>
    <p>Cosine similarity of two nonnegative vectors x and y is cos θ = (x · y) / (‖x‖₂ ‖y‖₂), the cosine of the angle between them. Multiplying either vector by a positive constant leaves the value unchanged, so two actors with the same mix of tags and different career lengths have cosine 1. On binary film vectors the dot product is the number of shared tags and each Euclidean norm is the square root of the length of that film’s tag list, so the formula is the Ochiai coefficient |K<sub>x</sub> ∩ K<sub>y</sub>| / √(|K<sub>x</sub>| |K<sub>y</sub>|). A short list contained in a long one is not forced toward zero.</p>
    <p>Euclidean distance is d₂(x, y) = ‖x − y‖₂ = √(Σ<sub>k</sub> (x<sub>k</sub> − y<sub>k</sub>)²). It uses magnitude. For binary film vectors each disagreeing coordinate contributes 1 inside the sum, so d₂(x, y) = √|K<sub>x</sub> Δ K<sub>y</sub>|, the square root of the number of tags that appear on exactly one of the two films. A long keyword list sits far from a short one even when every tag on the short list is shared. On actor count vectors the same formula penalizes a gap in how often a tag is used, not only in whether it is used. Cosine compares the direction of the keyword mix. Euclidean distance compares the raw count profile.</p>
    <p>A query adds two vectors and searches the rest of the collection. For films A and B the sum s = a + b has coordinates in {0, 1, 2}: a tag present on both films counts twice. The movie panel returns the other film z that maximizes cos θ(s, z), and it prints that cosine together with the angle θ = arccos(cos θ), once as a fraction of π and once in degrees. In the project notes, Frozen + The Expendables is nearest to The Chronicles of Narnia: The Lion, the Witch and the Wardrobe, with cosine about 0.283, an angle of about 0.409π, roughly 74°. The dot product that produces this ranking gives weight 2 to a tag shared with both queries and weight 1 to a tag shared with only one. The actor panel forms the same kind of sum from two count vectors. The nearest actor under cosine is the one that maximizes cos θ(s, z). The nearest actor under Euclidean distance is the one that minimizes ‖s − z‖₂. The explorer is the <a href="https://sdcastillo.shinyapps.io/imdb_addition/">movie and actor addition app</a>. The vectors and both rankings are built in <code>server.R</code> and <code>ui.R</code>.</p>
  </div>
  <aside class="sw-find">
    <h2>The two measures</h2>
    <ul>
      <li><strong>Film vector</strong> Binary indicator of each IMDB plot keyword.</li>
      <li><strong>Actor vector</strong> Sum of the film vectors for the three lead columns.</li>
      <li><strong>Cosine</strong> Direction of the keyword mix. Scale does not move it.</li>
      <li><strong>Euclidean</strong> Size of the coordinate-wise gap, including count gaps.</li>
      <li><strong>Sum query</strong> Tags on both inputs count twice in the dot product.</li>
    </ul>
  </aside>
</div>

## How a query is scored

- **Vocabulary.** 8,086 distinct plot keywords from 5,043 films. Each film is a vertex of the unit hypercube in that dimension.
- **Actor coordinate.** The number of lead credits whose keyword list contains that tag. Actors with at most ten such credits are left out.
- **Cosine.** (x · y) / (‖x‖₂ ‖y‖₂). For binary films this is |K<sub>x</sub> ∩ K<sub>y</sub>| / √(|K<sub>x</sub>| |K<sub>y</sub>|). It is unchanged by positive rescaling.
- **Euclidean distance.** √(Σ<sub>k</sub> (x<sub>k</sub> − y<sub>k</sub>)²). For binary films this is √|K<sub>x</sub> Δ K<sub>y</sub>|. Longer lists and larger count gaps increase it.
- **Film sum.** s = a + b, then the other title that maximizes cos θ(s, z). A tag on both query films contributes 2 to s · z.
- **Actor sum.** The same addition of count vectors. Cosine keeps the maximum. Euclidean distance keeps the minimum of ‖s − z‖₂.
- **Reported angle.** θ = arccos(cos θ), shown as θ/π and in degrees. Frozen + The Expendables has cosine about 0.283, about 0.409π, about 74°.

<p class="sw-actions">
  <a class="sw-btn sw-btn-live" href="https://sdcastillo.shinyapps.io/imdb_addition/">Open the Shiny app</a>
  <a class="sw-btn sw-btn-source" href="https://github.com/sdcastillo/Building-a-Vector-Space-Model-for-Movies-by-IMDB-Plot-Keywords">Source</a>
</p>
