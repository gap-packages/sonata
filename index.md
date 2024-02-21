---
layout: default
---

# GAP Package {{site.data.package.name}}

{{site.data.package.abstract}}

The current version of this package is version {{site.data.package.version}}, released on {{site.data.package.date}}.
For more information, please refer to [the package manual]({{site.data.package.doc-html}}).
There is also a [README](README.html) file.

{% if site.data.package.license %}
  License: [{{ site.data.package.license }}](https://spdx.org/licenses/{{ site.data.package.license }})
{% endif %}

## Dependencies

This package requires GAP version {{site.data.package.GAP}}
{% if site.data.package.needed-pkgs %}
The following other GAP packages are needed:
{% for pkg in site.data.package.needed-pkgs %}
- {% if pkg.url %}<a href="{{ pkg.url }}">{{ pkg.name }}</a> {% else %}{{ pkg.name }} {% endif %}
  {{- pkg.version -}}
{% endfor %}
{% endif %}
{% if site.data.package.suggested-pkgs %}
The following additional GAP packages are not required, but suggested:
{% for pkg in site.data.package.suggested-pkgs %}
- {% if pkg.url %}<a href="{{ pkg.url }}">{{ pkg.name }}</a> {% else %}{{ pkg.name }} {% endif %}
  {{- pkg.version -}}
{% endfor %}
{% endif %}


## Author{% if site.data.package.authors.size != 1 %}s{% endif %}
{% for person in site.data.package.authors %}
 {% if person.url %}<a href="{{ person.url }}">{{ person.name }}</a>{% else %}{{ person.name }}{% endif %}
 {%- if forloop.last -%}.{% else %}, {%- endif -%}
{% endfor %}

{% if site.data.package.contributors and site.data.package.contributors.size > 0 %}
## Contributor{% if site.data.package.contributors.size != 1 %}s{% endif %}
 {% for person in site.data.package.contributors %}
  {% if person.url %}<a href="{{ person.url }}">{{ person.name }}</a>{% else %}{{ person.name }}{% endif %}
  {%- if forloop.last -%}.{% else %}, {%- endif -%}
 {% endfor %}
{% endif %}

Introduction
------------

 SONATA stands for "systems of nearrings and their applications". It provides methods for the construction and the analysis of finite nearrings. A left nearring is an algebra (N;+,.), where (N,+) is a (not necessarily abelian) group, (N,.) is a semigroup, and x(y+z) = xy + xz holds for all x,y,z in N.

 As a typical example of a nearring, we may consider the set of all mappings from a group G into G, where the addition is the pointwise addition of mappings in G, and the multiplication is composition of functions. If functions are written on the right of their arguments, then the left distributive law holds, while the right distributive law is not satisfied for non-trivial G.

The SONATA package provides methods for the construction and analysis of finite nearrings.

- Methods for constructing all endomorphisms and all fixed-point-free automorphisms of a given group.
- Methods for constructing the following nearrings of functions on a group G:
        the nearring of polynomial functions of G (in the sense of Lausch-Nöbauer);
        the nearring of compatible functions of G;
        distributively generated nearrings such as I(G), A(G), E(G);
        centralizer nearrings. 
- A library of all small nearrings (up to order 15) and all small nearrings with identity (up to order 31).
- Functions to obtain solvable fixed-point-free automorphism groups on abelian groups, nearfields, planar nearrings, as well as designs from those.
- Various functions to study the structure (size, ideals, N-groups, ...) of nearrings, to determine properties of nearring elements, and to decide whether two nearrings are isomorphic.
- If the package XGAP is installed, the lattices of one- and two-sided ideals of a nearring can be studied interactively using a graphical representation. 


References
----------

- J.R. Clay, Nearrings: Geneses and applications, Oxford University Press -- Oxford, New York, Tokyo, 1992.
- C. Cotti Ferrero and G. Ferrero, Nearrings. some developments linked to semigroups and groups, Kluwer Academic Publishers, Dordrecht, Boston, London, 2002.
- H. Lausch and W. Nöbauer, Algebra of polynomials, North-Holland, Amsterdam, London; American Elsevier Publishing Company, New York, 1973.
- J.D.P. Meldrum, Near-rings and their links with groups, Pitman (Advanced Publishing Program), Boston, Mass., 1985.
- G.F. Pilz, Near-rings, 2nd ed., North-Holland Publishing Company -- Amsterdam, New York, Oxford, 1983.
- H. Wähling, Theorie der Fastkörper, Thales-Verlag, Essen, 1987. 



{% if site.data.package.citeas %}
## Citing

Please, cite this package as

{{site.data.package.citeas}}

You can get more info by typing `Cite("{{ site.data.package.name }}");` in the gap prompt.

{% include button-bibtex.html %}

{% endif %}


{% if site.github.issues_url %}
## Feedback

For bug reports, feature requests and suggestions, please use the
[issue tracker]({{site.github.issues_url}}).
{% endif %}
