---
layout: scalaz
position: 2
section: typeclass
title:  "Type classes"
---
# Type classes

_A type class is a type system construct that supports ad hoc polymorphism.
This is achieved by adding constraints to type variables in parametrically polymorphic types._

Type class instances allow us to add behaviour to types without changing the types themselves.

## Type classes in Scalaz

Here is an overview of some of the type class hierarchy present in Scalaz

<div id="core_types" style="width: 100%; height: 1000px;"></div>

<script type="text/javascript" src="{{site.baseurl}}/js/core_types.js"></script>
<script>directed("#core_types", coreTypes.nodes, coreTypes.links);</script>
