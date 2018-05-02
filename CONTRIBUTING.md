Contributing guidelines
==

General
--
- Modules are defined according to the degree of compromise with the language.
- Variance annotations are prohibited on both type classes and scalaz.data structures, they may cause ambiguity.

Data types
--

- ADTs
  - Singleton constructors that require variance (like `Maybe.Empty`) can be encoded using [Liskov lifting](http://typelevel.org/blog/2014/03/09/liskov_lifting.html) if their scope does not escape the `scalaz.data` package.

Type classes
--

 - Hierarchy
	 - For each trait of the hierarchy, a particular scalaz.data.typeclass must  appear only once as return type in one hierarchy level (`BH1`, `BH2`, ...).
	 - Companion objects should only declare implicit instances for the type classes at the top of a hierarchy. 

