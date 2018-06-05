Contributing guidelines
==

General
--
- Modules are defined according to the degree of compromise with the language.
- Variance annotations are prohibited on both type classes and data structures, they may cause ambiguity.

Data types
--

- ADTs
  - Singleton constructors that require variance (like `Maybe.Empty`) can be encoded using [Liskov lifting](http://typelevel.org/blog/2014/03/09/liskov_lifting.html) if their scope does not escape the `data` package.

Type classes
--

 - Hierarchy
	 - For each trait of the hierarchy, a particular typeclass must appear only once as return type in one hierarchy level (`BH1`, `BH2`, ...).
	 - Companion objects should only declare implicit instances for the type classes at the top of a hierarchy. 
 - Classes
     - Each typeclass `Foo` should have the following traits defined:
        - `FooClass`: The definition of the typeclass.
        - `FooSyntax`: Syntax macros for the typeclass methods.
        - `FooFunctions`: A trait containing the typeclass's methods, but with
                          the typeclass as a type parameter.
        - `FooInstances`: Instances which are associated with the typeclass and
                          have nowhere better to go. This should be made a
                          superclass of `FooClass`'s companion object, to ensure
                          that the instances are in the correct implicit scope.
     - Note that this means that all typeclass methods must have unique names!
       This is to allow users to `import scalaz.Prelude._` and have access to
       all of the typeclass methods, without needing to use syntax.
