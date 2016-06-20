Contributing guidelines
==

General
--
- Variance annotations are prohibited on both type classes and data structures, they may cause ambiguity.

Data types
--

Type classes
--

 - Hierarchy
	 - For each trait of the hierarchy, a particular typeclass must  appear only once as return type in one hierarchy level (`BH1`, `BH2`, ...).
	 - Companion objects should only declare implicit instances for the type classes at the top of a hierarchy. 

