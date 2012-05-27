Scalaz is a library written in the Scala Programming Language. One mandate of the library is to depend
only on the core Scala API and the core Java 2 Standard Edition API. The intention of Scalaz is to include general
functions that are not currently available in the core Scala API. Scalaz is released under a BSD open source licence
making it compatible with the licence of the Scala project.

The source code for Scalaz is hosted on GitHub: http://github.com/scalaz/scalaz

Documentation and downloads are on Google Code: http://code.google.com/p/scalaz/

Snapshots and Releases published to the repository on [scala-tools.org](http://scala-tools.org/).

Build Instructions
------------------

The root directory of the project contains the SBT launcher script.

This is the directory structure of the build.

	|- project +
	|          |-ScalazBuild.scala               Project Definition, containing module structure, compiler
	|          |                                 options, cross module dependencies, etc.
	|          |
	|          |-plugins.sbt                     Plugins and settings for the build itself.
	|          |
	|          - build.properties                Version of SBT
	|
	|- <mod N> +
		   |-src   +
		   |       |-main +
		   |       |      |-scala            Source files
		   |       |
		   |       |-test +
		   |              |-scala            Test source files
		   |
		   |-target +
			    | - <scala version M>    All built artifacts (classes, jars, scaladoc) for module N
						     built for version M of Scala.

IDE Support
-----------

To generate IntelliJ Project Files:

	./sbt
	> add-sbt-idea // unless already installed in ~/.sbt/plugins/build.sbt
	[info] Reapplying settings...
	[info] Set current project to scalaz (in build file:/Users/jason/code/scalaz/)
	> gen-idea

