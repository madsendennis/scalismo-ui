addCompilerPlugin("com.github.ghik" % "silencer-plugin" % "0.4")

libraryDependencies += "com.github.ghik" % "silencer-lib" % "0.4"

resolvers += Resolver.jcenterRepo

resolvers += Resolver.bintrayRepo("unibas-gravis", "maven")

libraryDependencies += "ch.unibas.cs.gravis" %% "scalismo-faces" % "develop-1151f924379dec238c07d7cfd28da085e7ce5f51-SNAPSHOT"


//dependencyOverrides += "ch.unibas.cs.gravis" %% "scalismo" % "0.18-M1"
