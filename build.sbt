scalaVersion := "3.3.3"

libraryDependencies += "org.scalatest" % "scalatest_3" % "3.2.15" % "test"

libraryDependencies += "com.lihaoyi" % "pprint_3" % "0.8.1"

Compile / scalaSource := baseDirectory.value / "src"

Test / scalaSource := baseDirectory.value / "test" / "src"

Compile / packageSrc / artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
    "for-marmoset.zip"
}

Compile / packageSrc / mappings := {
          (Compile / sources).value pair Path.rebase(baseDirectory.value / "src", "src/")
}

Compile / packageSrc / mappings ++= {
          ((baseDirectory.value / "test") ** "*") pair Path.rebase(baseDirectory.value / "test", "test/")
}
