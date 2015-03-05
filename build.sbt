def github(user: String, project: String, branch: String): RootProject =
  RootProject(uri(s"git://github.com/$user/$project.git#$branch"))

lazy val root = (
  project in file(".")
  dependsOn github("EECOLOR", "scala-program-builder", "742c138c79")
  settings (
            name := "ee-parser",
    scalaVersion := "2.11.6"
  )
)
