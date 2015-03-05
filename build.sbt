def github(user: String, project: String, branch: String): RootProject =
  RootProject(uri(s"git://github.com/$user/$project.git#$branch"))

lazy val root = (
  project in file(".")
  dependsOn github("EECOLOR", "scala-program-builder", "master")
  settings (
            name := "ee-parser",
    scalaVersion := "2.11.6"
  )
)
