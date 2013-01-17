import sbt._
import Keys._

object AivikaBuild extends Build {
println("Building aivika")
    lazy val root = Project(id = "aivika",
                            base = file("aivika"))

    lazy val examples = Project(id = "aivika-examples",
                           base = file("aivika-examples")) dependsOn(root, experiments)

    lazy val experiments = Project(id = "aivika-experiment",
                           base = file("aivika-experiment")) dependsOn(root)
}
