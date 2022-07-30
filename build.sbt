name         := "graph"
version      := "0.0"
scalaVersion := "2.13.8"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "org.scalactic"     %% "scalactic"       % "3.2.10"
libraryDependencies += "org.scalatest"     %% "scalatest"       % "3.2.10"   % "test"
libraryDependencies += "org.scalacheck"    %% "scalacheck"      % "1.15.4"   % "test"
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-15" % "3.2.10.0" % "test"
