import Util._

addCommandAlias("c", "compile")
addCommandAlias("ca", "test:compile")
addCommandAlias("t", "test")
addCommandAlias("testc", "clean; coverage; test; coverageReport")
addCommandAlias("r", "run")
addCommandAlias(
  "up2date",
  "reload plugins; dependencyUpdates; reload return; dependencyUpdates"
)

onLoadMessage +=
  s"""|
      |───────────────────────────
      |  List of defined ${styled("aliases")}
      |────────┬──────────────────
      |${styled("c")}       │ compile
      |${styled("ca")}      │ compile all
      |${styled("t")}       │ test
      |${styled("testc")}   │ test-coverage
      |${styled("r")}       │ run
      |${styled("up2date")} │ dependencyUpdates
      |────────┴──────────────────""".stripMargin
