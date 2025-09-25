# CSE-332 Software Design Methods
### Assignments Repository

**Environment**
- macOS
- IntelliJ IDEA
- Java 1.8 (OpenJDK)
- Scala 2.11.5 (via sbt)
- sbt 0.13.7 (specified in build.properties)

**Guideline:** Use the Scala REPL (the interactive Scala console) through sbt(simple build tool).

```scala
// build.properties
sbt.version=0.13.7
```

Using ```brew install sbt``` may not install the correct version for these assignments,
because homebrew installs the latest sbt release.

Please manually install sbt **version 0.13.18** (the latest 0.13.x legacy version) from the official website: https://www.scala-sbt.org/download/

Once installed, run the following sbt commands inside your project directory:
```shell
sbt // Launches the sbt shell and downloads project dependencies, update sbt.version at build.properties to 0.13.18
sbt sbtVersion // Checks the sbt version
sbt run // Compiles and runs the main class
sbt test // Compiles and runs tests
sbt console // Starts the Scala REPL with project dependencies
sbt "testOnly directory.testClassName" // Runs a specific test class (e.g., recfun.PascalSuite)
```
