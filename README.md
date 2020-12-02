# README.md

This repo shows my progress through the "Advent of Code 2020" exercise, described [here](https://adventofcode.com).

This code is written in Scala. You need to have some kind of working Scala environment to compile and run it, but that's not too hard. You might use an IDE like IntelliJ, in which case just import build.sbt as a project, and it should figure it out. Otherwise, you'll need a working sbt setup. This is expected to work just fine from the command line:

A few useful commands:

```
sbt clean
sbt test
sbt clean coverage test coverageReport
```

The code that does the work is generally in src/main/scala, and the code that drives it is generally in src/test/scala. I'm not really trying to set a great example here, just trying to have fun and live in a world without any conditional logic.

