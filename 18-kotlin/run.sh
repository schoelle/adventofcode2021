#!/bin/sh
set -e
kotlinc Snailfish.kt -include-runtime -d Snailfish.jar
java -jar Snailfish.jar $*
