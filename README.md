# aoc-solutions

My solutions in Haskell to [Advent of Code](https://adventofcode.com/) puzzles.

This repository has no connection to Advent of Code or its affiliates. The
Advent of Code web site states:

> Advent of Code is a registered trademark in the United States. The design
elements, language, styles, and concept of Advent of Code are all the sole
property of Advent of Code and may not be replicated or used by any other person
or entity without express written consent of Advent of Code. Copyright 2015-2025
Advent of Code. All rights reserved.

> You may link to or reference puzzles from Advent of Code in discussions,
classes, source code, printed material, etc., even in commercial contexts.
Advent of Code does not claim ownership or copyright over your solution
implementation.

> If you're posting a code repository somewhere, please don't include parts of
Advent of Code like the puzzle text or your inputs.

Inputs to Advent of Code puzzles are not checked in to this repository: see the
`.gitignore` file.

Annual Advent of Code puzzles are organised by day and part. This repository is
simiarly organised. Each puzzle has its own Haskell package.

The solutions assume that the example input is in a file `data.txt` in the
`test` subdirectory. The actual puzzle input can be located anywhere.

The package `aoc-utils` exposes modules providing functions common to puzzles.
