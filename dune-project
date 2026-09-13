(lang dune 3.24)

(name aoc)

(generate_opam_files true)

(source
 (github 0xmukesh/aoc.ml))

(authors "Mukesh <mukesh@0xc84.fyi>")

(maintainers "Mukesh <mukesh@0xc84.fyi>")

(package
 (name aoc)
 (synopsis "Advent of Code solutions in OCaml")
 (description "Advent of Code solutions in OCaml")
 (depends
  ocaml
  base
  core)
 (tags
  ("advent-of-code")))
