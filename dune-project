(lang dune 2.5)

(name timber)

(source
 (github revery-ui/timber))

(license MIT)

(authors "Glenn Slotte")

(maintainers "Glenn Slotte" "Bryan Phelps")

(formatting
 (enabled_for reason))

(generate_opam_files true)

(package
 (name timber)
 (synopsis "Opinionated logging framework based on @dbuenzli/logs")
 (depends
  (ocaml
   (>= 4.06.0))
  (reason :build)
  fmt
  logs
  re))
