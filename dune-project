(lang dune 3.20)

(name vocc)
(version 0.1)

(generate_opam_files true)

(source
 (github matthieuporte/vocc))

(authors "Matthieu Porte" "Clement Pochart")

(maintainers "Matthieu Porte <matthieu@famille-porte.eu>")

(license MIT)

(documentation https://url/to/documentation)

(using menhir 2.1)
; (using llvm 20.1)

(package
 (name vocc)
 (synopsis "C compiler")
 (description "A simple c compiler written in Ocaml.")
 (depends ocaml)
 (tags
  (c compiler)))
