(lang dune 2.0)

(name shatter)

(license MIT)

(maintainers "Hezekiah M. Carty <hez@0ok.org>")

(authors "Hezekiah M. Carty <hez@0ok.org>")

(source
 (github hcarty/shatter))

(generate_opam_files true)

(package
 (name shatter)
 (synopsis "A block breaking game")
 (depends
  orx
  (dune
   (>= 2.0))
  (ocaml
   (>= 4.10.0))))
