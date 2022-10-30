(lang dune 3.4)

(name nn_ocaml)

(generate_opam_files true)

(source
 (github diohabara/nn_ocaml))

(authors "diohabara")

(maintainers "diohabara")

(license MIT)

(documentation https://github.com/diohabara/nn_ocaml)

(package
 (name nn_ocaml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  core
  core_bench
  dune
  merlin
  ocaml
  ocaml-lsp-server
  ocamlformat
  odoc
  utop)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
