(lang dune 3.4)

(name ocaml_template)

(generate_opam_files true)

(source
 (github diohabara/ocaml_template))

(authors "diohabara")

(maintainers "diohabara")

(license MIT)

(documentation https://github.com/diohabara/ocaml_template)

(package
 (name ocaml_template)
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
