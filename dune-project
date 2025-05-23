(lang dune 3.19)

(name aau_spo_ocaml)

(generate_opam_files true)

(source
 (github audio-engineer/aau_spo_ocaml))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name aau_spo_ocaml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml menhir graphics)
 (depopts utop ocamlformat ocaml-lsp-server)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

(using menhir 3.0)
