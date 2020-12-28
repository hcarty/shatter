# shatter - a block breaking game

Shatter is a block breaking game written in OCaml using the
[Orx](https://orx-project.org/) game engine.

## Building and running the game

1. Install Orx, following the instructions on the [wiki](https://wiki.orx-project.org/).
1. You will need opam and OCaml version 4.10.0 or newer.
1. Pin [ocaml-orx](https://github.com/orx/ocaml-orx) with opam:
    ```
    opam pin add orx git+https://github.com/orx/ocaml-orx.git#master
    ```
1. Build and run the game with `dune`
    ```
    dune exec src/shatter.exe`
    ```

## Controls

- Left/right arrows or `A`/`D` keys to move the paddle
- `Esc`ape key to quit
