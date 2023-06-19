![preview](/screenshot.png)

### Running

Single file with the meson build system.

```sh
meson setup build
meson compile -C build
./build/src/plotter x^2
```

### Usage

`./build/src/plotter [equation] ?[o/no]`

- [o\/no] – **o**utline / **no** outline – force outline algorithm, **quality outline** vs **only zero values**

##### Keyboard

- arrows – moves the viewport
- W – zoom in
- Z – zoom out
- r – reset viewport to origin
- q – quit

### Trivia

This was the result of getting nerd-sniped to write a parser in C. It later motivated my high school graduation project [here](https://github.com/mihavlic/function-renderer), which used a Rust rewrite of this parser [here](https://github.com/mihavlic/function-renderer/blob/553147ab93c6b306b2f4faa4b330e109636acd04/src/parse/parser.rs).