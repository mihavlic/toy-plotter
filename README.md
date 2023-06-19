### Running

Single file with the meson build system.

```sh
meson setup build
meson compile -C build
./build/src/plotter x^2
```

### Usage

`./build/src/plotter [equation] ?[o/no]`

- [o\/no] -- *o*utline / *no* outline -- force outline algorithm, *quality outline* vs *only zero values*

##### Keyboard

- arrows -- moves the viewport
- W -- zoom in
- Z -- zoom out
- r -- reset viewport to origin
- q -- quit
