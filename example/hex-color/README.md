# hex-color

This example demonstrates how to add a custom hex color token.

If you've already built Owl at `../../owl`, you can build hex-color with `make`:

```
$ make
```

Pass a color in hex format or `rgb(...)` format to see the channels:

```
$ hex-color '#deface'
r = 222
g = 250
b = 206
$ hex-color 'rgb(1, 2, 3)'
r = 1
g = 2
b = 3
```
