# Transity Development

## Getting Started

Check out the `makefile` for all available tasks.
By simply running `make` it will also print a short list of them.


**Additional helpful commands:**

Build and run:

```shell
npx spago run --exec-args 'balance test/test.yaml'
```

Make `transity` executable available in your path:

```shell
npm link
```

All modifications to the source code (after building it)
will now be available via the linked `transity` executable.


## Documentation

Generate and serve the Pursuit documentation with:

```sh
npx spago docs
cd generated-docs/html
python3 -m http.server 1222
```

Then open [localhost:1222](http://localhost:1222).


Install [markdown-toc] with npm and run following command
to update the table of contents in the readme:

```shell
markdown-toc -i readme.md
```

[markdown-toc]: https://github.com/jonschlinkert/markdown-toc


## XLSX Generation

Check out https://stackoverflow.com/q/18334314/1850340 for an explanation
of the XML fields.


## Generate Screenshots

Use [asciinema] to generate the terminal recording:

```shell
asciinema rec \
  --title 'Transity' \
  recording.json
```

Change the size of the terminal in the `recording.json` file to
approximately

```json
  "width": 80,
  "height": 18,
```


Then use [svg-term] to generate the SVG image:

```shell
svg-term \
  --no-cursor \
  --at 99999 \
  --window \
  --term iterm2 \
  --profile ../../dotfiles/terminal/adius.itermcolors \
  < recording.json \
  > recording.svg
```


And lastly convert all CSS styles to inline styles
because of issue https://github.com/marionebl/svg-term-cli/issues/5

[asciinema]: https://github.com/asciinema/asciinema
[svg-term]: https://github.com/marionebl/svg-term-cli
