# Transity Development

Generate and serve the Pursuit documentation with:

```sh
pulp docs -- --format html
cd generated-docs
python3 -m http.server 1222
```

Then open [localhost:1222](http://localhost:1222).


## Add Documentation File to Local Pursuit

**Not working. Format seems wrong,
but can't find code in pulp where it gets changed**

```sh
bower list --json --offline > resolutions.json
psc-publish --manifest bower.json --resolutions resolutions.json
```

```sh
mv resolutions.json ../pursuit/data/verified/transity
```


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

[asciinema]: https://github.com/asciinema/asciinema
[svg-term]: https://github.com/marionebl/svg-term-cli
