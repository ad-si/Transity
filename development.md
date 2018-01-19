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
