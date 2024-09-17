# focus - cli utility for hacking and slashing data

**ALPHA**

`awk` can do anything, but it's not always the most intuitive tool to use. 

`focus` takes a different approach, similar to tools like `jq` and `xpath` it allows you to compose simple 'selectors' in a pipeline to
extract, transform and filter data, and provides easy inter-op with other unix tools so you can take advantage of the wealth of useful tools available.

## Examples

Keep forgetting the syntax for `sed`? No problem:

```sh
echo "hello world" | focus set '/hello/ | matches' goodbye
```

A key part of selectors in `focus` are that they work in both directions!
This means you can select a regex match, and then modify it in place! 

Let's use the `modify` command to find every word containing an 'o',
then capitalize and reverse those words, merging them back into the source text.

```sh
echo "celery watermelon apples carrots" | focus modify '/\w*[oO]\w*/ | matches' '{tr a-z A-Z | rev }'
celery NOLEMRETAW apples STORRAC
```
