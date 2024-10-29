Here are some examples of how to use `focus`.
Note that this example file is actually built and re-run using focus itself! Check out [build-examples.sh](./build-examples.sh) to see how!

# Text matching

Focus is _very_ good at matching and manipulating text. You can think of focus like 'jq' but for manipulating text!

Most selectors which work on text are _reversible_, which means you can use the `|=` operator to modify the selection and replace it with something else in the source text.

## Regex

You can use an `/`-delimited regex pattern to match text patterns. `focus` supports PCRE-compatible regex patterns.
It will search each provided input (by default, each line of the source text) and 
pass each match down the line to the next selector.

```focus
$ echo "Hello Jello World" | focus '/[HJ]ello/'
Hello
Jello
```

We can use `|=` to modify each match, mapping the new result back into the source text.

```focus
$ echo "Hello Jello World" | focus '/[HJ]ello/ |= concat [ reversed (chars) ] '
olleH olleJ World
```


```focus
$ echo "hello world" | focus '/hello/ |= "goodbye"'
goodbye world
```

Cool.






