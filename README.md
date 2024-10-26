# focus - cli utility for hacking and slashing data

**ALPHA** - Very experimental and will change from day to day, don't expect anything to work, it's not stable.

`focus` is a flexible tool which helps you to **select** data from stream of text,
<!-- toc GFM -->

* [Examples](#examples)
    * [The Basics](#the-basics)
    * [Using focus as a replacement for `sed`](#using-focus-as-a-replacement-for-sed)
    * [Using focus as a replacement for `grep`](#using-focus-as-a-replacement-for-grep)
* [Anatomy of Focus](#anatomy-of-focus)
* [The Subcommands](#the-subcommands)
* [Selectors and Actions](#selectors-and-actions)
    * [Selectors](#selectors)
    * [Actions](#actions)
* [TODO](#todo)

<!-- tocstop -->
process and modify that data, then **re-merge** the back into the source text.

Incidentally it also acts as a flexible alternative to sed, grep, awk, cut, and other common unix utilities.

## Examples

### The Basics

To get a sense of what this can do, let's start with a simple example.

Let's say we've got a markdown document, and we want to UPPERCASE all the headings!

`tr a-z A-Z` is what most people would reach for, but that would capitalize the ENTIRE file.

`focus` allows us to single out the chunks of the file we care about, and modify them in place using any of our favourite utilities.

Let's see how it works, then we'll break it down.

```focus
document=$(cat <<EOF
# hello world
Here is our markdown document.

## this is a subheading
This is a paragraph.
EOF
)
echo "$document" | focus modify 'filter /^#/' '{ tr a-z A-Z }'
# HELLO WORLD
Here is our markdown document.

## THIS IS A SUBHEADING
This is a paragraph.
```

```
echo "$document" | focus modify 'filter /^#/' '{ tr a-z A-Z }'
┬───────────────
╰╸ Firstly we use echo to pipe the document into focus via stdin. 
   focus uses stdin by default, but you can also pass file names if you like.
```

```
echo "$document" | focus modify 'filter /^#/' '{ tr a-z A-Z }'
                   ┬───────────
                   ╰╸ Next we invoke focus with the 'modify' subcommand.
                      This indicates we'd like to focus something and run an action
                      to modify it, then re-merge it back into the source text
                      and output the combined result
```

```
echo "$document" | focus modify 'filter /^#/' '{ tr a-z A-Z }'
                                ┬────────────
                                ╰╸ Here we define a *selector* to select the lines we care about.
                                   By default, focus processes input one line at a time.
                                   You can pass the --full flag if you'd prefer to process
                                   the entire input at once.
                                   Anything between /slashes/ is a regex pattern.
                                   This will focus all lines which start with a '#'
```

```
echo "$document" | focus modify 'filter /^#/' '{ tr a-z A-Z }'
                                               ┬─────────────
                                               ╰╸ Finally, we define the *action* we'd like to take.
                                                  We can invoke arbitrary shell expressions inside '{}',
                                                  Any focused lines will be piped into this expression.
                                                  The output will be fed backwards through the selector
                                                  and merged back into the source.
```


Oh, that's quite intense! 
Let's alter our invocation to instead **focus** on the first letter of each word using the regex `\b` word boundary marker.
Notice that this time we focus the match of the regex itself, rather than using `filter` to filter lines which match.
Since we're focusing on only the first character of each word, `tr` will now only capitalize our headings instead of uppercasing them.

```focus
echo "$document" | focus modify 'filter /^#/ | /\b\w/' '{ tr a-z A-Z }'
# Hello World
Here Is Our Markdown Document.

## This Is A Subheading
This Is A Paragraph.
```

Much better!

### Using focus as a replacement for `sed`

Keep forgetting the syntax for `sed`? No problem!

You can use a simple regex match to select text and the `set` sub command to replace it with a new string.

```
echo "hello world" | focus set '/hello/' 'goodbye'
goodbye world
```

### Using focus as a replacement for `grep`

Instead of replacing text with `set`, we can use `view` to print only the focused text.

```
echo "he11o  w0rld" | focus view '/\d+/'
11
0
```

## Anatomy of Focus

## The Subcommands

1. `focus modify <selector> <action>` 

`modify` will use the selector to focus on parts of the input, then will process that data through the action, 
and will re-merge it back into the source text.

2. `focus set <selector> <replacement>`

`set` will use the selector to focus on parts of the input, then will replace that data with the replacement string.

`set` is really just a special case of `modify` where the action is to replace the focused text with a static string.

3. `focus view <selector>`

`view` will use the selector to focus on parts of the input, then will print that data to stdout.

## Selectors and Actions

Selectors and actions are the core of `focus`. They are the building blocks which allow you to select and manipulate text.

### Selectors

Selectors are pipelines composed of one or more **reversible pieces**. Every selector knows not only how to 
select some piece of data, but also how to re-compose the data it selected it from after it has been modified.

E.g. 
`splitOn ","` will select chunks of text separated by commas, and will re-merge the result back into the source text by interspersing the results with commas.
`filter "/regex/"` will only process the elements which match the regex, and will insert the altered results back into the source text in the same position.

Every selector MUST be reversible, such that if the modify command leaves the input untouched the result should match the source text.

### Actions

Actions are a super-set of selectors which don't need to be reversible. 

The `view` command doesn't need to re-merge the data back into the source text, so it can use non-reversible actions rather than a selector.
The `modify` command takes a selector as its first argument, but also takes an **action**, which can be used to perform arbitrary transformations 
before feeding the result back through the selector.

## TODO

- [ ] Allow defining custom selectors and actions in lua
  - Allow sharing these/caching them, etc.
- [ ] Fix or warn on cases which trigger unsafePartsOf (e.g. changing number of elements in returned list)
- [ ] Fix parsing ambiguity of division and regex
- [ ] Fix casting, e.g. `echo 1,2 | focus modify '[ splitOn "," ] | !(at 0) + !(at 1)' '%{.}'`
- [x] ~Maybe remove distinction between selectors and actions, just have selectors, but allow wrapping in a "forward" or "backward" action.~
- [ ] any/all
- [x] ~Math, boolean ops~
- [ ] Add 'index' selector
- [ ] Generate help and examples from source
- [ ] ability to zip/cartesian product selectors (for paste-like things)
- [ ] figure out how to typecheck JSON union types
- [ ] allow failing exit code on view on non-matches
- [x] ~Switch to input file arguments~
- [x] ~In-place editing~
- [x] ~Make expression language and selectors more consistent and distinct~
- [x] ~Ability to 'NOT'/invert filters~
- [ ] Add more examples
- [ ] Add more tests
- [ ] README tests
- [ ] Support JSON
- [x] ~List splat~
- [x] ~Expression language~
- [x] ~Require reversible selectors~
- [ ] Handle errors from shell commands better
- [ ] shell 'if'
- [ ] Fix annoying ansi codes coming from diagnose even on no-color mode.
- [x] ~Fix printing of "STVar" in type errors, e.g. `focus view '/one/ | ...'`~
