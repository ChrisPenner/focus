#!/bin/sh

# shellcheck disable=2016

focus --full -i '(/```focus\n\$ (?<command>.+?)\n(?<rest>.*?)```/ ) |= "```focus
$ %{ %command }
%{ -{%command} }
```"' examples.md
