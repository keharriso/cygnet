# Cygnet

An impure functional language inspired by Haskell and compiled to C.

## Example

```lua
# Hello, world!

include "stdio.h"

export foreign main : int
main = do
    puts "Hello, world!"
    return 0
```

## Status

This project is in its infancy. It is a hobby project for its author. Maybe one day we can write non-trivial programs in it, but that day is not today.

## License

Copyright (c) 2023 Kevin Harrison, released under the MIT License (see LICENSE for details).
