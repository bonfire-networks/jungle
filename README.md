# jungle

Bonfire's forthcoming build tool.

## Installation

1. Install racket through your regular package management
2. Clone this repository to a relatively permanent home (pulling git will update it!)
3. Run `raco pkg install` in this directory

To set your shell up to use it, there are two options. You only need to do one.

### Shell alias

In your shell rc file, set an alias like this (changing the path to this directory)

  `alias jungle="racket $HOME/code/bonfire/jungle/main.rkt"`

### Wrapper script

Alternatively, you may create a wrapper script in a directory on your
path that looks like this instead of an alias:

```
#!/bin/sh
racket $HOME/code/bonfire/jungle/main.rkt $@
```

Make it executable with `chmod u+x jungle`

### Command reference

* `jungle git status` - overview of the current repositories 
* `jungle git fetch` - run `git fetch` across all forks in parallel

<!-- ## Background -->

<!-- Bonfire is a rather unique elixir project. We decided fairly early on -->
<!-- that the best way to empower users and communities was to make bonfire -->
<!-- the most flexible and customisable social media software in existence. -->

<!-- This is a good thing in theory, but the practice has -->
<!-- been... challenging.  The elixir ecosystem has a fairly rigid idea of -->
<!-- how projects work. It's not so dissimilar from the erlang ecosystem in -->
<!-- general, except elixir has greater metaprogramming capabilities than -->
<!-- erlang. -->

<!-- The main trick to this flexibility is configuration. We try to make -->
<!-- everything configurable - at compile time if need be. Unfortunately, -->
<!-- the only time mix can be guaranteed to provide the correct -->
<!-- configuration is when triggering a build of the main application. -->

<!-- Currently this is handled by a monster makefile that will do things -->
<!-- like delete built libraries to force mix to compile them again with -->
<!-- the application configuration. We'd like an easier option. -->

<!-- ## Solution: custom programmable build tool -->

<!-- ### Configuration language -->

<!-- Custom syntax (mostly stolen from clojure): -->

<!-- * `{}` - maps -->
<!-- * `#{}` - sets -->
<!-- * `:foo` - keywords -->
<!-- * `foo` - symbols -->
<!-- * -->

## Copyright and License

Copyright 2022 James Laver, bonfire contributors

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
