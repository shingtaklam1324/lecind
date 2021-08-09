# lecind

A small command line tool for managing references in notes. It is mainly designed for (mathematics) lecture notes. The tag system is inspired by the [Stacks Project](https://stacks.math.columbia.edu/).

## Features

- Labels, with a small amount of meta data, as well as a unique hex tag (NB. I chose to use 5 base 16 instead of 4 base 35 like in the stacks project).
- References, which themselves have a tag. This allows for referencing, as well as seeing everything which refers to a given thing. Furthermore, references can link to references...
- Export/Import to/from JSON
- Basic command line interface
