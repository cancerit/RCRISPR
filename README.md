---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# rcrispr

<!-- badges: start -->

[![cancerit](https://circleci.com/gh/cancerit/RCRISPR.svg?style=svg)](https://circleci.com/gh/cancerit/RCRISPR)

<!-- badges: end -->

The goal of rcrispr is to ...

## Installation

You can install the released version of rcrispr from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rcrispr")
```

## Example

This is a basic example which shows you how to solve a common problem:


```r
library(rcrispr)
#> Error in library(rcrispr): there is no package called 'rcrispr'
## basic example code
```

## Development

### Keeping Rmd and md in sync

You should not edit `README.md`, only `README.Rmd`.  Once edited you can update with:

```bash
# native R
r -e 'library(knitr); knit("README.Rmd", "README.md")'

# docker
docker run -ti --rm -v $PWD:/md:rw rocker/r-rmd r -e 'library(knitr); knit("/md/README.Rmd", "/md/README.md")'
```

### Updating licence headers

Please use [skywalking-eyes](https://github.com/apache/skywalking-eyes).

Expected workflow:

```bash
# recent build, change to apache/skywalking-eyes:0.2.0 once released
export DOCKER_IMG=ghcr.io/apache/skywalking-eyes/license-eye@sha256:17a4e86591c9908c8e76531943d5522881a82a33580ad7bdf36938517ef25aa9
```

1. Check state before modifying `.licenserc.yaml`:
   - `docker run -it --rm -v $(pwd):/github/workspace $DOCKER_IMG header check`
   - You should get some 'valid' here, those without a header as 'invalid'
1. Modify `.licenserc.yaml`
1. Apply the changes:
   - `docker run -it --rm -v $(pwd):/github/workspace $DOCKER_IMG header fix`
1. Add/commit changes

The check is executed in the CI pipeline which will fail if expected files are missing the license.

*DO NOT* edit the header in the files, please modify the date component of `content` in `.licenserc.yaml`.  The only files needing manual update being:

- `README.Rmd`

If you need to make more extensive changes to the license carefully test the pattern is functional.

## LICENSE

```
Copyright (c) 2021 Genome Research Ltd

Author: CASM/Cancer IT <cgphelp@sanger.ac.uk>

This file is part of RCRISPR.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

1. The usage of a range of years within a copyright statement contained within
this distribution should be interpreted as being equivalent to a list of years
including the first and last year specified and all consecutive years between
them. For example, a copyright statement that reads ‘Copyright (c) 2005, 2007-
2009, 2011-2012’ should be interpreted as being identical to a statement that
reads ‘Copyright (c) 2005, 2007, 2008, 2009, 2011, 2012’ and a copyright
statement that reads ‘Copyright (c) 2005-2012’ should be interpreted as being
identical to a statement that reads ‘Copyright (c) 2005, 2006, 2007, 2008,
2009, 2010, 2011, 2012’.
```
