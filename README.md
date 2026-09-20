# WaDorule

**WaDorule** derives reservoir-specific daily release targets using a water-balance
approach. The user supplies the reservoir data and physical/operational
constraints; the algorithm derives the release target rather than requiring a
predefined release-target series.

## Installation

Install the development version from GitHub:

```r
remotes::install_github("waswapeter/WaDorule")
library(WaDorule)
```

## User inputs

The main function is:

```r
wad_wadorule(
  inflows,
  targets,
  Smin,
  Smax,
  Rmin,
  Rmax
)
```

### Inflows

`inflows` must contain:

- `Date`
- `inflow` (m3/s)

### Targets

`targets` must contain:

- `Date`
- `Storage` (MCM)
- `Elevation` (m)

The storage target is used by the water-balance calculation. Elevation is
carried through as the user-supplied target and returned in the output. No
generic elevation-to-storage relationship is assumed.

### Reservoir constraints

The user supplies:

- `Smin` — minimum storage
- `Smax` — maximum storage
- `Rmin` — minimum release
- `Rmax` — maximum release

The package does not read a reservoir constraints file and does not embed
reservoir-specific constraint values.

## Output

The function returns exactly four columns:

```text
Date
Storage_Target
Elevation_Target
Release_Target
```

`Release_Target` is derived internally from the water balance, storage-level
feedback, release bounds, and release-ramping rules.

Optional arguments can write the final output and diagnostic results to CSV and
can produce contextual plots when observed releases are supplied.

## Reproducible reservoir examples

The GitHub repository contains reservoir-specific input files and generated
reference outputs for selected reservoirs. These files are provided so users
can download them, run WaDorule in RStudio, and reproduce or inspect the
resulting release-target calculations.

The repository data are **not required by the package** and are not read
automatically by `wad_wadorule()`. Users can instead provide their own inflow
and target files and their own constraint values.

For a repository example, the workflow is:

```r
inflows <- readr::read_csv("Garrison_inflows.csv")
targets <- readr::read_csv("Garrison_targets.csv")

result <- wad_wadorule(
  inflows = inflows,
  targets = targets,
  Smin = 5910,
  Smax = 28928,
  Rmin = 142,
  Rmax = 917
)

head(result)
```

The generated release target can then be compared with the repository's
reference output for the corresponding reservoir. Such comparisons are
intended to examine the resulting operating-pattern relationships and
reproducibility; a reference series is not used as an input to the algorithm.

## Package design

WaDorule has one exported main function, `wad_wadorule()`. Reservoir-specific
inputs remain external to the algorithm so that the same function can be
applied to reservoirs with different inflow regimes, storage targets,
elevation targets, and constraints.
