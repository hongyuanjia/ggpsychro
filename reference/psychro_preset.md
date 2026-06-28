# Apply a psychrometric chart preset

`psychro_preset()` returns a list of ggplot additions that can be added
to a
[`ggpsychro()`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro.md)
plot. Presets make selected reference grids explicit, configure grid
labels, and apply a matching psychrometric chart theme. The `"ashrae"`
and `"minimal"` presets are inspired by psychrochart's chart styles.

## Usage

``` r
psychro_preset(name = c("ashrae", "minimal"), labels = TRUE)
```

## Arguments

- name:

  A preset name. One of `"ashrae"` or `"minimal"`.

- labels:

  A single logical value. If `FALSE`, preset grid labels are hidden
  while grid visibility and theme styling are kept.

## Value

A list of ggplot additions.

## Examples

``` r
ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 30)) +
    psychro_preset("ashrae")


ggpsychro(tdb_lim = c(0, 50), hum_lim = c(0, 50)) +
    psychro_preset("minimal", labels = FALSE)

```
