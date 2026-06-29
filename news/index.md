# Changelog

## ggpsychro 0.0.0.9000

- Preserved ordinary ggplot2 aesthetics across legacy psychrometric
  stats. ([\#27](https://github.com/hongyuanjia/ggpsychro/issues/27))
- Replaced foreground psychrometric panel masking with valid-region
  clipping for ggpsychro-generated grids, stats, tiles, zones, and
  comfort overlays, while allowing ordinary ggplot layers to draw in the
  mask area and state markers to render above the saturation boundary.
  ([\#4](https://github.com/hongyuanjia/ggpsychro/issues/4),
  [\#28](https://github.com/hongyuanjia/ggpsychro/issues/28))
- Added Heat Index overlays, Givoni bioclimatic strategy zones, and
  per-zone styling controls for Givoni overlays.
  ([\#24](https://github.com/hongyuanjia/ggpsychro/issues/24))
- Added optional contour labels to
  [`geom_comfort_contour()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_comfort_overlay.md).
  ([\#26](https://github.com/hongyuanjia/ggpsychro/issues/26))
- Clarified SET and adaptive comfort overlay examples in the comfort
  overlays article.
  ([\#25](https://github.com/hongyuanjia/ggpsychro/issues/25))
- Added README acknowledgements for external psychrometric-chart
  references and refined SET/adaptive comfort overlay examples.
  ([\#23](https://github.com/hongyuanjia/ggpsychro/issues/23))
- Added README and pkgdown documentation for comfort overlays and the
  psychrometric protractor.
  ([\#22](https://github.com/hongyuanjia/ggpsychro/issues/22))
- Added an ASHRAE-style psychrometric protractor for sensible heat ratio
  and heat-moisture-ratio guides, including Mollier rotation support,
  overall scaling, and independent mask-area margins.
  ([\#21](https://github.com/hongyuanjia/ggpsychro/issues/21))
- Added thermal comfort calculations and comfort overlay layers for
  PMV/PPD, SET, adaptive comfort, PMV curves, PMV-based ASHRAE 55 / EN
  15251 comfort zones, and point-state comfort metrics.
  ([\#20](https://github.com/hongyuanjia/ggpsychro/issues/20))
- Removed an ambiguous `specvol-rh` zone example from the reference
  documentation.
  ([\#19](https://github.com/hongyuanjia/ggpsychro/issues/19))
- Clipped
  [`geom_psychro_tile()`](https://hongyuanjia.github.io/ggpsychro/reference/stat_psychro_bin.md)
  bodies to the saturation curve so tiles stay inside the valid
  psychrometric region.
  ([\#17](https://github.com/hongyuanjia/ggpsychro/issues/17))
- Added runnable core reference examples for psychrometric stats, grid
  helpers, state/process layers, zones, and explicit coordinates.
  ([\#18](https://github.com/hongyuanjia/ggpsychro/issues/18))
- Added website-only pkgdown articles and shortened the README.
  ([\#15](https://github.com/hongyuanjia/ggpsychro/issues/15))
- Added psychrometric tile bins.
  ([\#14](https://github.com/hongyuanjia/ggpsychro/issues/14))
- Added psychrometric chart presets.
  ([\#12](https://github.com/hongyuanjia/ggpsychro/issues/12))
- Added psychrometric zones and process layers.
  ([\#13](https://github.com/hongyuanjia/ggpsychro/issues/13))
- Rendered psychrometric grid labels.
  ([\#11](https://github.com/hongyuanjia/ggpsychro/issues/11))
- Revived the dual psychrometric grid API.
  ([\#10](https://github.com/hongyuanjia/ggpsychro/issues/10))
