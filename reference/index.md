# Package index

## Package overview

- [`ggpsychro-package`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro-package.md)
  : ggpsychro: A 'ggplot2' Extension for Making Psychrometric Charts

## Core chart construction

- [`ggpsychro()`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro.md)
  : Create a ggpsychro plot
- [`coord_psychro()`](https://hongyuanjia.github.io/ggpsychro/reference/coord_psychro.md)
  : Psychrometric coordinates
- [`is.ggpsychro()`](https://hongyuanjia.github.io/ggpsychro/reference/is.ggpsychro.md)
  : Reports whether x is a ggplot object

## Psychrometric equation layers and grids

- [`stat_relhum()`](https://hongyuanjia.github.io/ggpsychro/reference/stat.md)
  [`stat_wetbulb()`](https://hongyuanjia.github.io/ggpsychro/reference/stat.md)
  [`stat_vappres()`](https://hongyuanjia.github.io/ggpsychro/reference/stat.md)
  [`stat_specvol()`](https://hongyuanjia.github.io/ggpsychro/reference/stat.md)
  [`stat_enthalpy()`](https://hongyuanjia.github.io/ggpsychro/reference/stat.md)
  : Calculate psychrometric properties of moist air
- [`geom_grid_relhum()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_grid.md)
  [`geom_grid_wetbulb()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_grid.md)
  [`geom_grid_vappres()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_grid.md)
  [`geom_grid_specvol()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_grid.md)
  [`geom_grid_enthalpy()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_grid.md)
  : Add psychrometric grid lines
- [`geom_psychro_protractor()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_psychro_protractor.md)
  [`guide_psychro_protractor()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_psychro_protractor.md)
  : Add a psychrometric protractor

## Psychrometric data layers

- [`geom_psychro_process()`](https://hongyuanjia.github.io/ggpsychro/reference/psychro_state.md)
  [`stat_psychro_state()`](https://hongyuanjia.github.io/ggpsychro/reference/psychro_state.md)
  : Draw psychrometric state points or process lines
- [`geom_psychro_zone()`](https://hongyuanjia.github.io/ggpsychro/reference/psychro_zone.md)
  [`stat_psychro_zone()`](https://hongyuanjia.github.io/ggpsychro/reference/psychro_zone.md)
  : Draw psychrometric zones
- [`stat_psychro_bin()`](https://hongyuanjia.github.io/ggpsychro/reference/stat_psychro_bin.md)
  [`geom_psychro_tile()`](https://hongyuanjia.github.io/ggpsychro/reference/stat_psychro_bin.md)
  : Bin data on psychrometric chart coordinates

## Thermal comfort calculations and models

- [`comfort_pmv()`](https://hongyuanjia.github.io/ggpsychro/reference/comfort_pmv.md)
  [`comfort_set()`](https://hongyuanjia.github.io/ggpsychro/reference/comfort_pmv.md)
  [`comfort_adaptive()`](https://hongyuanjia.github.io/ggpsychro/reference/comfort_pmv.md)
  [`comfort_heat_index()`](https://hongyuanjia.github.io/ggpsychro/reference/comfort_pmv.md)
  : Thermal comfort calculations
- [`comfort_model_pmv()`](https://hongyuanjia.github.io/ggpsychro/reference/comfort_model_pmv.md)
  [`comfort_model_set()`](https://hongyuanjia.github.io/ggpsychro/reference/comfort_model_pmv.md)
  [`comfort_model_adaptive()`](https://hongyuanjia.github.io/ggpsychro/reference/comfort_model_pmv.md)
  [`comfort_model_heat_index()`](https://hongyuanjia.github.io/ggpsychro/reference/comfort_model_pmv.md)
  : Comfort model objects
- [`comfort_standard_ashrae55_2017()`](https://hongyuanjia.github.io/ggpsychro/reference/comfort_standard_ashrae55_2017.md)
  [`comfort_standard_en15251_2007()`](https://hongyuanjia.github.io/ggpsychro/reference/comfort_standard_ashrae55_2017.md)
  : PMV-based comfort standards
- [`comfort_strategy_givoni()`](https://hongyuanjia.github.io/ggpsychro/reference/comfort_strategy_givoni.md)
  : Givoni bioclimatic strategy

## Thermal comfort chart layers

- [`geom_comfort_overlay()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_comfort_overlay.md)
  [`geom_comfort_heat_index()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_comfort_overlay.md)
  [`geom_comfort_contour()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_comfort_overlay.md)
  [`geom_comfort_zone()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_comfort_overlay.md)
  [`geom_comfort_pmv_lines()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_comfort_overlay.md)
  [`geom_comfort_standard_zone()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_comfort_overlay.md)
  [`geom_comfort_givoni()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_comfort_overlay.md)
  [`stat_comfort_state()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_comfort_overlay.md)
  : Comfort overlays for psychrometric charts
- [`scale_fill_comfort_pmv()`](https://hongyuanjia.github.io/ggpsychro/reference/scale_fill_comfort_pmv.md)
  : Comfort PMV fill scale

## Scales, labels, and transformations

- [`scale_drybulb_continuous()`](https://hongyuanjia.github.io/ggpsychro/reference/scale.md)
  [`scale_humratio_continuous()`](https://hongyuanjia.github.io/ggpsychro/reference/scale.md)
  [`scale_relhum_continuous()`](https://hongyuanjia.github.io/ggpsychro/reference/scale.md)
  [`scale_wetbulb_continuous()`](https://hongyuanjia.github.io/ggpsychro/reference/scale.md)
  [`scale_vappres_continuous()`](https://hongyuanjia.github.io/ggpsychro/reference/scale.md)
  [`scale_specvol_continuous()`](https://hongyuanjia.github.io/ggpsychro/reference/scale.md)
  [`scale_enthalpy_continuous()`](https://hongyuanjia.github.io/ggpsychro/reference/scale.md)
  : Transformation object for psychrometric chart
- [`label_drybulb()`](https://hongyuanjia.github.io/ggpsychro/reference/label.md)
  [`label_humratio()`](https://hongyuanjia.github.io/ggpsychro/reference/label.md)
  [`label_relhum()`](https://hongyuanjia.github.io/ggpsychro/reference/label.md)
  [`label_wetbulb()`](https://hongyuanjia.github.io/ggpsychro/reference/label.md)
  [`label_vappres()`](https://hongyuanjia.github.io/ggpsychro/reference/label.md)
  [`label_specvol()`](https://hongyuanjia.github.io/ggpsychro/reference/label.md)
  [`label_enthalpy()`](https://hongyuanjia.github.io/ggpsychro/reference/label.md)
  [`drybulb_format()`](https://hongyuanjia.github.io/ggpsychro/reference/label.md)
  [`humratio_format()`](https://hongyuanjia.github.io/ggpsychro/reference/label.md)
  [`relhum_format()`](https://hongyuanjia.github.io/ggpsychro/reference/label.md)
  [`wetbulb_format()`](https://hongyuanjia.github.io/ggpsychro/reference/label.md)
  [`vappres_format()`](https://hongyuanjia.github.io/ggpsychro/reference/label.md)
  [`specvol_format()`](https://hongyuanjia.github.io/ggpsychro/reference/label.md)
  [`enthalpy_format()`](https://hongyuanjia.github.io/ggpsychro/reference/label.md)
  : Label wet-bulb temperature
- [`drybulb_trans()`](https://hongyuanjia.github.io/ggpsychro/reference/trans.md)
  [`humratio_trans()`](https://hongyuanjia.github.io/ggpsychro/reference/trans.md)
  [`relhum_trans()`](https://hongyuanjia.github.io/ggpsychro/reference/trans.md)
  [`wetbulb_trans()`](https://hongyuanjia.github.io/ggpsychro/reference/trans.md)
  [`vappres_trans()`](https://hongyuanjia.github.io/ggpsychro/reference/trans.md)
  [`specvol_trans()`](https://hongyuanjia.github.io/ggpsychro/reference/trans.md)
  [`enthalpy_trans()`](https://hongyuanjia.github.io/ggpsychro/reference/trans.md)
  : Create transformation objects for psychrometric chart
- [`demo_scale()`](https://hongyuanjia.github.io/ggpsychro/reference/demo_scale.md)
  : Demonstrate scales functions with ggplot2 code

## Themes, presets, and elements

- [`theme_grey_psychro()`](https://hongyuanjia.github.io/ggpsychro/reference/theme.md)
  [`theme_gray_psychro()`](https://hongyuanjia.github.io/ggpsychro/reference/theme.md)
  [`theme_psychro()`](https://hongyuanjia.github.io/ggpsychro/reference/theme.md)
  [`theme_psychro_ashrae()`](https://hongyuanjia.github.io/ggpsychro/reference/theme.md)
  [`theme_psychro_minimal()`](https://hongyuanjia.github.io/ggpsychro/reference/theme.md)
  : Custom theme for psychrometric chart.
- [`psychro_preset()`](https://hongyuanjia.github.io/ggpsychro/reference/psychro_preset.md)
  : Apply a psychrometric chart preset
- [`element_polygon()`](https://hongyuanjia.github.io/ggpsychro/reference/element_polygon.md)
  : Polygon theme element for psychrometric chart panels
- [`element_comfort_zone()`](https://hongyuanjia.github.io/ggpsychro/reference/element_comfort_zone.md)
  : Comfort zone style element

## Extension API

- [`ggpsychro-extensions`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro-extensions.md)
  [`StatRelhum`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro-extensions.md)
  [`StatWetbulb`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro-extensions.md)
  [`StatVappres`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro-extensions.md)
  [`StatSpecvol`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro-extensions.md)
  [`StatEnthalpy`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro-extensions.md)
  [`StatPsychroBin`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro-extensions.md)
  [`StatPsychroState`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro-extensions.md)
  [`StatPsychroZone`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro-extensions.md)
  : ggpsychro extensions to ggplot2
- [`ggpsychro-ggproto`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro-ggproto.md)
  [`CoordPsychro`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro-ggproto.md)
  : Base ggproto classes for ggpsychro
