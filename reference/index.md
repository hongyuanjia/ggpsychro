# Package index

## Package overview

- [`ggpsychro-package`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro-package.md)
  : ggpsychro: psychrometric charts with ggplot2

## Core chart construction

- [`ggpsychro()`](https://hongyuanjia.github.io/ggpsychro/reference/ggpsychro.md)
  : Create a ggpsychro plot
- [`is_ggpsychro()`](https://hongyuanjia.github.io/ggpsychro/reference/is_ggpsychro.md)
  : Test for ggpsychro plots

## Advanced chart coordinates

- [`coord_psychro()`](https://hongyuanjia.github.io/ggpsychro/reference/coord_psychro.md)
  : Advanced psychrometric coordinates

## Psychrometric equation layers and grids

- [`stat_relhum()`](https://hongyuanjia.github.io/ggpsychro/reference/stat.md)
  [`stat_wetbulb()`](https://hongyuanjia.github.io/ggpsychro/reference/stat.md)
  [`stat_vappres()`](https://hongyuanjia.github.io/ggpsychro/reference/stat.md)
  [`stat_specvol()`](https://hongyuanjia.github.io/ggpsychro/reference/stat.md)
  [`stat_enthalpy()`](https://hongyuanjia.github.io/ggpsychro/reference/stat.md)
  : Draw constant-property psychrometric data
- [`geom_psychro_grid_relhum()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_psychro_grid.md)
  [`geom_psychro_grid_wetbulb()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_psychro_grid.md)
  [`geom_psychro_grid_vappres()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_psychro_grid.md)
  [`geom_psychro_grid_specvol()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_psychro_grid.md)
  [`geom_psychro_grid_enthalpy()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_psychro_grid.md)
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
- [`comfort_pmv_ashrae55()`](https://hongyuanjia.github.io/ggpsychro/reference/comfort_pmv_ashrae55.md)
  [`comfort_pmv_en15251()`](https://hongyuanjia.github.io/ggpsychro/reference/comfort_pmv_ashrae55.md)
  : PMV-based comfort standards
- [`comfort_strategy_givoni()`](https://hongyuanjia.github.io/ggpsychro/reference/comfort_strategy_givoni.md)
  : Givoni-Milne strategy overlay

## Thermal comfort chart layers

- [`geom_comfort_pmv()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_comfort_pmv.md)
  : Draw PMV comfort layers
- [`geom_comfort_set()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_comfort_set.md)
  : Draw SET comfort layers
- [`geom_comfort_heat_index()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_comfort_heat_index.md)
  : Draw heat-index comfort categories
- [`geom_comfort_adaptive()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_comfort_adaptive.md)
  : Draw adaptive comfort zones
- [`geom_comfort_givoni()`](https://hongyuanjia.github.io/ggpsychro/reference/geom_comfort_givoni.md)
  : Draw Givoni-Milne strategy zones
- [`stat_comfort_state()`](https://hongyuanjia.github.io/ggpsychro/reference/stat_comfort_state.md)
  : Evaluate comfort metrics at state points
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
  : Psychrometric continuous scales
- [`label_drybulb()`](https://hongyuanjia.github.io/ggpsychro/reference/label.md)
  [`label_humratio()`](https://hongyuanjia.github.io/ggpsychro/reference/label.md)
  [`label_relhum()`](https://hongyuanjia.github.io/ggpsychro/reference/label.md)
  [`label_wetbulb()`](https://hongyuanjia.github.io/ggpsychro/reference/label.md)
  [`label_vappres()`](https://hongyuanjia.github.io/ggpsychro/reference/label.md)
  [`label_specvol()`](https://hongyuanjia.github.io/ggpsychro/reference/label.md)
  [`label_enthalpy()`](https://hongyuanjia.github.io/ggpsychro/reference/label.md)
  : Label psychrometric scale breaks
- [`drybulb_trans()`](https://hongyuanjia.github.io/ggpsychro/reference/trans.md)
  [`humratio_trans()`](https://hongyuanjia.github.io/ggpsychro/reference/trans.md)
  [`relhum_trans()`](https://hongyuanjia.github.io/ggpsychro/reference/trans.md)
  [`wetbulb_trans()`](https://hongyuanjia.github.io/ggpsychro/reference/trans.md)
  [`vappres_trans()`](https://hongyuanjia.github.io/ggpsychro/reference/trans.md)
  [`specvol_trans()`](https://hongyuanjia.github.io/ggpsychro/reference/trans.md)
  [`enthalpy_trans()`](https://hongyuanjia.github.io/ggpsychro/reference/trans.md)
  : Create transformation objects for psychrometric chart
- [`demo_scale()`](https://hongyuanjia.github.io/ggpsychro/reference/demo_scale.md)
  : Demonstrate psychrometric label and scale functions

## Themes, presets, and elements

- [`theme_grey_psychro()`](https://hongyuanjia.github.io/ggpsychro/reference/theme_psychro.md)
  [`theme_gray_psychro()`](https://hongyuanjia.github.io/ggpsychro/reference/theme_psychro.md)
  [`theme_psychro()`](https://hongyuanjia.github.io/ggpsychro/reference/theme_psychro.md)
  [`theme_psychro_ashrae()`](https://hongyuanjia.github.io/ggpsychro/reference/theme_psychro.md)
  [`theme_psychro_minimal()`](https://hongyuanjia.github.io/ggpsychro/reference/theme_psychro.md)
  : Custom theme for psychrometric chart.
- [`psychro_preset()`](https://hongyuanjia.github.io/ggpsychro/reference/psychro_preset.md)
  : Apply a psychrometric chart preset
- [`element_givoni_zone()`](https://hongyuanjia.github.io/ggpsychro/reference/element_givoni_zone.md)
  : Comfort zone style element
