#ifndef GGPSYCHRO_H
#define GGPSYCHRO_H

#include <R.h>
#include <Rinternals.h>

/* Internal SI psychrometric helpers shared by comfort.c and psychro-extra.c. */
double ggpsy_sat_vap_pres_si(double tdb);
double ggpsy_hum_ratio_from_vap_pres_si(double vap_pres, double pressure,
                                        double min_hum_ratio);
double ggpsy_vap_pres_from_hum_ratio_si(double hum_ratio, double pressure,
                                        double min_hum_ratio);
double ggpsy_dew_point_from_vap_pres_si(double vap_pres);
double ggpsy_dew_point_from_hum_ratio_si(double hum_ratio, double pressure,
                                         double min_hum_ratio);
double ggpsy_relhum_from_hum_ratio_si(double tdb, double hum_ratio,
                                      double pressure, double min_hum_ratio);
double ggpsy_sat_hum_ratio_si(double tdb, double pressure,
                              double min_hum_ratio);

#endif
