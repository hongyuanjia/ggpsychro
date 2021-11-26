# functions of calculating psychrometrics that are currently missing from psychrolib
# the reverse of psychrolib::GetMoistAirVolume
#' @importFrom psychrolib GetTRankineFromTFahrenheit GetTKelvinFromTCelsius isIP
GetHumRatioFromAirVolume <- function (TDryBulb, AirVolume, Pressure) {
        r_da <- get("R_DA_IP", envir = asNamespace("psychrolib"), inherits = FALSE)
    if (isIP()) {
        HumRatio <- (AirVolume * (144 * Pressure) / (r_da * GetTRankineFromTFahrenheit(TDryBulb)) - 1) / 1.607858
    } else {
        r_da <- get("R_DA_IP", envir = asNamespace("psychrolib"), inherits = FALSE)
        HumRatio <- (AirVolume * Pressure / (r_da * GetTKelvinFromTCelsius(TDryBulb)) - 1) / 1.607858
    }

    pmax(HumRatio, psy_op$MIN_HUM_RATIO)
}

GetTDewPointFromHumRatioOnly <- function(HumRatio, Pressure) {
    VapPres <- psychrolib::GetVapPresFromHumRatio(HumRatio, Pressure)
    if (isIP()) {
        CONST <- 6894.7572931783
        # convert to SI
        VapPres <- VapPres * CONST
    }
    TDewPoint <- vapply(VapPres, GetTDewPointFromVapPresOnly, double(1L))

    if (isIP()) {
        TDewPoint <- TDewPoint * 9.0 / 5.0 + 32.0
    }

    TDewPoint
}

GetTDewPointFromVapPresOnly <- function(VapPres) {
    if (VapPres >= 1555000.0) {
        TSat <- 200.0
    } else if (VapPres <= 0.0017) {
        TSat <- -100.0
    } else if (VapPres > 611.0 && VapPres < 611.25) {
        TSat <- 0.0
    } else {
        TSat <- 100.0
        TSat <- stats::uniroot(
            function(TSat) with_units("SI", psychrolib::GetSatVapPres(TSat)) - VapPres,
            interval = c(-100, 200), tol = 1E-4, maxiter = 1000
        )$root
    }

    TSat
}

GetTDryBulbFromMoistAirVolumeAndHumRatio <- function(MoistAirVolume, HumRatio, Pressure) {
    if (psychrolib::isIP()) {
        R_DA_IP <- get("R_DA_IP", envir = asNamespace("psychrolib"), inherits = FALSE)
        psychrolib::GetTFahrenheitFromTRankine(MoistAirVolume * (144 * Pressure) / (R_DA_IP * (1 + 1.607858 * HumRatio)))
    } else {
        R_DA_SI <- get("R_DA_SI", envir = asNamespace("psychrolib"), inherits = FALSE)
        psychrolib::GetTCelsiusFromTKelvin(MoistAirVolume * Pressure / (R_DA_SI * (1 + 1.607858 * HumRatio)))
    }
}

GetHumRatioFromMoistAirVolumeAndTDryBulb <- function(MoistAirVolume, TDryBulb, Pressure) {
    if (psychrolib::isIP()) {
        R_DA_IP <- get("R_DA_IP", envir = asNamespace("psychrolib"), inherits = FALSE)
        (MoistAirVolume * (144 * Pressure) /psychrolib::GetTRankineFromTFahrenheit(TDryBulb) / R_DA_IP - 1L) / 1.607858
    } else {
        R_DA_SI <- get("R_DA_SI", envir = asNamespace("psychrolib"), inherits = FALSE)
        (MoistAirVolume * Pressure / psychrolib::GetTKelvinFromTCelsius(TDryBulb) / R_DA_SI - 1L) / 1.607858
    }
}

GetHumRatioFromEnthalpyAndTDryBulb <- function(MoistAirEnthalpy, TDryBulb) {
    if (psychrolib::isIP()) {
        HumRatio <- (MoistAirEnthalpy - 0.24 * TDryBulb)/(1061 + 0.444 * TDryBulb)
    }
    else {
        HumRatio <- (MoistAirEnthalpy/1000 - 1.006 * TDryBulb)/(2501 + 1.86 * TDryBulb)
    }
}
