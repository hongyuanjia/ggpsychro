# functions of calculating psychrometrics that are currently missing from psychrolib
# the reverse of psychrolib::GetMoistAirVolume
#' @importFrom psychrolib GetTRankineFromTFahrenheit GetTKelvinFromTCelsius isIP
GetHumRatioFromAirVolume <- function (TDryBulb, AirVolume, Pressure) {
    if (isIP()) {
        r_da <- get("R_DA_IP", envir = asNamespace("psychrolib"), inherits = FALSE)
        HumRatio <- (AirVolume * (144 * Pressure) / (r_da * GetTRankineFromTFahrenheit(TDryBulb)) - 1) / 1.607858
    } else {
        r_da <- get("R_DA_IP", envir = asNamespace("psychrolib"), inherits = FALSE)
        HumRatio <- (AirVolume * Pressure / (r_da * GetTKelvinFromTCelsius(TDryBulb)) - 1) / 1.607858
    }

    pmax(HumRatio, psy_op$MIN_HUM_RATIO)
}
