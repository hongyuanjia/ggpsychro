# functions of calculating psychrometrics that are currently missing from psychrolib
# the reverse of psychrolib::GetMoistAirVolume
#' @importFrom psychrolib GetTRankineFromTFahrenheit GetTKelvinFromTCelsius isIP
GetHumRatioFromAirVolume <- function (TDryBulb, AirVolume, Pressure) {
    if (isIP()) {
        HumRatio <- (AirVolume * (144 * Pressure) / (psychrolib:::R_DA_IP * GetTRankineFromTFahrenheit(TDryBulb)) - 1) / 1.607858
    } else {
        HumRatio <- (AirVolume * Pressure / (psychrolib:::R_DA_SI * GetTKelvinFromTCelsius(TDryBulb)) - 1) / 1.607858
    }

    pmax(HumRatio, psychrolib:::PSYCHRO_OPT$MIN_HUM_RATIO)
}
