#' Sodium Adsorption Ratio (SCAR) Calculation
#'
#' @param df Dataframe containing necessary variables (Na, Ca).
#' @param convert_to_meq Logical, indicating whether to convert concentrations to meq/L (default: TRUE).
#' @return SCAR value.
#'
SCAR <- function(df, convert_to_meq = TRUE) {
  # Conversion factors for mg/L to meq/L
  conversion_factor_Na <- 1 / 22.98
  conversion_factor_Ca <- 1 / 20.04

  # Apply conversion if requested
  if (convert_to_meq) {
    df$Na <- df$Na * conversion_factor_Na
    df$Ca <- df$Ca * conversion_factor_Ca
  }

  # SCAR calculation
  scar_value <- (df$Na) / sqrt(df$Ca)
  return(scar_value)
}
