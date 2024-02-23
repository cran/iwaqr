#' Magnesium Adsorption Ratio (MAR) Calculation
#'
#' @param df Dataframe containing necessary variables (Mg, Ca).
#' @param convert_to_meq Logical, indicating whether to convert concentrations to meq/L (default: TRUE).
#' @return MAR value.
#'
MAR <- function(df, convert_to_meq = TRUE) {
  # Conversion factors for mg/L to meq/L
  conversion_factor_Mg <- 1 / 12.1
  conversion_factor_Ca <- 1 / 20.04

  # Apply conversion if requested
  if (convert_to_meq) {
    df$Mg <- df$Mg * conversion_factor_Mg
    df$Ca <- df$Ca * conversion_factor_Ca
  }

  # MAR calculation
  mar_value <- (df$Mg) / (df$Ca+df$Mg)*100
  return(mar_value)
}
