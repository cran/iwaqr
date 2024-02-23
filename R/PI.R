#' Permeability Index (PI) Calculation
#'
#' @param df Dataframe containing necessary variables (Na, HCO3, Ca, Mg).
#' @param convert_to_meq Logical, indicating whether to convert concentrations to meq/L (default: TRUE).
#' @return PI value.
#'
PI <- function(df, convert_to_meq = TRUE) {
  # Conversion factors for mg/L to meq/L
  conversion_factor_Na <- 1 / 22.98
  conversion_factor_HCO3 <- 1 / 61.02
  conversion_factor_Ca <- 1 / 20.04
  conversion_factor_Mg <- 1 / 12.1

  # Apply conversion if requested
  if (convert_to_meq) {
    df$Na <- df$Na * conversion_factor_Na
    df$HCO3 <- df$HCO3 * conversion_factor_HCO3
    df$Ca <- df$Ca * conversion_factor_Ca
    df$Mg <- df$Mg * conversion_factor_Mg
  }

  # PI calculation
  pi_value <- (df$Na + sqrt(df$HCO3)) / (df$Na + df$Ca + df$Mg) * 100
  return(pi_value)
}
