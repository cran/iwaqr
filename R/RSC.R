#' Residual Sodium Carbonate (RSC) Calculation
#'
#' @param df Dataframe containing necessary variables (HCO3, CO3, Ca, Mg).
#' @param convert_to_meq Logical, indicating whether to convert concentrations to meq/L (default: TRUE).
#' @return RSC value.
#'
RSC <- function(df, convert_to_meq = TRUE) {
  # Conversion factors for mg/L to meq/L
  conversion_factor_HCO3 <- 1 / 61.02
  conversion_factor_CO3 <- 1 / 30.0
  conversion_factor_Ca <- 1 / 20.04
  conversion_factor_Mg <- 1 / 12.1

  # Apply conversion if requested
  if (convert_to_meq) {
    df$HCO3 <- df$HCO3 * conversion_factor_HCO3
    df$CO3 <- df$CO3 * conversion_factor_CO3
    df$Ca <- df$Ca * conversion_factor_Ca
    df$Mg <- df$Mg * conversion_factor_Mg
  }

  # RSC calculation
  rsc_value <- (df$HCO3 + df$CO3) - (df$Ca + df$Mg)
  return(rsc_value)
}
