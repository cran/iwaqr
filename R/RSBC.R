#' Residual Sodium Bicarbonate (RSBC) Calculation
#'
#' @param df Dataframe containing necessary variables (HCO3, Ca).
#' @param convert_to_meq Logical, indicating whether to convert concentrations to meq/L (default: TRUE).
#' @return RSBC value.
#'
RSBC <- function(df, convert_to_meq = TRUE) {
  # Conversion factors for mg/L to meq/L
  conversion_factor_HCO3 <- 1 / 61.02
  conversion_factor_Ca <- 1 / 20.04

  # Apply conversion if requested
  if (convert_to_meq) {
    df$HCO3 <- df$HCO3 * conversion_factor_HCO3
    df$Ca <- df$Ca * conversion_factor_Ca
  }

  # RSBC calculation
  rsbc_value <- df$HCO3 - df$Ca
  return(rsbc_value)
}
