#' Potential Salinity (PS) Calculation
#'
#' @param df Dataframe containing necessary variables (Cl, SO4).
#' @param convert_to_meq Logical, indicating whether to convert concentrations to meq/L (default: TRUE).
#' @return PS value.
#'
PS <- function(df, convert_to_meq = TRUE) {
  # Conversion factors for mg/L to meq/L
  conversion_factor_Cl <- 1 / 35.45
  conversion_factor_SO4 <- 1 / 48.03

  # Apply conversion if requested
  if (convert_to_meq) {
    df$Cl <- df$Cl * conversion_factor_Cl
    df$SO4 <- df$SO4 * conversion_factor_SO4
  }

  # PS calculation
  ps_value <- df$Cl + (0.5 * df$SO4)
  return(ps_value)
}
