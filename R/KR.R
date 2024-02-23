#' Kelly Ratio (KR) Calculation
#'
#' @param df Dataframe containing necessary variables (Na, Ca).
#' @param convert_to_meq Logical, indicating whether to convert concentrations to meq/L (default: TRUE).
#' @return KR value.
#'
KR <- function(df, convert_to_meq = TRUE) {
  # Conversion factors for mg/L to meq/L
  conversion_factor_Na <- 1 / 22.98
  conversion_factor_Ca <- 1 / 20.04
  conversion_factor_Mg <- 1 / 12.1

  # Apply conversion if requested
  if (convert_to_meq) {
    df$Na <- df$Na * conversion_factor_Na
    df$Ca <- df$Ca * conversion_factor_Ca
  }

  # KR calculation
  kr_value <- (df$Na) / (df$Ca+df$Mg)
  return(kr_value)
}
