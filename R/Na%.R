

#' Sodium Percentage (NaPercentage) Calculation
#'
#' @param df Dataframe containing necessary variables (Na, Ca, Mg, K).
#' @param convert_to_meq Logical, indicating whether to convert concentrations to meq/L (default: TRUE).
#' @return Sodium Percentage value.
#'
NaPercentage <- function(df, convert_to_meq = TRUE) {
  # Conversion factors for mg/L to meq/L
  conversion_factor_Na <- 1 / 23.0
  conversion_factor_Ca <- 1 / 20.0
  conversion_factor_Mg <- 1 / 12.1
  conversion_factor_K <- 1 / 39.1

  # Apply conversion if requested
  if (convert_to_meq) {
    df$Na <- df$Na * conversion_factor_Na
    df$Ca <- df$Ca * conversion_factor_Ca
    df$Mg <- df$Mg * conversion_factor_Mg
    df$K <- df$K * conversion_factor_K
  }

  # Sodium Percentage calculation
  na_percent_value <- (df$Na / (df$Na + df$Ca + df$Mg + df$K)) * 100
  return(na_percent_value)
}
