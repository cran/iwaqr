#' Irrigation Water Quality Index Calculations
#'
#' This function calculates multiple water quality indices for irrigation.
#'
#' @param df Dataframe containing necessary variables.
#' @param convert_to_meq Logical, indicating whether to convert concentrations to meq/L (default: TRUE).
#' @return Dataframe containing calculated indices: SAR, MAR, SCAR, RSC, RSBC, PI, KR, NaPercentage, and PS.
#'
irrigationALL <- function(df, convert_to_meq = TRUE) {
  # Calculate individual indices
  sar_value <- SAR(df, convert_to_meq)
  mar_value <- MAR(df, convert_to_meq)
  scar_value <- SCAR(df, convert_to_meq)
  rsc_value <- RSC(df, convert_to_meq)
  rsbc_value <- RSBC(df, convert_to_meq)
  pi_value <- PI(df, convert_to_meq)
  kr_value <- KR(df, convert_to_meq)
  na_percent_value <- NaPercentage(df, convert_to_meq)
  ps_value <- PS(df, convert_to_meq)

  # Create a dataframe with results
  result_df <- data.frame(
    SAR = sar_value,
    MAR = mar_value,
    SCAR = scar_value,
    RSC = rsc_value,
    RSBC = rsbc_value,
    PI = pi_value,
    KR = kr_value,
    NaPercentage = na_percent_value,
    PS = ps_value
  )

  return(result_df)
}

