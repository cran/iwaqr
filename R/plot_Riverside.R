

#' Plot Riverside diagram for all rows
#'
#' This function plots the USSL diagram for the given data frame.
#'
#' @param df Data frame containing the necessary columns.
#' @param ec_column Column name for electrical conductivity (EC).
#' @param sar_column Column name for SAR (optional).
#' @param label_column Column name for labels (optional).
#' @param grp_column Column name for grouping (optional).
#' @param convert_units Logical, whether to convert units.
#' @return A ggplot object representing the USSL diagram.
#' @examples
#' df <- data.frame(EC = c(1000, 2000, 3000),
#' SAR = c(20, 30, 40),
#' Color = c("red", "green", "blue"))
#' plot_Riverside(df, ec_column = "EC" , sar_column = "SAR",grp_column = "Color",
#'convert_units= FALSE)
#'
#' @export
#'
#' @param df containing values in relevant columns
#' @param convert_units logical wether to convert from mg/l to meq/l
#' @return a numeric vector containing SAR values


# Function to plot Riverside diagram for all rows
plot_Riverside <- function(df, ec_column, sar_column, label_column = NULL, grp_column = NULL, convert_units = FALSE) {
  # Calculate SAR if not provided
  if (missing(sar_column)) {
    sar_values <- calculate_sar(df, convert_units = convert_units)
  } else {
    sar_values <- df[[sar_column]]
  }

  # Use original EC values
  ec_values <- df[[ec_column]]

  # add the group column
  Color <- group_values <- if (!is.null(grp_column) && grp_column %in% colnames(df))
    df[[grp_column]] else rep("red", nrow(df))

  # Create a data frame
  plot_data <- data.frame(EC = ec_values, SAR = sar_values, Color = group_values)

  # Calculate SAR values based on the curve equations
  curve_data <- data.frame(EC = seq(20, 10000, length.out = 1000))
  curve_data$SAR1 <- pmin(32, 43.75 - 8.87 * log10(curve_data$EC))
  curve_data$SAR2 <- 31.31 - 6.66 * log10(curve_data$EC)
  curve_data$SAR3 <- 18.87 - 4.44 * log10(curve_data$EC)

  # Initialize gg object
  gg <- ggplot(plot_data, aes(x = plot_data$EC, y = plot_data$SAR)) +
    geom_point(aes(color = as.factor(Color)), size = 4) +
    geom_line(data = curve_data, aes(x = curve_data$EC, y = curve_data$SAR1), color = "blue", linetype = "dashed", size = 1) +
    geom_line(data = curve_data, aes(x = curve_data$EC, y = curve_data$SAR2), color = "brown", linetype = "dashed", size = 1) +
    geom_line(data = curve_data, aes(x = curve_data$EC, y = curve_data$SAR3), color = "firebrick", linetype = "dashed", size = 1) +
    labs(
      title = "Riverside Diagram",
      x = "EC (\u00B5S/cm)",
      y = "SAR",
      color = ifelse(!is.null(grp_column), grp_column, "Group")
    ) +

    theme_few() +  # use theme_few from ggthemes library

    # fonts
    theme(
      axis.title = element_text(size = 20, face = "bold"),  # axis titles
      axis.text = element_text(size = 18, face = "bold"),
      plot.margin = margin(2,2,2,2, "cm")
    ) +

    # axis text
    scale_x_continuous(
      limits = c(10, 10000), trans = "log10",
      breaks = c(20, 100, 250, 750, 2250, 5000, 10000), expand = c(0 , 0)
    ) +  # Set x-axis limits
    annotation_logticks(sides = "b", base = 10) +  # add log ticks
    scale_y_continuous(
      limits = c(-2, 32),
      breaks = c(0, 2, 4,6, 8,10, 12,14, 16, 18, 20,22, 24,26, 28,30, 32), expand = c(0, 0)
    ) +

    # Adjust y-axis limits as needed
    geom_vline(xintercept = c(20, 100, 250, 750, 2250, 5000, 10000)) +  # add vertical lines
    geom_hline(yintercept = 0)+

    geom_rect(aes(xmin = 10, xmax = 20, ymin = 0, ymax = 13), fill = "lightgreen", alpha = 0.3) +#add rectangles on y-axis
    geom_rect(aes(xmin = 10, xmax = 20, ymin = 13, ymax = 23), fill = "yellow", alpha = 0.3) +
    geom_rect(aes(xmin = 10, xmax = 20, ymin = 22.5, ymax = 32), fill = "red", alpha = 0.3) +

    #annotate rectangles on y axis
    annotate("text", x = 14, y = 7, label = "Suitable", color = "black", size = 6, fontface = "bold", angle = 90) +
    annotate("text", x = 14, y = 17, label = "Moderate", color = "black", size = 6, fontface = "bold", angle = 90) +
    annotate("text", x = 14, y = 27, label = "Strong", color = "black", size = 6, fontface = "bold", angle = 90)+

  #annotate rectangles on x axis
    annotate("text", x = 50, y = -0.8, label = "0", color = "black", size = 6, fontface = "bold") +
    annotate("text", x = 150, y = -0.8, label = "1", color = "black", size = 6, fontface = "bold") +
    annotate("text", x = 500, y = -0.8, label = "2", color = "black", size = 6, fontface = "bold") +
    annotate("text", x = 1500, y = -0.8, label = "3", color = "black", size = 6, fontface = "bold")+
    annotate("text", x = 3500, y = -0.8, label = "4", color = "black", size = 6, fontface = "bold")+
    annotate("text", x = 7000, y = -0.8, label = "5", color = "black", size = 6, fontface = "bold")



   # Add labels if specified
  if (!missing(label_column) && label_column %in% colnames(df)) {
    gg <- gg + geom_text_repel(data = plot_data, aes(label = df[[label_column]]),
                         hjust = 1, vjust = 1, size = 4, color = "black",
                         fontface = "bold", max.overlaps = Inf)
  }

  # If color_by is not specified, remove legend
  if (is.null(grp_column)) {
    gg <- gg + theme(legend.position = "none")
  } else {
    # Use scale_color_manual to specify colors for each group
    gg <- gg + scale_color_manual(values = CC, guide="legend") +
      # Add legend
      theme(legend.position = "top",
            legend.justification = c(1,0),
            legend.title = element_text(face = "bold", size=18),
            legend.text = element_text(face = "bold", size = 18))
  }

  return(gg)
}

# Example usage
# df <- your_dataframe
# ec_column <- "your_ec_column"
# sar_column <- "your_sar_column"
# label_column <- "your_label_column"
# grp_column <- "your_group_column"  # replace with an actual column name or NULL
# plot_Riverside(df, ec_column, sar_column, label_column, grp_column)

