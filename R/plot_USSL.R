#' Calculate SAR
#'
#' This function calculates the Sodium Adsorption Ratio (SAR) for water quality.
#' @param df dataframe
#' @param convert_units logical, for conversion to meq/l
#' @return SAR values


# Function to calculate SAR
calculate_sar <- function(df, convert_units = FALSE) {

  # SAR calculation logic here

  # Use the provided conversion factors if needed

  if (convert_units) {
    # Convert Ca, Mg, and Na from mg/l to meq/l
    df$Ca <- df$Ca * 0.0499002
    df$Mg <- df$Mg * 0.08223684
    df$Na <- df$Na * 0.0435161
  }

  sar_value <- (df$Na) / sqrt((df$Ca + df$Mg) / 2)
  return(sar_value)
}

#' Plot USSL diagram for all rows
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
#' #' @examples
#' df <- data.frame(EC = c(1000, 2000, 3000),
#'                  Na_percent = c(20, 30, 40),
#'                  Group = c("red", "green", "blue"))
#' plot_USSL(df, ec_column = "EC", sar_column = "SAR", label_column = NULL,
#'           grp_column = "Group", convert_units = FALSE)


#'
#' @export

# Function to plot USSL diagram for all rows
plot_USSL <- function(df, ec_column, sar_column, label_column = NULL, grp_column = NULL, convert_units = FALSE) {


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
  plot_data <- data.frame(EC = ec_values, SAR = sar_values,
                          Color = group_values)


  # Calculate SAR values based on the curve equations

  curve_data <- data.frame(EC = seq(100, 12000, length.out = 1000))
  curve_data$SAR1 <- 43.75 - 8.87 * log10(curve_data$EC)
  curve_data$SAR2 <- 31.31 - 6.66 * log10(curve_data$EC)
  curve_data$SAR3 <- 18.87 - 4.44 * log10(curve_data$EC)

  # Plot using ggplot2
  gg <-
  ggplot(plot_data, aes(x = plot_data$EC, y = plot_data$SAR)) +
    geom_point(aes(color = as.factor(Color)), size = 4) +
    geom_line(data = curve_data, aes(x = curve_data$EC, y = curve_data$SAR1), color = "blue", linetype = "dashed", linewidth = 1) +
    geom_line(data = curve_data, aes(x = curve_data$EC, y = curve_data$SAR2), color = "brown", linetype = "dashed", linewidth = 1) +
    geom_line(data = curve_data, aes(x = curve_data$EC, y = curve_data$SAR3), color = "firebrick", linetype = "dashed", linewidth = 1) +
    labs(
      title = "USSL Diagram",
      x = "EC (\u00B5S/cm)",
      y = "SAR",
      color = ifelse(!is.null(grp_column), grp_column, "Group")
    ) +

    theme_few() +  # use theme_few from ggthemes library

    # fonts
    theme(
      axis.title = element_text(size = 20, face = "bold"),  # axis titles
      axis.text = element_text(size = 20, face = "bold"),
      plot.margin = margin(1,1,1,1, "cm")
    ) +

    # axis text

    scale_x_continuous(
      limits = c(80, 14000), trans = "log10",
      breaks = c(100, 250, 750, 2250, 12000), expand = c(0.0, 0.0)
    ) +  # Set x-axis limits
    annotation_logticks(sides = "b", base = 10) +  # add log ticks
    scale_y_continuous(
      limits = c(-2, 42),
      breaks = c(0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40), expand = c(0.0, 0.0)
    )+ # Adjust y-axis limits as needed
    geom_vline(xintercept = c(100, 250, 750, 2250, 12000)) +           #add vertical lines
    geom_hline(yintercept = c(-2, 0, 40))+                                 #add horizontal lines
    geom_rect(aes(xmin = 80, xmax = 100, ymin = 0, ymax = 10), fill = "lightgreen", alpha = 0.3) +#add rectangles on y-axis
    geom_rect(aes(xmin = 80, xmax = 100, ymin = 10, ymax = 18), fill = "yellow", alpha = 0.3) +
    geom_rect(aes(xmin = 80, xmax = 100, ymin = 18, ymax = 26), fill = "orange", alpha = 0.3) +
    geom_rect(aes(xmin = 80, xmax = 100, ymin = 26, ymax = 40), fill = "red", alpha = 0.3) +
    #annotate rectangles on y axis
    annotate("text", x = 90, y = 5, label = "Low", color = "black", size = 5, fontface = "bold", angle = 90) +
    annotate("text", x = 90, y = 14, label = "Medium", color = "black", size = 5, fontface = "bold", angle = 90) +
    annotate("text", x = 90, y = 22, label = "High", color = "black", size = 5, fontface = "bold", angle = 90) +
    annotate("text", x = 90, y = 33, label = "Very High", color = "black", size = 5, fontface = "bold", angle = 90)+
    # rectangle on the y-axis
    geom_rect(aes(xmin = 100, xmax = 250, ymin = -2, ymax = 0), fill = "lightgreen", alpha = 0.3) +
    geom_rect(aes(xmin = 250, xmax = 750, ymin = -2, ymax = 0), fill = "yellow", alpha = 0.3) +
    geom_rect(aes(xmin = 750, xmax = 2250, ymin = -2, ymax = 0), fill = "orange", alpha = 0.3) +
    geom_rect(aes(xmin = 2250, xmax = 12000, ymin = -2, ymax = 0), fill = "red", alpha = 0.3) +
    #annotate rectangles on x axis
    annotate("text", x = 150, y = -1, label = "Low", color = "black", size = 5, fontface = "bold") +
    annotate("text", x = 450, y = -1, label = "Medium", color = "black", size = 5, fontface = "bold") +
    annotate("text", x = 1200, y = -1, label = "High", color = "black", size = 5, fontface = "bold") +
    annotate("text", x = 5500, y = -1, label = "Very High", color = "black", size = 5, fontface = "bold")+
    # add rectangle and annotations to top of the plot
    # rectangle on the y-axis
    annotate("text", x = 150, y = 41, label = "C1", color = "black", size = 5, fontface = "bold") +
    annotate("text", x = 450, y = 41, label = "C2", color = "black", size = 5, fontface = "bold") +
    annotate("text", x = 1200, y = 41, label = "C3", color = "black", size = 5, fontface = "bold") +
    annotate("text", x = 5500, y = 41, label = "C4", color = "black", size = 5, fontface = "bold")+

    # add rectangle annotations to right of the plot
    #add rectangles to the right of the plot
    geom_rect(aes(xmin = 12000, xmax = 14000, ymin = 0, ymax = 0.9), fill = "lightgreen", alpha = 0.3) +#add rectangles on y-axis
    geom_rect(aes(xmin = 12000, xmax = 14000, ymin = 0.9, ymax = 4), fill = "yellow", alpha = 0.3) +
    geom_rect(aes(xmin = 12000, xmax = 14000, ymin = 4, ymax = 7.5), fill = "orange", alpha = 0.3) +
    geom_rect(aes(xmin = 12000, xmax = 14000, ymin = 7.5, ymax = 40), fill = "red", alpha = 0.3) +

    # rectangle on the y-axis
    annotate("text", x = 12999, y = 0.5, label = "S1", color = "black", size = 4, fontface = "bold") +
    annotate("text", x = 12999, y = 2.0, label = "S2", color = "black", size = 4, fontface = "bold") +
    annotate("text", x = 12999, y = 5.5, label = "S3", color = "black", size = 4, fontface = "bold") +
    annotate("text", x = 12999, y = 28, label = "S4", color = "black", size = 4, fontface = "bold")



    # Add labels if specified

    gg <- gg +

    if (!missing(label_column) && label_column %in% colnames(df)) {
      geom_text_repel(data = plot_data, aes(label = df[[label_column]]),
                hjust = 1, vjust = 1, size = 4, color = "black",
                fontface = "bold", max.overlaps = Inf)
    }


  # If color_by is not specified, remove legend
  if (is.null(grp_column)) {
    gg <- gg + theme(legend.position = "none")

  } else {
    # Use scale_color_manual to specify colors for each group
    gg <- gg + scale_color_manual(values = CC, guide="legend") +
      #add legend
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
# plot_USSL(df, ec_column, sar_column, label_column, grp_column)
