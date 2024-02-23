
#' Plot Doneen diagram (Low permeability) for all rows
#'
#' This function plots the USSL diagram for the given data frame.
#'
#' @param df Data frame containing the necessary columns.
#' @param tc_column Column name for total concentration (tc).
#' @param PI_column Column name for PI (optional).
#' @param label_column Column name for labels (optional).
#' @param grp_column Column name for grouping (optional).
#' @param convert_units Logical, whether to convert units.
#' @return A ggplot object representing the USSL diagram.
#' #'@examples
#' df <- data.frame(tc = c(80, 65, 70),
#'                  PI = c(30, 65, 150),
#'                  Color = c("red", "green", "blue"))
#' plot_DoneenL(df, tc_column = "tc", PI_column = "PI", label_column = NULL, grp_column = NULL, convert_units = FALSE)

#' @export

# Function to plot USSL diagram for all rows
plot_DoneenL <- function(df, tc_column, PI_column, label_column = NULL, grp_column = NULL, convert_units = FALSE) {
  # Calculate PI if not provided
  if (missing(PI_column)) {
    PI_values <- calculate_PI(df, convert_units = convert_units)
  } else {
    PI_values <- df[[PI_column]]
  }

  # if TC column is not provided
  if (missing(tc_column)) {
    tc_values <- calculate_tc(df, convert_units = convert_units)
  } else {
    tc_values <- df[[tc_column]]
  }

  # add the group column
  Color <- group_values <- if (!is.null(grp_column) && grp_column %in% colnames(df))
    df[[grp_column]] else rep("red", nrow(df))

  # Create a data frame
  plot_data <- data.frame(tc = tc_values, PI = PI_values, Color = group_values)

  # curve data for 93% line
  data_ninteythree <- data.frame(x = c(
    75.73604061, 75.73604061,
                                       75.73604061,
                                       75.32994924,
                                       75.12690355,
                                       74.72081218,
                                       74.72081218,
                                       74.31472081,
                                       73.50253807,
                                       73.0964467,
                                       72.69035533,
                                       71.87817259,
                                       71.26903553,
                                       69.64467005,
                                       68.42639594,
                                       67.00507614,
                                       65.38071066,
                                       63.75634518,
                                       61.11675127,
                                       58.27411168,
                                       55.43147208,
                                       52.18274112,
                                       47.71573604,
                                       47.71573604,
                                       43.65482234,
                                       39.39086294,
                                       33.0964467,
                                       27.20812183,
                                       18.88324873,
                                       11.37055838,
                                       4.873096447,
                                       0.406091371
  ), y = c(
           100,
           19.97557998,
           19.12087912,
           17.92429792,
           17.28937729,
           16.7032967,
           16.7032967,
           16.04395604,
           15.21367521,
           14.62759463,
           14.16361416,
           13.43101343,
           12.91819292,
           12.28327228,
           11.79487179,
           11.25763126,
           10.79365079,
           10.42735043,
           10.01221001,
           9.67032967,
           9.206349206,
           8.864468864,
           8.473748474,
           8.473748474,
           8.131868132,
           7.765567766,
           7.423687424,
           7.106227106,
           6.764346764,
           6.495726496,
           6.275946276,
           6.105006105
  ))

  #data for plotting 25% curve

  data_twentyfive <- data.frame(x = c(95,
                                      95,
                                      95,
                                      95,
                                      94.80,
                                      94.75,
                                      94.70,
                                      94.65,
                                      94.6,
                                      94.58,
                                      92.84,
                                      90.72,
                                      88.41,
                                      85.90,
                                      82.82,
                                      80.32,
                                      77.81,
                                      75



  ), y = c(20,
           15.09068924,
           12.55139057,
           9.939540508,
           7.521160822,
           6.674727932,
           6.070133011,
           6.094316808,
           5.465538089,
           4.812575574,
           3.990326481,
           3.313180169,
           2.587666264,
           1.934703748,
           1.281741233,
           0.773881499,
           0.38694075,
           0.0

  ))




# Calculate the scaling factor based on the maximum tc value
  scaling_factor <- max(plot_data$tc) / max(data_ninteythree$y)

  # Normalize the y values based on the scaling factor
  data_ninteythree$y_normalized <- data_ninteythree$y * scaling_factor

  # Calculate the scaling factor based on the maximum tc value
  scaling_factor <- max(plot_data$tc) / max(data_twentyfive$y)

  # Normalize the y values based on the scaling factor
  data_twentyfive$y_normalized <- data_twentyfive$y * scaling_factor



  # Plot using ggplot2
  gg <- ggplot(plot_data, aes(x = plot_data$PI, y = plot_data$tc)) +
    geom_point(aes(color = as.factor(Color)), size = 4) +




 #add 25% line


    geom_line(data = data_twentyfive, aes(x = data_twentyfive$x,
                                          y = data_twentyfive$y_normalized),
              color = "deeppink4",
              linetype ="dashed", linewidth = 1.2)+





  #add 93% line

 geom_line(data = data_ninteythree, aes(x = data_ninteythree$x,
                                        y = data_ninteythree$y_normalized),
           color = "deeppink4",
        linetype ="dashed", linewidth = 1.2)+



# add labels

    labs(
      title = "Doneen Diagram (Low Permeability)",
      x = "Permeability Index (%)",
      y = "Total concentration (meq/l)",
      color = ifelse(!is.null(grp_column), grp_column, "Group")
    ) +

    theme_few() +  # use theme_few from ggthemes library

    # fonts
    theme(
      axis.title = element_text(size = 20, face = "bold",
                                margin = margin(b = 60, unit = "mm")),   # axis titles
      axis.text = element_text(size = 18, face = "bold",
                               margin = margin(t = 100, unit = "mm")),
      plot.margin = margin(1, 1, 1, 1, "cm")
    ) +

    # Set x-axis limits
    scale_x_continuous(
      limits = c(NA, 0),  # Adjust limits to reverse axis
      breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 150),
      expand = c(0.01, 0.01),
      trans = scales::reverse_trans()
    ) +

    # Set y-axis limits
    scale_y_continuous(
      limits = c(0, NA),
      breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 150),
      expand = c(0, 0)
    ) +

    # Add classification curves
    geom_vline(xintercept = c(25, 75), linetype = "solid",
               color = "gray", size = 0.8) +


    # add annotations and label plot

    annotate(geom = "text", x = 100, y = max(plot_data$tc) * 0.87 - 5,
             label = "25% permeability", fontface = "bold",
             size = 5, color = "black", angle = 90) +

    annotate(geom = "text", x = 78, y = max(plot_data$tc) * 0.87 - 5,
             label = "93 % permeability", fontface = "bold",
             size = 5, color = "black", angle = 90) +

    annotate(geom = "text", x = 50, y = max(plot_data$tc) * 0.87 - 5,
             label = "Class I", fontface = "bold",
             size = 5, color = "black") +

    annotate(geom = "text", x = 90, y = max(plot_data$tc) * 0.87 - 5,
             label = "Class II", fontface = "bold",
             size = 5, color = "black", angle = 90) +

    annotate(geom = "text", x = 105, y = max(plot_data$tc) * 0.87 - 5,
             label = "Class III", fontface = "bold",
             size = 5, color = "black", angle = 90)



  # Add labels if specified
  gg <- gg +
    if (!missing(label_column) && label_column %in% colnames(df)) {
      geom_text_repel(data = plot_data, aes(label = df[[label_column]]),
                hjust = 1.2, vjust = 1, size = 4, color = "black",
                fontface = "bold", max.overlaps = Inf)
    }

  # If color_by is not specified, remove legend
  if (is.null(grp_column)) {
    gg <- gg + theme(legend.position = "none")
  } else {
    # Use scale_color_manual to specify colors for each group
    gg <- gg + scale_color_manual(values = CC, guide="legend") +
      # add legend
      theme(legend.position = "top",
            legend.justification = c(1,0),
            legend.title = element_text(face = "bold", size=18),
            legend.text = element_text(face = "bold", size = 18))
  }

  return(gg)
}

#Example usage
# plot_DoneenL(df, tc_column,  PI_column, label_column, grp_column, convert_units)
#df the data frame
#tc_column <- total concentration values
#PI_column <- permeability index values
#label_column <- column with labels for samples
# grp_column <- color code sample points according to categories
#convert_units <- conversion from mg/l to meq/l


