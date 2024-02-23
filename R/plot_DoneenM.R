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
#'#' @examples
#' df <- data.frame(tc = c(80, 65, 70),
#'                  PI = c(30, 65, 150),
#'                  Color = c("red", "green", "blue"))
#' plot_DoneenM(df, tc_column = "tc", PI_column = "PI", label_column = NULL, grp_column = NULL, convert_units = FALSE)

#' @export


# Function to plot Doneen (Medium Permeability) diagram for all rows

plot_DoneenM <- function(df, tc_column, PI_column, label_column = NULL, grp_column = NULL, convert_units = FALSE) {
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


  # data for plotting 75 % curve

  data_sevetyfive <- data.frame(x = c(83.97941681,
                                      83.77358491,
                                      83.77358491,
                                      83.77358491,
                                      83.3619211,
                                      83.15608919,
                                      82.74442539,
                                      81.92109777,
                                      80.27444254,
                                      77.59862779,
                                      74.92281304,
                                      71.42367067,
                                      67.30703259,
                                      63.80789022,
                                      58.45626072,
                                      53.72212693,
                                      48.98799314,
                                      44.45969125,
                                      39.51972556,
                                      32.52144082,
                                      25.11149228,
                                      19.34819897,
                                      13.37907376,
                                      6.380789022,
                                      0.0
  ), y = c(20,
          17.80548628,
          15.23690773,
          12.66832918,
          10.64837905,
          10.17456359,
          9.775561097,
          9.301745636,
          8.778054863,
          8.304239401,
          7.905236908,
          7.456359102,
          6.932668329,
          6.558603491,
          6.034912718,
          5.66084788,
          5.311720698,
          4.987531172,
          4.688279302,
          4.314214464,
          3.965087282,
          3.740648379,
          3.516209476,
          3.216957606,
          3.067331671
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
  scaling_factor <- max(plot_data$tc) / max(data_sevetyfive$y)

  # Normalize the y values based on the scaling factor
  data_sevetyfive$y_normalized <- data_sevetyfive$y * scaling_factor

  # Calculate the scaling factor based on the maximum tc value
  scaling_factor <- max(plot_data$tc) / max(data_twentyfive$y)

  # Normalize the y values based on the scaling factor
  data_twentyfive$y_normalized <- data_twentyfive$y * scaling_factor




# Plot using ggplot2
  gg <- ggplot(plot_data, aes(x = plot_data$PI, y = plot_data$tc)) +
    geom_point(aes(color = as.factor(Color)), size = 4) +

   #add labels to x and y axis, and plot title

    labs(
      title = "Doneen Diagram (Medium Permeability)",
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
                               margin = margin(t = 100, unit = "mm")), #axis text
      plot.margin = margin(1, 1, 1, 1, "cm") #plot margins
    ) +

    # Set x-axis limits
    scale_x_continuous(
      limits = c(NA, 0),  # Adjust limits to reverse axis
      breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 150, 160, 170, 180, 190, 200),
      expand = c(0.01, 0.01),
      trans = scales::reverse_trans()
    ) +

    # Set y-axis limits
    scale_y_continuous(
      limits = c(0, NA),
      breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
      expand = c(0, 0)
    ) +

    # Add classification lines (verticals)
    geom_vline(xintercept = c(25, 75), linetype = "solid",
               color = "gray", size = 0.8) +


    #add 25% line


    geom_line(data = data_twentyfive, aes(x = data_twentyfive$x,
                                          y = data_twentyfive$y_normalized),
              color = "deeppink4",
              linetype ="dashed", linewidth = 1.2)+

    #add 75% line

    geom_line(data = data_sevetyfive, aes(x = data_sevetyfive$x,
                                          y = data_sevetyfive$y_normalized),
              color = "deeppink4",
              linetype ="dashed", linewidth = 1.2)+



    # add annotations and label plot

    annotate(geom = "text", x = 100, y = max(plot_data$tc) * 0.87 - 5,
             label = "25% permeability", fontface = "bold",
             size = 5, color = "black", angle = 90) +

    annotate(geom = "text", x = 82, y = max(plot_data$tc) * 0.87 - 5,
             label = "75% permeability", fontface = "bold",
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
# df <- user specified dataframe
# tc_column <- column with total concentration values
# PI_column <- column with PI values
# label_column <- column used for labelling points
# grp_column <- columns with classes or categories used to color points
# plot_DoneenM(df, tc_column, PI_column, label_column, grp_column, convert_units)
