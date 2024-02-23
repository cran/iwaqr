#' Color palette for Wilcox diagram
#'
#' This vector defines the color palette used in the Wilcox diagram.
#' It contains a sequence of color names.
#'
#' @name CC
#' @aliases CC
#' @keywords data
#' @export


CC <- c("red", "darkgoldenrod4",
        "blue4", "green",
        "coral", "orange",
        "lightgreen", "firebrick",
        "cyan", "darkgreen", "Yellow", "deeppink4")



#' Calculate Na percent
#'
#' This function calculates the percentage of sodium (Na%) in water samples.
#'
#' @param df A dataframe containing the necessary columns.
#' @param convert_units Logical indicating whether to convert values from mg/l to meq/l.
#' @return A numeric vector containing Na percent values.
#'
#' @examples
#' df <- data.frame(Ca = c(10, 20, 30),
#' Mg = c(5, 10, 15),
#' Na = c(15, 25, 35), K = c(3, 5, 6))
#'
#' calculate_Napercent(df, convert_units = TRUE)
#'
#' @export


calculate_Napercent <- function(df, convert_units = FALSE) {
  # Your Na% calculation logic here
  # Use the provided conversion factors if needed

  if (convert_units) {

     # Convert Ca, Mg, and Na from mg/l to meq/l

    df$Ca <- df$Ca * (1/20.04)
    df$Mg <- df$Mg * (1/12.16)
    df$Na <- df$Na * (1/22.98)
    df$K  <- df$K * (1/39.1)

  }

  calculate_Napercent <- (df$Na / (df$Ca + df$Mg + df$Na+df$K)) * 100
  return(calculate_Napercent)
}


#' Plot Wilcox diagram for all rows
#'
#' This function plots the USSL diagram for the given data frame.
#'
#' @param df Data frame containing the necessary columns.
#' @param ec_column Column name for electrical conductivity (EC).
#' @param Napercent_column Column name for Na percent (optional).
#' @param label_column Column name for labels (optional).
#' @param grp_column Column name for grouping (optional).
#' @param convert_units Logical, whether to convert units.
#' @return A ggplot object representing the Wilcox diagram.
#'
#'
#' #' @examples
#' f <- data.frame(EC = c(1000, 2000, 3000),
#'                 Na_percent = c(20, 30, 40),
#'                 Color = c("red", "green", "blue"))
#' plot_Wilcox(f, ec_column = "EC", Napercent_column = "Na_percent",
#'             label_column = NULL, grp_column = "Color", convert_units = FALSE)

#'
#' @export
#'
#' @param df containig relevant columns with values
#' @param convert_units logical wether to convert values from mg/l to meq/l
#' @return a numeric vector containing Na percent values

# Function to plot Wilcox diagram for all rows
plot_Wilcox <- function(df, ec_column, Napercent_column,
                        label_column = NULL, grp_column = NULL, convert_units = FALSE) {


  # Calculate Na percent if not provided


  if (missing(Napercent_column)) {
    Napercent_values <- calculate_Napercent(df, convert_units = convert_units)
  } else {
    Napercent_values <- df[[Napercent_column]]
  }

  # Use original EC values
  ec_values <- df[[ec_column]]

  # add the group column

  Color <- group_values <- if (!is.null(grp_column) && grp_column %in% colnames(df))
    df[[grp_column]] else rep("red", nrow(df))


  # Create a data frame

  plot_data <- data.frame(EC = ec_values, Na_percent = Napercent_values,
                          Color = group_values)

  # Calculate the dynamic x-axis limits based on the data
  data_range <- range(plot_data$EC)
  dynamic_limits <- c(min(0, data_range[1]), 4500)

  # If the maximum value in the data is greater than 4500, update the upper limit
  if (data_range[2] > 4500) {
    dynamic_limits[2] <- data_range[2]
  }


# Plot using ggplot2

  gg <- ggplot(plot_data, aes(x = plot_data$EC, y = plot_data$Na_percent)) +

    geom_point(aes(color = as.factor(Color)), size = 4) +

    # add curves and verticals to the plot

    annotate(geom = "segment", x=0, xend = 3000, y=100, yend = 93,
             linetype="solid", linewidth = 1, color ="gray")+
    annotate(geom = "segment", x=0, xend = 2000, y=100, yend = 77,
             linetype="solid", linewidth = 1, color ="gray")+
    annotate(geom = "curve", x=0, xend = 2000, y=100, yend = 48,
             linetype="solid", linewidth = 1, curvature=0.3, color ="gray")+
    geom_segment(aes(x = 750, y = 0, xend = 750, yend = 65),
                 color = "gray", linetype = "solid", size = 1)+
    geom_segment(aes(x = 2000, y = 0, xend = 2000, yend = 77),
                 color = "gray", linetype = "solid", size = 1)+
    geom_segment(aes(x = 3000, y = 0, xend = 3000, yend = 93),
                 color = "gray", linetype = "solid", size = 1)+

     #add labels optional

    labs(
      title = "Wilcox Diagram",
      x = "EC (\u00B5S/cm)",
      y = "Na%",
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

    scale_x_continuous(limits = dynamic_limits,
      breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 5500, 6000),
      expand = c(0, 0.2)
    ) +

    # Set x-axis limits

    scale_y_continuous(
      limits = c(0, 100),
      breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
      expand = c(0 , 0)

    ) +

  #annotate rectangles on x axis

    annotate("text", x = 250, y = 30, label = "Excellent",
             color = "black", size = 5, fontface = "bold", angle = 90) +
    annotate("text", x = 1500, y = 30, label = "Good",
             color = "black", size = 5, fontface = "bold") +
    annotate("text", x = 2500, y = 30, label = "Doubtfull",
             color = "black", size = 5, fontface = "bold") +
    annotate("text", x = 3800, y = 95, label = "Unsuitable",
             color = "black", size = 5, fontface = "bold")+
    annotate("text", x = 1200, y = 78, label = "Permissible",
             color = "black", size = 5, fontface = "bold")


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

# Example usage with color_by only
# df <- your_dataframe
# ec_column <- "your_ec_column"
# Napercent_column <- "your_Na%_column"
# label_column <- "your_label_column"
# grp_column <- "your_group_column"  # replace with an actual column name or NULL
# plot_Wilcox(df, ec_column, Napercent_column, label_column, grp_column)
