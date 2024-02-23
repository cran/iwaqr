#' Calculate PI
#'
#' This function calculates the PI for water quality.
#' @param df dataframe containing the necessary columns,
#' @param convert_units Logical, indicating whether to convert units from mg/l to meq/l.
#' @return  A numeric vector representing the permeability index (PI)
#' for each row in the dataframe,
#'  @examples
#' df <- data.frame(Ca = c(10, 20, 30),
#' Mg = c(5, 10, 15), Na = c(8, 16, 24),
#' Na = c(15, 25, 10),
#' K = c(2, 6, 4),
#' HCO3 = c(15, 30, 45),
#' SO4 = c(110, 115, 88),
#' CO3 = c(0, 0, 0),
#' Cl = c(42, 25, 16)),
#' calculate_PI <- function(df, convert_units = FALSE)

# Function to calculate PI
calculate_PI <- function(df, convert_units = FALSE) {
  # Your SAR calculation logic here
  # Use the provided conversion factors if needed
  if (convert_units) {
    # Convert Ca, Mg, and Na from mg/l to meq/l
    df$Ca <- df$Ca * 0.0499002
    df$Mg <- df$Mg * 0.08223684
    df$Na <- df$Na * 0.0435161
    df$HCO3 <- df$HCO3 * 0.01638807
  }
  PI_value <- ((df$Na + sqrt(df$HCO3)) / (df$Ca + df$Mg + df$Na)) * 100
  return(PI_value)
}



#' Calculate Total Concentration (tc)
#'
#' This function calculates the total concentration (tc) based on the provided dataframe.
#'
#' @param df Data frame containing the necessary columns.
#' @param convert_units Logical, indicating whether to convert units from mg/l to meq/l.
#' @return A numeric vector representing the total concentration (tc) for each row in the dataframe.
#' @examples
#' df <- data.frame(Ca = c(10, 20, 30),
#' Mg = c(5, 10, 15), Na = c(8, 16, 24),
#' Na = c(15, 25, 10),
#' K = c(2, 6, 4),
#' HCO3 = c(15, 30, 45),
#' SO4 = c(110, 115, 88),
#' CO3 = c(0, 0, 0),
#' Cl = c(42, 25, 16))
#' calculate_tc(df, convert_units = TRUE)

calculate_tc <- function(df, convert_units = FALSE) {
  # if needed convert units
  if (convert_units) {
    # Convert Ca, Mg, and Na from mg/l to meq/l
    df$Ca <- df$Ca * 0.0499002
    df$Mg <- df$Mg * 0.08223684
    df$Na <- df$Na * 0.0435161
    df$K  <- df$K  * 0.02557545
    df$HCO3 <- df$HCO3 * 0.01638807
    df$SO4 <- df$SO4 * 0.02082032
    df$CO3 <- df$CO3 *  0.03333333
    df$Cl <- df$Cl * 0.02820874
  }

  # calculate TC values
  tc_value <- df$Na + df$HCO3 + df$Ca + df$Mg + df$K + df$SO4 + df$CO3 + df$Cl
  return(tc_value)
}


#' Plot Doneen diagram (High permeability) for all rows
#'
#' @param df Data frame containing the necessary columns.
#' @param tc_column Column name for total concentration (tc).
#' @param PI_column Column name for PI (optional).
#' @param label_column Column name for labels (optional).
#' @param grp_column Column name for grouping (optional).
#' @param convert_units Logical, whether to convert units.
#' @return A ggplot object representing the USSL diagram.
#' #' @examples
#' df <- data.frame(tc = c(80, 65, 70),
#'                  PI = c(30, 65, 150),
#'                  Color = c("red", "green", "blue"))
#' plot_DoneenH(df, tc_column = "tc", PI_column = "PI", label_column = NULL, grp_column = NULL, convert_units = FALSE)
#'
#' @export

# Function to plot Doneen diagram for high permeability soils for all rows

plot_DoneenH <- function(df, tc_column, PI_column, label_column = NULL, grp_column = NULL, convert_units = FALSE) {
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
  data_sixtyfive <- data.frame(x = c(86.4847512,
                                     86.4847512,
                                     86.4847512,
                                     86.25521669,
                                     86.25521669,
                                     86.25521669,
                                     86.25521669,
                                     86.25521669,
                                     86.09951846,
                                     85.1364366,
                                     84.36597111,
                                     83.59550562,
                                     82.24719101,
                                     80.32102729,
                                     78.78009631,
                                     76.66131621,
                                     73.57945425,
                                     71.07544141,
                                     71.07544141,
                                     68.3788122,
                                     64.71910112,
                                     62.02247191,
                                     57.59229535,
                                     53.54735152,
                                     49.69502408,
                                     44.68699839,
                                     40.4494382,
                                     33.13001605,
                                     26.96629213,
                                     23.49919743,
                                     18.29855538,
                                     14.06099518,
                                     9.438202247,
                                     5.200642055,
                                     0


  ), y = c(19.95163241,
           17.7025393,
           15.18742443,
           12.59975816,
           11.46311971,
           11.14873035,
           10.66505441,
           10.06045949,
           9.697702539,
           9.093107618,
           8.633615478,
           8.295042322,
           7.714631197,
           7.182587666,
           6.674727932,
           6.311970979,
           5.779927449,
           5.392986699,
           5.392986699,
           5.126964933,
           4.69165659,
           4.377267231,
           4.038694075,
           3.748488513,
           3.38573156,
           3.071342201,
           2.829504232,
           2.53929867,
           2.297460701,
           2.176541717,
           2.031438936,
           1.958887545,
           1.886336155,
           1.813784764,
           1.741233374

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
  scaling_factor <- max(plot_data$tc) / max(data_sixtyfive$y)

  # Normalize the y values based on the scaling factor
  data_sixtyfive$y_normalized <- data_sixtyfive$y * scaling_factor

  # Calculate the scaling factor based on the maximum tc value
  scaling_factor <- max(plot_data$tc) / max(data_twentyfive$y)

  # Normalize the y values based on the scaling factor
  data_twentyfive$y_normalized <- data_twentyfive$y * scaling_factor



  # Plot using ggplot2
  gg <- ggplot(plot_data, aes(x = plot_data$PI, y = plot_data$tc)) +
    geom_point(aes(color = as.factor(Color)), size = 4) +



  #add 25% line


  geom_line(data = data_twentyfive, aes(x = data_twentyfive$x,
                                        y = data_twentyfive$y_normalized), color = "deeppink4",
            linetype ="dashed", linewidth = 1.2)+




  #add 65% line


    geom_line(data = data_sixtyfive, aes(x = data_sixtyfive$x,
                                         y = data_sixtyfive$y_normalized), color = "deeppink4",
              linetype ="dashed", linewidth = 1.2)+



    # add labels

    labs(
      title = "Doneen Diagram (High Permeability)",
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

    annotate(geom = "text", x = 98, y = max(plot_data$tc) * 0.87 - 5,
             label = "25% permeability", fontface = "bold",
             size = 5, color = "black", angle = 90) +

    annotate(geom = "text", x = 83, y = max(plot_data$tc) * 0.87 - 5,
             label = "65 % permeability", fontface = "bold",
             size = 5, color = "black", angle = 90) +

    annotate(geom = "text", x = 50, y = max(plot_data$tc) * 0.87 - 5,
             label = "Class I", fontface = "bold",
             size = 5, color = "black") +

    annotate(geom = "text", x = 90, y = max(plot_data$tc) * 0.87 - 5,
             label = "Class II", fontface = "bold",
             size = 5, color = "black", angle = 90) +

    annotate(geom = "text", x = 100, y = max(plot_data$tc) * 0.87 - 5,
             label = "Class III", fontface = "bold",
             size = 5, color = "black", angle = 90)



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
      # add legend
      theme(legend.position = "top",
            legend.justification = c(1,0),
            legend.title = element_text(face = "bold", size=18),
            legend.text = element_text(face = "bold", size = 18))
  }

  return(gg)
}

#Example usage
# plot_DoneenH(df, tc_column,  PI_column, label_column, grp_column, convert_units)
#df the data frame
#tc_column <- total concentration values
#PI_column <- permeability index values
#label_column <- column with labels for samples
# grp_column <- color code sample points according to categories
#convert_units <- conversion from mg/l to meq/l


