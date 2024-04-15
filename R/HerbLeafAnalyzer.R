
#' Load Herb Data
#'
#' Loads the specified .RData file containing herb data.
#' @param filename A string specifying the path to the .RData file to be loaded.
#' @return A data frame containing herb data.
#' @examples
#' data <- loadHerbData("herbData.RData")
#' @export
loadHerbData <- function(filename) {
  load(filename)
  return(herbData)
}

#' Plot Leaf Color Distribution
#'
#' Creates a bar plot showing the distribution of leaf colors in the provided data.
#' @param data A data frame containing a 'color' column.
#' @return A ggplot object representing the color distribution plot.
#' @import ggplot2
#' @examples
#' plotLeafColorDistribution(data)
#' @export
plotLeafColorDistribution <- function(data) {
  library(ggplot2)
  ggplot(data, aes(x = color, fill = color)) +
    geom_bar() +
    theme_minimal() +
    labs(title = "Leaf Color Distribution", x = "Color", y = "Count")
}

#' Calculate Leaf Area
#'
#' Calculates the area of a leaf based on provided length and width.
#' @param length Numeric, the length of the leaf in centimeters.
#' @param width Numeric, the width of the leaf in centimeters.
#' @return Numeric, the calculated area of the leaf.
#' @examples
#' calculateLeafArea(7, 3)
#' @export
calculateLeafArea <- function(length, width) {
  area <- length * width
  return(area)
}

#' Compare Leaf Shape
#'
#' Compares the shape of leaves from two specified species by plotting the ratio of leaf length to width.
#' @param data A data frame that must contain 'species', 'leaf_length', and 'leaf_width' columns.
#' @param species1 A string, the name of the first species to compare.
#' @param species2 A string, the name of the second species to compare.
#' @return A ggplot object representing the comparison of leaf shape ratios.
#' @import ggplot2
#' @examples
#' compareLeafShape(data, "Species A", "Species B")
#' @export
compareLeafShape <- function(data, species1, species2) {
  subset_data <- data[data$species %in% c(species1, species2), ]
  library(ggplot2)
  ggplot(subset_data, aes(x = species, y = leaf_length/leaf_width, fill = species)) +
    geom_bar(stat = "identity") +
    labs(title = "Comparison of Leaf Shape Ratios", x = "Species", y = "Length/Width Ratio")
}

#' Display Herb Table
#'
#' Returns the input data as a table.
#' @param data A data frame containing herb data.
#' @return The same data frame as provided in the input.
#' @examples
#' displayHerbTable(data)
#' @export
displayHerbTable <- function(data) {
  data
}


# Example usage
data <- loadHerbData("herbData.RData")
print(data)
plot <- plotLeafColorDistribution(data)
print(plot)
area <- calculateLeafArea(7, 3)  # Example dimensions in cm
print(area)
compare_plot <- compareLeafShape(data, "Species A", "Species B")
print(compare_plot)
table <- displayHerbTable(data)
print(table)
