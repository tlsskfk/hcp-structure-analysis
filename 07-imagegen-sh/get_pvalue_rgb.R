# Load necessary library
library(viridis)

# Function to map a numeric value to a color using the viridis 'H' option
map_value_to_color <- function(value, min_value, max_value) {
  # Normalize value to range [0, 1]
  normalized_value <- (value - min_value) / (max_value - min_value)
  
  # Ensure the value is within [0, 1]
  normalized_value <- max(min(normalized_value, 1), 0)
  
  # Get the corresponding color from the viridis color scale
  color_hex <- viridis(256, option = "H", direction = -1)[max(min(floor(normalized_value * 255) + 1, 256), 1)]
  
  # Return the RGB representation of the color
  return(col2rgb(color_hex))
}

# Example use
min_value <- 0 # Minimum expected value
max_value <- 1 # Maximum expected value


# Get the RGB color for the example value
rgb_color <- matrix(nrow = length(my_pvalues$pvalue), ncol = 3)
for (i in 1:length(my_pvalues$pvalue)){
  test <- map_value_to_color(my_pvalues$pvalue[i], 1, 0)

  rgb_color[i, 1:3] <- test
}
# Print the RGB color
print(rgb_color)

my_pvalues$R=rgb_color[,1]
my_pvalues$G=rgb_color[,2]
my_pvalues$B=rgb_color[,3]

write.csv(file="mypvalues.csv", my_pvalues)
