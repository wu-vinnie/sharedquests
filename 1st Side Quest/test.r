library(here)
library(tidyverse)

# Generate fake data: 1000 samples from a normal distribution
data <- rnorm(1000, mean = 50, sd = 10)
df <- tibble(value = data)

# Create histogram with ggplot
p <- ggplot(df, aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Fake Data",
       x = "Value",
       y = "Frequency") +
  theme_minimal()

# Display the plot
print(p)

# Ensure the directory exists
if(!dir.exists(here("graphs"))) dir.create(here("graphs"))

# Save the plot as a PNG
ggsave(here("graphs", "histogram.png"), plot = p, width = 6, height = 4)
