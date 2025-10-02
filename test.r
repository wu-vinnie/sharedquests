# Generate fake data: 1000 samples from a normal distribution
data <- rnorm(1000, mean = 50, sd = 10)

# Plot histogram
hist(data, breaks = 30, col = "skyblue", border = "black",
    main = "Histogram of Fake Data",
    xlab = "Value",
    ylab = "Frequency")