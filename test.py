import numpy as np
import matplotlib.pyplot as plt

# Generate fake data: 1000 samples from a normal distribution
data = np.random.normal(loc=50, scale=10, size=1000)

# Plot histogram
plt.hist(data, bins=30, color='skyblue', edgecolor='black')
plt.title('Histogram of Fake Data')
plt.xlabel('Value')
plt.ylabel('Frequency')
plt.show()


