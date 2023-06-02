library(ggplot2)
library(readr)
library(quantreg)
library(Hmisc)
library(tidyverse)

# Download the data
vocab <- read_csv("https://r-data.pmagunia.com/system/files/datasets/dataset-15712.csv")

# Load the mtcars data
data("mtcars")
glimpse(mtcars)

# Change the fam and cyl columns from doubles to factors
mtcars$fam <- as.factor(mtcars$am)
mtcars$fcyl <- as.factor(mtcars$cyl)

# Using mtcars, draw a scatter plot of mpg vs. wt
ggplot(mtcars, aes(wt, mpg)) + 
  geom_point()

# Amend the plot to add a smooth layer
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth()

# Amend the plot. Use lin. reg. smoothing; turn off std err ribbon
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Amend the plot. Swap geom_smooth() for stat_smooth().
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

# Using mtcars, plot mpg vs. wt, colored by fcyl
ggplot(mtcars, aes(wt, mpg, color = fcyl)) +
  # Add a point layer
  geom_point() +
  # Add a smooth lin reg stat, no ribbon
  stat_smooth(method = "lm", se = FALSE)

# Amend the plot to add another smooth layer with dummy grouping
ggplot(mtcars, aes(x = wt, y = mpg, color = fcyl)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  stat_smooth(aes(group = 1), method = "lm", se = FALSE)

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  # Add 3 smooth LOESS stats, varying span & color
  stat_smooth(se = FALSE, color = "red", span = 0.9) +
  stat_smooth(se = FALSE, color = "green", span = 0.6) +
  stat_smooth(se = FALSE, color = "blue", span = 0.3)

# Amend the plot to color by fcyl
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  # Add a smooth LOESS stat, no ribbon
  stat_smooth(se = FALSE) +
  # Add a smooth lin. reg. stat, no ribbon
  stat_smooth(se = FALSE, method = "lm")

# Amend the plot
ggplot(mtcars, aes(x = wt, y = mpg, color = fcyl)) +
  geom_point() +
  # Map color to dummy variable "All"
  stat_smooth(aes(color = "All"), se = FALSE) +
  stat_smooth(method = "lm", se = FALSE)

# Switch to exploring the vocab data set
ggplot(vocab, aes(x = education, y = vocabulary)) +
  geom_jitter(alpha = 0.25) +
  # Add a quantile stat, at 0.05, 0.5, and 0.95
  stat_quantile(quantiles = c(0.05, 0.5, 0.95))

ggplot(vocab, aes(x = education, y = vocabulary)) +
  stat_sum() +
  # Add a size scale, from 1 to 10
  scale_size(range = c(1,10))

# Amend the stat to use proportion sizes
ggplot(vocab, aes(x = education, y = vocabulary)) +
  stat_sum(aes(size = ..prop..))

# Amend the plot to group by education
ggplot(vocab, aes(x = education, y = vocabulary, group = education)) +
  stat_sum(aes(size = ..prop..))

# Define position objects
# 1. Jitter with width 0.2
posn_j <- position_jitter(width = 0.2)
# 2. Dodge with width 0.1
posn_d <- position_dodge(width = 0.1)
# 3. Jitter-dodge with jitter.width 0.2 and dodge.width 0.1
posn_jd <- position_jitterdodge(jitter.width = 0.2, dodge.width = 0.1)

# Create the plot base: wt vs. fcyl, colored by fam
p_wt_vs_fcyl_by_fam <- ggplot(mtcars, aes(x = fcyl, y = wt, color = fam))

# Add a point layer
p_wt_vs_fcyl_by_fam +
  geom_point()

# Add jittering only
p_wt_vs_fcyl_by_fam +
  geom_point(position = posn_j)

# Add dodging only
p_wt_vs_fcyl_by_fam +
  geom_point(position = posn_d)

# Add jittering and dodging
p_wt_vs_fcyl_by_fam +
  geom_point(position = posn_jd)

# Add jittering only
p_wt_vs_fcyl_by_fam_jit <- p_wt_vs_fcyl_by_fam +
  geom_point(position = posn_j)

# Add dodging only
p_wt_vs_fcyl_by_fam +
  geom_point(position = posn_d)

p_wt_vs_fcyl_by_fam_jit +
  # Add a summary stat of std deviation limits
  stat_summary(position = posn_d,
               fun.data = mean_sdl,
               fun.args = list(mult = 1))

# The default geom for stat_summary() is "pointrange" which is already great.
# Update the summary stat to use an "errorbar" geom by assigning it to the geom argument.
p_wt_vs_fcyl_by_fam_jit +
  # Change the geom to be an errorbar
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), position = posn_d, geom = "errorbar")

# Update the plot to add a summary stat of 95% confidence limits.
# Set the data function to mean_cl_normal (without parentheses).
# Again, use the dodge position.
p_wt_vs_fcyl_by_fam_jit +
  # Add a summary stat of normal confidence limits
  stat_summary(fun.data = mean_cl_normal, position = posn_d)







