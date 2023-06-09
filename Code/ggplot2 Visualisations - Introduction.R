# Load the ggplot2 package
library(ggplot2)

# Explore the mtcars data frame with str()
str(mtcars)
mtcars$fcyl <- as.factor(mtcars$cyl)
mtcars$fam <- as.factor(mtcars$am)

# Execute the following command
ggplot(mtcars, aes(cyl, mpg)) +
  geom_point()

# Change the command below so that cyl is treated as factor
ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_point()

# Edit to add a color aesthetic mapped to disp
ggplot(mtcars, aes(wt, mpg, color = disp)) +
  geom_point()

# Change the color aesthetic to a size aesthetic
ggplot(mtcars, aes(wt, mpg, size = disp)) +
  geom_point()

# Explore the diamonds data frame with str()
str(diamonds)

# Add geom_point() with +
ggplot(diamonds, aes(carat, price)) +
  geom_point()

# Add geom_smooth() with +
ggplot(diamonds, aes(carat, price)) +
  geom_point()+
  geom_smooth()

# Map the color aesthetic to clarity
ggplot(diamonds, aes(carat, price, color = clarity)) +
  geom_point() +
  geom_smooth()

# Make the points 40% opaque
ggplot(diamonds, aes(carat, price, color = clarity)) +
  geom_point(alpha = 0.4) +
  geom_smooth()

# Draw a ggplot
# Use the diamonds dataset
# For the aesthetics, map x to carat and y to price
plt_price_vs_carat <- ggplot(diamonds,
                             aes(carat, price))

# Add a point layer to plt_price_vs_carat
plt_price_vs_carat + geom_point()

# From previous step
plt_price_vs_carat <- ggplot(diamonds, aes(carat, price))

# Edit this to make points 20% opaque: plt_price_vs_carat_transparent
plt_price_vs_carat_transparent <- plt_price_vs_carat +
  geom_point(alpha = 0.2)

# See the plot
plt_price_vs_carat_transparent

# From previous step
plt_price_vs_carat <- ggplot(diamonds, aes(carat, price))

# Edit this to map color to clarity,
# Assign the updated plot to a new object
plt_price_vs_carat_by_clarity <- plt_price_vs_carat + geom_point(aes(color = clarity))

# See the plot
plt_price_vs_carat_by_clarity

# Map x to mpg and y to fcyl
ggplot(mtcars, aes(mpg, fcyl)) +
  geom_point()

# Swap mpg and fcyl
ggplot(mtcars, aes(fcyl, mpg)) +
  geom_point()

# Map x to wt, y to mpg and color to fcyl
ggplot(mtcars, aes(wt, mpg, color = fcyl)) +
  geom_point()

# Set the shape and size of the points
ggplot(mtcars, aes(wt, mpg, color = fcyl)) +
  geom_point(shape = 1, size = 4)

# Map fcyl to fill
ggplot(mtcars, aes(wt, mpg, fill = fcyl)) +
  geom_point(shape = 1, size = 4)

# Change point shape; set alpha
ggplot(mtcars, aes(wt, mpg, fill = fcyl)) +
  geom_point(shape = 21, alpha = 0.6)

# Change point shape; set alpha
ggplot(mtcars, aes(wt, mpg, fill = fcyl)) +
  geom_point(shape = 21, size = 4, alpha = 0.6)

# Map color to fam
ggplot(mtcars, aes(wt, mpg, fill = fcyl, color = fam)) +
  geom_point(shape = 21, size = 4, alpha = 0.6)

# Base layer
plt_mpg_vs_wt <- ggplot(mtcars, aes(wt, mpg))
# Map fcyl to alpha, not size
plt_mpg_vs_wt +
  geom_point(aes(size = fcyl))
# Map fcyl to shape, not alpha
plt_mpg_vs_wt +
  geom_point(aes(alpha = fcyl))
# Map fcyl to shape, not alpha
plt_mpg_vs_wt +
  geom_point(aes(shape = fcyl))
# Use text layer and map fcyl to label
plt_mpg_vs_wt +
  geom_text(aes(label = fcyl))

# A hexadecimal color
my_blue <- "#4ABEFF"
  
ggplot(mtcars, aes(wt, mpg)) +
  # Set the point color and alpha
  geom_point(color = my_blue, alpha = 0.6)

# Change the color mapping to a fill mapping
ggplot(mtcars, aes(wt, mpg, fill = fcyl)) +
  # Set point size and shape
  geom_point(color = my_blue, size = 10, shape = 1)

ggplot(mtcars, aes(wt, mpg, color = fcyl)) +
  # Add text layer with label rownames(mtcars) and color red
  geom_text(label = row.names(mtcars), color = "red")

# 5 aesthetics: add a mapping of size to hp / wt
ggplot(mtcars, aes(mpg, qsec, color = fcyl, shape = fam, size = hp/wt)) +
  geom_point()

ggplot(mtcars, aes(fcyl, fill = fam)) +
  geom_bar() +
  # Set the axis labels
  labs( x = "Number of Cylinders",
        y = "Count")

palette <- c(automatic = "#377EB8", manual = "#E41A1C")
ggplot(mtcars, aes(fcyl, fill = fam)) +
  geom_bar() +
  labs(x = "Number of Cylinders", y = "Count") +
  # Set the fill color scale
  scale_fill_manual("Transmission", values = palette)

palette <- c(automatic = "#377EB8", manual = "#E41A1C")
# Set the position
ggplot(mtcars, aes(fcyl, fill = fam)) +
  geom_bar(position = "dodge") +
  labs(x = "Number of Cylinders", y = "Count")
scale_fill_manual("Transmission", values = palette)

# Plot 0 vs. mpg
ggplot(mtcars, aes(mpg, 0)) +
  # Add jitter 
  geom_point(position = 'jitter')

ggplot(mtcars, aes(mpg, 0)) +
  geom_jitter() +
  # Set the y-axis limits
  ylim(-2,2)

# Plot price vs. carat, colored by clarity
plt_price_vs_carat_by_clarity <- ggplot(diamonds, aes(carat, price, color = clarity))
# Add a point layer with tiny points
plt_price_vs_carat_by_clarity + geom_point(alpha = 0.5, shape =".")

# Plot base
plt_mpg_vs_fcyl_by_fam <- ggplot(mtcars, aes(mpg, fcyl, color = fam))
# Default points are shown for comparison
plt_mpg_vs_fcyl_by_fam + geom_point()

# Plot base
plt_mpg_vs_fcyl_by_fam <- ggplot(mtcars, aes(fcyl, mpg, color = fam))
# Default points are shown for comparison
plt_mpg_vs_fcyl_by_fam + geom_point()
# Alter the point positions by jittering, width 0.3
plt_mpg_vs_fcyl_by_fam + geom_point(position = position_jitter(width = 0.3))

# Now jitter and dodge the point positions
plt_mpg_vs_fcyl_by_fam + geom_point(position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.3))

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  # Use a jitter position function with width 0.1
  geom_point(position = position_jitter(width = 0.3), alpha = 0.5)

# Plot mpg
ggplot(mtcars, aes(x = mpg)) +
  # Add a histogram layer
  geom_histogram()

# Set the binwidth to 1
ggplot(mtcars, aes(mpg)) +
  geom_histogram(binwidth = 1)

# Map y to ..density..
ggplot(mtcars, aes(mpg, ..density..)) +
  geom_histogram(binwidth = 1)

datacamp_light_blue <- "#51A8C9"
  ggplot(mtcars, aes(mpg, ..density..)) +
    # Set the fill color to datacamp_light_blue
    geom_histogram(fill = datacamp_light_blue,
                   binwidth = 1)
  
# Update the aesthetics so the fill color is by fam
ggplot(mtcars, aes(mpg, fill = fam)) +
  geom_histogram(binwidth = 1)
  
ggplot(mtcars, aes(mpg, fill = fam)) +
  # Change the position to dodge
  geom_histogram(binwidth = 1,
                 position = "dodge")  

ggplot(mtcars, aes(mpg, fill = fam)) +  
# Change the position to fill
geom_histogram(binwidth = 1, position = "fill") 
  
ggplot(mtcars, aes(mpg, fill = fam)) +
  # Change the position to identity, with transparency 0.4
  geom_histogram(binwidth = 1, position = "identity",
                 alpha = 0.4)  
  
data(sleep)  
  
# Plot fcyl, filled by fam
ggplot(mtcars, aes(fcyl, fill = fam)) +
  # Add a bar layer
  geom_bar()  
  
# Set the position to "fill"
ggplot(mtcars, aes(fcyl, fill = fam)) +
  geom_bar(position = "fill")

# Change the position to "dodge"
ggplot(mtcars, aes(fcyl, fill = fam)) +
  geom_bar(position = "dodge")

# Change position to use the functional form, with width 0.2
ggplot(mtcars, aes(cyl, fill = fam)) +
  geom_bar(position = position_dodge(width = 0.2))

# Set the transparency to 0.6
ggplot(mtcars, aes(cyl, fill = fam)) +
  geom_bar(position = position_dodge(width = 0.2),
           alpha = 0.6)

# Print the head of economics
head(economics)
# Using economics, plot unemploy vs. date
ggplot(economics, aes(date, unemploy)) +
  # Make it a line plot
  geom_line()

# Change the y-axis to the proportion of the population that is unemployed
ggplot(economics, aes(date, unemploy/pop)) +
  geom_line()

# View the default plot
plt_prop_unemployed_over_time

# Remove legend entirely
plt_prop_unemployed_over_time +
  theme(legend.position = "none")

# Position the legend at the bottom of the plot
plt_prop_unemployed_over_time +
  theme(legend.position = "bottom")

# Position the legend inside the plot at (0.6, 0.1)
plt_prop_unemployed_over_time +
  theme(legend.position = c(0.6, 0.1))

plt_prop_unemployed_over_time +
  theme(
    # For all rectangles, set the fill color to grey92
    rect = element_rect(fill= "grey92"),
    # For the legend key, turn off the outline
    legend.key = element_rect(color = NA)
  )

plt_prop_unemployed_over_time +
  theme(
    rect = element_rect(fill = "grey92"),
    legend.key = element_rect(color = NA),
    # Turn off axis ticks
    axis.ticks = element_blank(),
    # Turn off the panel grid
    panel.grid = element_blank()
  )

plt_prop_unemployed_over_time +
  theme(
    rect = element_rect(fill = "grey92"),
    legend.key = element_rect(color = NA),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    # Add major y-axis panel grid lines back
    panel.grid.major.y = element_line(
      # Set the color to white
      color = 'white',
      # Set the size to 0.5
      size = 0.5,
      # Set the line type to dotted
      linetype = 'dotted'
    )
  )

plt_prop_unemployed_over_time +
  theme(
    rect = element_rect(fill = "grey92"),
    legend.key = element_rect(color = NA),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(
      color = "white",
      size = 0.5,
      linetype = "dotted"
    ),
    # Set the axis text color to grey25
    axis.text = element_text(color = 'grey25'),
    # Set the plot title font face to italic and font size to 16
    plot.title = element_text(size = 16, face = 'italic')
  )

# View the original plot
plt_mpg_vs_wt_by_cyl

plt_mpg_vs_wt_by_cyl +
  theme(
    # Set the axis tick length to 2 lines
    axis.ticks.length = unit(2,'lines')
  )

plt_mpg_vs_wt_by_cyl +
  theme(
    # Set the legend key size to 3 centimeters
    legend.key.size = unit(3, 'cm')
  )

plt_mpg_vs_wt_by_cyl +
  theme(
    # Set the legend margin to (20, 30, 40, 50) points
    legend.margin = margin(20, 30, 40, 50, 'pt')
  )

plt_mpg_vs_wt_by_cyl +
  theme(
    # Set the plot margin to (10, 30, 50, 70) millimeters
    plot.margin = margin(10, 30, 50, 70, 'mm')
  )

# Add a black and white theme
plt_prop_unemployed_over_time +
  theme_bw()

# Add a classic theme
plt_prop_unemployed_over_time +
  theme_classic()

# Add a void theme
plt_prop_unemployed_over_time +
  theme_void()

# Use the fivethirtyeight theme
plt_prop_unemployed_over_time +
  theme_fivethirtyeight()

# Use Tufte's theme
plt_prop_unemployed_over_time +
  theme_tufte()

# Use the Wall Street Journal theme
plt_prop_unemployed_over_time +
  theme_wsj()



# Theme layer saved as an object, theme_recession
theme_recession <- theme(
  rect = element_rect(fill = "grey92"),
  legend.key = element_rect(color = NA),
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  panel.grid.major.y = element_line(color = "white", size = 0.5, linetype = "dotted"),
  axis.text = element_text(color = "grey25"),
  plot.title = element_text(face = "italic", size = 16),
  legend.position = c(0.6, 0.1)
)
# Combine the Tufte theme with theme_recession
theme_tufte_recession <- theme_tufte() + theme_recession
# Add the recession theme to the plot
plt_prop_unemployed_over_time + theme_tufte_recession

theme_recession <- theme(
  rect = element_rect(fill = "grey92"),
  legend.key = element_rect(color = NA),
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  panel.grid.major.y = element_line(color = "white", size = 0.5, linetype = "dotted"),
  axis.text = element_text(color = "grey25"),
  plot.title = element_text(face = "italic", size = 16),
  legend.position = c(0.6, 0.1)
)
theme_tufte_recession <- theme_tufte() + theme_recession
# Set theme_tufte_recession as the default theme
theme_set(theme_tufte_recession)
# Draw the plot (without explicitly adding a theme)
plt_prop_unemployed_over_time

plt_prop_unemployed_over_time +
  theme_tufte() +
  # Add individual theme elements
  theme(
    # Turn off the legend
    legend.position = 'none',
    # Turn off the axis ticks
    axis.ticks = element_blank()
  )

plt_prop_unemployed_over_time +
  theme_tufte() +
  theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    # Set the axis title's text color to grey60
    axis.title = element_text(color = 'grey60'),
    # Set the axis text's text color to grey60
    axis.text = element_text(color = 'grey60')
  )

plt_prop_unemployed_over_time +
  theme_tufte() +
  theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    axis.title = element_text(color = "grey60"),
    axis.text = element_text(color = "grey60"),
    # Set the panel gridlines major y values
    panel.grid.major.y = element_line(
      # Set the color to grey60
      color = 'grey60',
      # Set the size to 0.25
      size = 0.25,
      # Set the linetype to dotted
      linetype = 'dotted'
    )
  )

# Add a geom_segment() layer
ggplot(gm2007, aes(x = lifeExp, y = country, color = lifeExp)) +
  geom_point(size = 4) +
  geom_segment(aes(xend = 30, yend = country), size = 2)

# Add a geom_text() layer
ggplot(gm2007, aes(x = lifeExp, y = country, color = lifeExp)) +
  geom_point(size = 4) +
  geom_segment(aes(xend = 30, yend = country), size = 2) +
  geom_text(aes(label = lifeExp), color = 'white', size = 1.5)

# Set the color scale
palette <- brewer.pal(5, "RdYlBu")[-(2:4)]

# Modify the scales
ggplot(gm2007, aes(x = lifeExp, y = country, color = lifeExp)) +
  geom_point(size = 4) +
  geom_segment(aes(xend = 30, yend = country), size = 2) +
  geom_text(aes(label = round(lifeExp,1)), color = "white", size = 1.5) +
  scale_x_continuous("", expand = c(0, 0), limits = c(30, 90), position = 'top') +
  scale_color_gradientn(colors = palette)

# Set the color scale
palette <- brewer.pal(5, "RdYlBu")[-(2:4)]

# Add a title and caption
ggplot(gm2007, aes(x = lifeExp, y = country, color = lifeExp)) +
  geom_point(size = 4) +
  geom_segment(aes(xend = 30, yend = country), size = 2) +
  geom_text(aes(label = round(lifeExp,1)), color = "white", size = 1.5) +
  scale_x_continuous("", expand = c(0,0), limits = c(30,90), position = "top") +
  scale_color_gradientn(colors = palette) +
  labs(title = "Highest and lowest life expectancies, 2007", caption = "Source: gapminder")

# Define the theme
plt_country_vs_lifeExp +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(color = 'black'),
        axis.title = element_blank(),
        legend.position = 'none')

# Add a vertical line
plt_country_vs_lifeExp +
  step_1_themes +
  geom_vline(xintercept = global_mean, color = 'grey40', linetype = 3)

# Add a curve
plt_country_vs_lifeExp +  
  step_1_themes +
  geom_vline(xintercept = global_mean, color = "grey40", linetype = 3) +
  step_3_annotation +
  annotate(
    "curve",
    x = x_start, y = y_start,
    xend = x_end, yend = y_end,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    color = "grey40"
  )