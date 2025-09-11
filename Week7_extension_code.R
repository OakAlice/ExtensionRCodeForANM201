# Lab 4 Extension Code ----------------------------------------------------
# So you like R code and want to learn more! Yay! :))
# For this extension code, we'll have a look at different ways to make better plots

library(tidyverse)
library(data.table)

data <- read.csv("Week7_data.csv")

# Custom themes -----------------------------------------------------------
# rather than defining all the separate qualities of what you want the plot to look like
# you can use a "theme" that includes a pre-set aesthetic
# so far we have been using theme_minimal() but you can actually build your own

# here's the one I personally use (run example on line 42)
my_theme <- function() {
  theme_minimal(base_size = 10, base_family = "serif") +
    theme(
      panel.border = element_rect(color = "black", linewidth = 1.5, fill = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12) 
    )
}
my_colours = c("aquamarine3", "coral", "orchid3", "slateblue2", "goldenrod2")

# you can find full information on all the changeable ggplot elements using:
?theme
# read through the documentation to learn more

## Get extra fonts --------------------------------------------------------
# for windows computers... (if you have a mac, google the apple version)
windowsFonts() # this will show you the fonts you currently have available
library(extrafont) # to get more fonts
font_import() # this takes like 5 minutes to run 
loadfonts(device = "win") # now you can use all the fonts that are normally in word etc! :))

## Bonus Task 1: Design your own plot theme and colour palette ------------
# play around with making this plot as professional as possible using your custom theme
# remember to add axes titles and stuff
# you can also format the legend a lot more than we have been
ggplot() +
  geom_point(data = data, aes(x = FWW, y = chirp, colour = campus, shape = campus)) +
  geom_smooth(data = data, aes(x = FWW, y = chirp, colour = campus), alpha = 0.2, method = "lm") +
  my_theme() + # here's my aesthetic theme
  scale_color_manual(values=my_colours)  +
  guides(
    colour = guide_legend(            # here's where you can control the legend
      override.aes = list(size = 3),  # bigger legend points
      title.position = "top",
      title.hjust = 0.5,              # change where the title is centred
      label.position = "right",
      ncol = 1                        # if you had more variables you might rearrange
    )
  ) 

# Plotting with ggpubr ----------------------------------------------------
# There is actually a variant of ggplot designed specifically for publications

install.packages("ggpubr")
library(ggpubr)

ggscatter(
  data, 
  x = "FWL", y = "chirp",        # define your variables
  color = "campus",              # colour points by grouping variable # note this is US spelling
  palette = "simpsons",          # pull in a predefined palette. search: ?ggscatter
  add = "reg.line",              # add linear regression line
  conf.int = TRUE                # add confidence interval around regression line
) +
  # Add correlation coefficient
  stat_cor(aes(color = campus),  # match correlation text colour to groups
           label.x = 8,          # adjust position of text so not covered
           label.y = c(3.5, 3)) +# vertically stack the two labels
  theme_pubr()                   # you can change this as well and could define your own for this too!

# Facet-wrapping multiple plotting variables ------------------------------
# you probably already know this?? but it's possible to split your plots into separate panels
# for example, here I've split it by the campus the data was collected from

ggplot() +
  geom_point(data = data, aes(x = FWW, y = chirp)) +
  geom_smooth(data = data, aes(x = FWW, y = chirp), method = "lm") +
  facet_wrap(~campus) # this is the splitting variable here

# you can facet_wrap() by different variables as long as the splitting variable is a column in the data
colnames(data)
# what if I wanted to put the chirp and the freq next to each other? 
# these are currently separate columns so I can't... but I can rearrange my data to make it possible

## Rearranging/wrangling data ---------------------------------------------
# we want to make what was previously 2 columns in the same row to be 2 rows in the same column
data_long <- data %>%
  pivot_longer(
    cols = c(chirp, freq),           # columns to turn into rows
    names_to = "cricket_variable",   # new column with the previous column names
    values_to = "value"              # new column holding the corresponding values
  )

# and now we can plot with that
ggplot(data = data_long, aes(x = Mass, y = value)) + # if the plot and line has same variables, can be put here instead
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~cricket_variable)

# but that sucks because the chirp and freq have such different scales
ggplot(data = data_long, aes(x = Mass, y = value)) + # if the plot and line has same variables, can be put here instead
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~cricket_variable, scales = "free_y") # allow the y scale to be different
  
# cool! and you can fix the aesthetics of these just like you did for the single panel :))

## Bonus Task 2: Make a 6 panel plot ---------------------------------------
# what if we wanted to have the Mass, FWW, and FWL for both the chirp and the freq
# displayed in the same 6 panel plot?
# think about how you would wrangle the data into the correct long format
# make it look publishable!
# look up ?facet_wrap for the formula to wrap the plots

# Function for making plots -----------------------------------------------
# Any time you're going to do the same task twice, you may as well build a function to do it
# considering we do the same thing every week in ANM201 - why not build a function?
# functions are pieces of plug-and-play code that can be repeated anywhere
# below I design a function that can be used to generate a plot for any variables

build_scatterplot <- function(your_data, x_var, y_var){ # here you define the place-holder variables
  # in there you make the code you want to be repeated
  ggplot(data = your_data, aes(x = .data[[x_var]], y = .data[[y_var]])) + # index for the column
    geom_point() +
    geom_smooth(method = "lm")
}

# and then you execute it with any data variables you like
build_scatterplot(your_data = data, x_var = "Mass", y_var = "chirp")
build_scatterplot(your_data = data, x_var = "FWL", y_var = "freq")

## Bonus Task 3: Improve the function -------------------------------------
# add better aesthetics to the function
# make variable axes labels (i.e., you will need to add place-holder variables)
# can you make a function to use for plots in your lab report?

# Extension Code For Next Lab ---------------------------------------------
# I was thinking of showing you how to make automated statistics reports?
# (i.e., instead of having to manually inspect the summary() output)
# If anyone has other ideas, you are so welcome to speak up and I'll add it in!
# Thank you so much for reading the extension code! :))<3
