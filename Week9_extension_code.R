# Week 9 Extension Code ----------------------------------------------------
# Advanced Data Wrangling --------------------------------------------------

# In the last few weeks we've been looking at visualising and communicating data
# but before we can do all that, we need to actually get our data into usable shape!
# Cleaning and rearranging our data is known as 'wrangling'.
# Some people call this 'munging' but... that sounds weird, so I don't lol. 

# this code may possibly be too many steps ahead??? Idk. Let me know how you go??
# also happy to talk about stuff - just ask me in class!

# install.packages("pacman") # package for loading multiple packages
library(pacman)
p_load(data.table, # isn't this so much nicer than repeated library() calls?
       tidyverse,
       stringr,
       lubridate
       )

# Reading in unstructured data --------------------------------------------
# this weeks example data is drawn from a real-life project I'm working on
# this is a GPS txt file readout of a wild Impala in South Africa
# collected as part of a large-scale study by Chris

# The file is called "bonus_code_data.txt"
# before you read it into R, go open it in notepad and have a look at it

# As you can see, it isn't anything like the data we've been working with so far
# There are a mix of legitimate readings and logging statements, with irregular formatting
# read.csv() isn't going to help us here :(

## Alternative file read methods ------------------------------------------
# R's universal file reader is fread(). Lets try it...
data <- fread("Week9_data.txt")
# oops. that just gets an error on line 10 due to an unexpected number of "fields"
# most file readers expect standard formatting. because we dont have that
# we can try reading in each line as its own object...

lines <- readLines("Week9_data.txt")
head(lines) # inspect this and see how it has just literally pulled in the raw strings

## Extracting text strings ------------------------------------------------
# format of the data is a GPS reading (time and location), a blank line, and then the internal clock time
# we're going to ignore everything but the GPS readings

# grep stands for "global regular expression print" and is a Unix encoding that allows
# us to search for particular formatting and strings.

# For example: A simple character vector
words <- c("wow", "coding", "is", "cool!", "WOW!", "code", "bonus_code", "week_9", "5th Lab")
grep("cod", words) # location of the string in 'words' that matches 'cod' (so coding, code, bonus_code)
grep("cod", words, value = TRUE) # gives me those actual words
grep("WOW", words, ignore.case = TRUE, value = TRUE) # gives me the same word but upper and lower case
grep("^cod", words, value = TRUE) # only give me the ones that BEGIN with cod
grep("!$", words, value = TRUE) # give me the ones that END with !

# there is SO MUCH utility to this functionality - you will use this in a million ways!

### Bonus Task 1 ------------------------------------------------------------
# one of the main ways I use this system is to load in specific files
# for example, try to load in this week's practice data using this pattern
# how would you write a pattern that retrieved "Lab5_data_practice.csv" 
# from your working directory but nothing else

file.pattern <- "try_it_here"
files <- list.files() # gives you all files in your working directory
my_file <- list.files(pattern = file.pattern) # retrieve just the files that match the pattern

## Extracting the gps from the txt strings ---------------------------------
# Now we need to design a grep string that will extract the date times

# \\d{2} means "2 digits". \\d{4} would be 4 digits, and so on 
# [0-9.-] means a single character that is either a digit, a dot, or a minus sign
# + means anything can follow
# $ means at the end of the string
# I got chat-gpt to do the rest because I don't use grep enough to memorise it lol

gps_pattern <- "^(\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}:\\d{2}) - Lon:([0-9.-]+), Lat:([0-9.-]+)$"

results <- list() # prepare somewhere for us to save our output

# Loop through lines, looking for a line that will match the GPS string
for (i in seq_along(lines)) { # seq_along() just lists the indexes of the lines so we can iterate them
  
  # i <- 9 # run this line to play along! Increase the number to see what happens for each line
  line <- lines[i]
  
  # Check for GPS line
  if (grepl(gps_pattern, line)) { # match the pattern to the line 
    # if it matches TRUE, then do this... otherwise skip this line entirely
    
    gps_match <- str_extract(line, gps_pattern) # extract anything that matches
    gps_parts <- str_split(gps_match, " ")[[1]] # decompose the line into parts at each " " (empty space)
    # have a look at the gps_parts now
    
    # extract the time and date
    date <- gps_parts[1] # pull out the date (1st object in the split values)
    time <- gps_parts[2] # and so forth
    
    # add the times together
    datetime <- paste(date, time) # paste adds them back together with a space between
    # paste0(date, time, sep = "-") # change the sep to change what gets pasted between the variables
    
    # now lets pull out the location data and clean it up
    lon <- gps_parts[4] # but that still has "Lon:" at the start :(
    lat <- gps_parts[5]
    
    clean_lon <- sub(".*Lon:([0-9.-]+),.*", "\\1", lon) # remove the "Lon:" part of the string
    clean_lat <- sub(".*Lat:([0-9.-]+).*", "\\1", lat) # slightly different as no comma afterwards
    
    # Store result
    results[[length(results) + 1]] <- list(
      datetime = datetime,
      lon = clean_lon,
      lat = clean_lat
    )
  }
}

# results will now be a very long list. Collapse the list by "binding" together
gps_data <- do.call(rbind, lapply(results, as.data.frame))
# inspect our new data!
head(gps_data)

# Fixing the data types ---------------------------------------------------
# because we've read our data in from a txt file, everything is just a character to R
str(gps_data) # even the lon and lat are "chr" instead of numeric

gps_data$lon[1] + gps_data$lon[2] # we can't add them together because they're not numbers

# mutate these into the numeric format
gps_data <- gps_data %>%
  mutate(lon = as.numeric(lon),
         lat = as.numeric(lat))

## Working with date times in R -------------------------------------------
# one of the worst things about computers is how every single system interprets time differently
# working with time and timestamps is the bane of my existence
# the system you'll likely encounter most is the POSIXct object though so lets talk about that

# currently just a character string... R doesn't know what it is
str(gps_data$datetime)[1]
day(gps_data$datetime)[1]  # I asked it to give me the day and it just falsely gave me its best guess
year(gps_data$datetime)[1] # this is wrong again

# but we can tell R how to interpret it
gps_data$formatted_datetime <- as.POSIXct(
  gps_data$datetime,                   # for this column
  format = "%d/%m/%Y %H:%M:%OS",       # this is a type of grep pattern defining the datetime format
  tz = "UTC"                           # this is the timezone we want our datetime to be in
  )

str(gps_data$formatted_datetime)[1]
day(gps_data$formatted_datetime)[1] # yay that's better!
year(gps_data$formatted_datetime)[1]

gps_data <- gps_data %>%
  select(formatted_datetime, lon, lat) %>% # select just the columns you want
  rename(Datetime = formatted_datetime, # rename them with nicer names
         Longitude = lon,
         Latitude = lat)

### Bonus Task 2 ----------------------------------------------------------
# Figure out what the datetime actually is... is it just a special string, or is it some new kind of data entirely?
# I assigned it to UTC but it might have been in local time (Johannesburg/Africa)?
# or maybe Chris set all his clocks to Brisbane (AEST) time?
# Have a play around with changing the timezone and see what happens
# Try adding and subtracting days and times
# If you have a spare moment, I would recommend learning about how time is managed in R vs excel


# All done ----------------------------------------------------------------
# have we succeeded in wrangling our data? lets have a look!
head(gps_data)
ggplot(gps_data, aes(x = Latitude, y = Longitude, colour = Datetime)) +
  geom_point()

# yay we did it! we wrangled an unstructured txt file into meaningful data
# and now we can analyse it to learn something cool about our impala :)

# now that you know it's all cleaned - save it back to disk with fwrite
fwrite(gps_data, "cleaned_bonus_code_data.csv")

# thanks for bearing with me through all of that
# hope this was interesting and exposed you to something new!
