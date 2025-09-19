# Week 10 Extension Code --------------------------------------------------

# This week we will take the cleaned GPS dataframe we created last week and make 
# something presentable of it using an R shiny app
# R Shiny is R's version of apps - these are generally hosted locally
# but can also be exported to run on a web page or hosted elsewhere

# in this case, we are going to make a visualisation for the GPS impala data

# Set up ------------------------------------------------------------------
library(pacman)
p_load(shiny,
       tidyverse,
       data.table
       )

# Remember the GPS data we wrangled last week?
# I wrangled a whole bunch of it from multiple different individuals. Lets stitch them together
GPS_files <- list.files(pattern = "Board_GPS_", full.names = TRUE) # list all the individual files
data <- lapply(GPS_files, function(x){
  dat <- fread(x) # read in the data
  ID <- sub("Board_GPS_", "", tools::file_path_sans_ext(basename(x))) # extract the number from the string
    # I did this using a sub function where I'm substituting the Board_GPS_ for "" (nothing)
    # I did this because I'm lazy and it was easy
    # Bonus Task: Try to do the same task with a grep pattern instead.
  dat$ID <- ID # assign the number as the ID
  dat # end all lapply functions with the object you want to return # this is important
})
data <- rbindlist(data) # bind the objects back together into a single object
head(data)

# I want to make some extra columns in the data that allow me to subset it later
data$Date <- as.Date(data$gps_timestamp)
data$DayNight <- ifelse(hour(data$gps_timestamp) > 6 & hour(data$gps_timestamp) < 18, "Day", "Night")

# Learning about R shiny --------------------------------------------------
# An R shiny is created in an R script named 'app.R'
# The code will contain three components:
  # 1. a user interface (ui) object -> controls the layout and aesthetics of the app
  # 2. a server function -> instructions for how to build and use the app
  # 3. a call to the shinyApp function -> combines the interface with the server
# To run the shiny you've made, either:
  # 1. run the call to the shinyApp function (at the bottom of this doc)
  # 2. open the file and hit "Run App" up in the top right
  # 3. if your shiny is part of a bigger program, you can use the function runApp()
    # e.g., runApp("path_to_directory_from_working_directory")... in the case where the 
    # app directory is the working directory, just use runApp()
# okay now we know the basics, lets get into it! :)<3

# Making a basic R shiny ---------------------------------------------------
## Define the UI -----------------------------------------------------------
# Load libraries ------------------------------------------------------------
library(shiny)      # For building the Shiny app
library(ggplot2)    # For plotting
library(dplyr)      # For data manipulation
library(data.table) # If your data is a data.table

# UI ------------------------------------------------------------------------
ui <- fluidPage(
  
  # Application title displayed at the top
  titlePanel("Impala Movement Data Visualisation"),
  
  # Layout with a sidebar and main panel
  sidebarLayout(
    
    # Sidebar panel contains all the input controls
    sidebarPanel(
      
      # Dropdown to select one or more individual IDs
      selectInput(
        inputId = "idSelect",                     # Internal variable name
        label = "Select Individual:",             # Label displayed in UI
        choices = sort(unique(data$ID)),          # All unique IDs from your dataset
        selected = unique(data$ID)[1],           # Default selection (first ID)
        multiple = TRUE                           # Allow multiple IDs to be selected
      ),
      
      # Calendar-style date range selector
      dateRangeInput(
        inputId = "dateRange",                    # Internal variable name
        label = "Select Date Range:",             # Label in UI
        start = min(data$Date),                   # Default start date
        end   = max(data$Date),                   # Default end date
        min   = min(data$Date),                   # Minimum date allowed
        max   = max(data$Date)                    # Maximum date allowed
      ),
      
      # Checkbox group for Day/Night selection
      checkboxGroupInput(
        inputId = "dayNight",                     # Internal variable name
        label = "Select Day/Night:",              # Label in UI
        choices = c("Day", "Night"),              # Options for selection
        selected = c("Day", "Night")             # Default selection (both)
      )
      
    ),
    
    # Main panel displays the plot
    mainPanel(
      plotOutput(
        outputId = "movementPlot",               # Must match the server output
        height = "600px"                         # Height of the plot area
      )
    )
  )
)

# Server --------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive expression to filter data based on user inputs
  filteredData <- reactive({
    # Require inputs to exist before running
    req(input$idSelect, input$dateRange, input$dayNight)
    
    # Filter the data.table / data.frame
    data %>%
      filter(ID %in% input$idSelect,                    # Keep only selected IDs
             Date >= input$dateRange[1],               # Within start date
             Date <= input$dateRange[2],               # Within end date
             DayNight %in% input$dayNight) %>%         # Only selected day/night
      group_by(ID) %>%                                 # Group by ID
      filter(n() > 1) %>%                              # Remove groups with <2 points
      ungroup()                                        # Ungroup for plotting
  })
  
  # Render the movement plot
  output$movementPlot <- renderPlot({
    df <- filteredData()          # Get filtered data
    req(nrow(df) > 0)             # Only plot if there is data
    
    # Create the ggplot
    ggplot(df, aes(x = lon, y = lat, color = ID, group = ID)) +
      geom_path(alpha = 0.5) +        # Draw paths connecting points for each ID
      geom_point(size = 2) +          # Draw points for each GPS fix
      labs(
        x = "Longitude", 
        y = "Latitude", 
        color = "Individual ID"       # Legend label
      ) +
      theme_minimal() +               # Clean minimal theme
      theme(legend.position = "bottom") # Put legend at bottom
  })
  
}

# Run the Shiny app ---------------------------------------------------------
shinyApp(ui = ui, server = server)


# (good data starts after the 2nd July btw)





# Thank you for learning with me ------------------------------------------
# I hope that was an interesting introduction to what can be done with R shiny!
# Every app you build will have different 
# More information can be found at the official website: https://shiny.posit.co/

