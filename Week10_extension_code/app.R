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
ui <- fluidPage(
  
  # Application title
  titlePanel("Impala Movement Data Visualisation"),
  
  sidebarLayout( # this is the overall layout function 
    sidebarPanel( # this is specifically the side bar
      # Dropdown to select individual ID
      selectInput( # selectInput means this will be a toggle tool
                  "idSelect", # give it an easy name
                  "Select Individual:", # this is the name that is displayed to the user
                  choices = sort(unique(data$ID)), # these are the options the user can select from
                  selected = unique(data$ID)[1], # default option is always to display the first option
                  multiple = TRUE), # allow multiple choice
      
      # Date range input
      dateRangeInput( # this will create a calander style selection grid
                    "dateRange", # again, internal tag
                     "Select Date Range:", # displayed name
                     start = min(data$Date), # when do you start it from
                     end   = max(data$Date), # when do you end it to
                     min   = min(data$Date),
                     max   = max(data$Date))
    
    ),
    
    mainPanel( # the main panel of the app shows the plot
      # Display movement plot
      plotOutput("movementPlot", height = "600px")
    )
  )
)

## Define the server -------------------------------------------------------
server <- function(input, output, session) {
  
  # Event-driven filtered data
  filteredData <- eventReactive(input$playButton, {
    req(input$idSelect, input$dateRange, input$dayNight)
    
    data %>%
      filter(ID %in% input$idSelect,
             Date >= input$dateRange[1],
             Date <= input$dateRange[2],
             DayNight %in% input$dayNight) %>%
      group_by(ID) %>%
      filter(n() > 1) %>%
      ungroup()
  })
  
  # Render the plot
  output$movementPlot <- renderPlot({
    df <- filteredData()
    req(nrow(df) > 0)  # make sure there’s data
    
    ggplot(df, aes(x = lon, y = lat, color = ID, group = ID)) +
      geom_path(alpha = 0.5) +
      geom_point(size = 2) +
      labs(x = "Longitude", y = "Latitude", color = "Individual ID") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
}



# Make the app ------------------------------------------------------------
shinyApp(ui = ui, server = server)




# (good data starts after the 2nd July btw)





# Thank you for learning with me ------------------------------------------
# I hope that was an interesting introduction to what can be done with R shiny!
# Every app you build will have different 
# More information can be found at the official website: https://shiny.posit.co/

