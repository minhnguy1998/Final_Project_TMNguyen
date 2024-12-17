# Load required libraries
library(shiny)
library(leaflet)
library(dplyr)
library(shinydashboard)
library(plotly)
library(tidyr)
#install.packages("timevis")

library(timevis)

# Data for the timeline
timeline_data <- data.frame(
  id      = 1:9,  # Increment IDs for the new event
  content = c(
    "500-700 AD", 
    "900",           # New event
    "1512", 
    "1515-1530", 
    "1530s-1540s", 
    "1660s", 
    "1753-1783", 
    "1764", 
    "1962"
  ),
  start   = c(
    "0500-01-01", 
    "0900-01-01",   # New event date
    "1512-01-01", 
    "1515-01-01", 
    "1530-01-01",
    "1660-01-01", 
    "1753-01-01", 
    "1764-01-01", 
    "1962-01-01"
  )
)


# Descriptions for the events
event_descriptions <- list(
  "1" = "500-700 AD: 
        The first known inhabitants arrived in the islands, likely via Hispaniola.",
  "2" = "900: 
        The larger known Lucayan settlement sites have been built, and consistent trade routes established. 
        Salt, dried conch, and beads from the islands were exchanged for honey, tools, canoes, and fruit 
        from primarily Hispaniola.",
  "3" = "1512: 
        Spanish explorer Ponce de Leon stopped in the islands, likely on Grand Turk.",
  "4" = "1515-1530: 
        The Lucayan populations rapidly decline, leading to the islands being uninhabited.",
  "5" = "1530s-1540s: 
        Complete disappearance of the Lucayans.",
  "6" = "1660s: 
        Bermudian ship ‘wreckers’ salvaged shipwrecks and collected salt.",
  "7" = "1753-1783: 
        The French conducted invasions. The islands were restored to Britain under the Treaty of Versailles.",
  "8" = "1764: 
        Britain officially claimed ownership of the Turks and Caicos Islands.",
  "9" = "1962: 
        After Jamaican independence, the Turks and Caicos decided to remain a British Crown Colony."
)

#Turks GDP Dataset
GDP_Turks <- read.csv("Turks_GDP.csv", header = TRUE)
GDP_Turks$Date <- as.Date(GDP_Turks$Date, format = "%m/%d/%Y")

# Extract only the Year
GDP_Turks$Year <- format(GDP_Turks$Date, "%Y")
GDP_Turks$Year <- as.numeric(GDP_Turks$Year)

GDP_Turks <- GDP_Turks %>%
  rename(
    GDP_billions_USD = GDP..Billions.of.US...,        
    Per_Capita = Per.Capita..US... 
  )
GDP_Turks <- GDP_Turks[order(GDP_Turks$Year), ]  # Sort by Year
GDP_Turks <- na.omit(GDP_Turks)  # Remove rows with NA values
GDP_Turks <- GDP_Turks %>%
  group_by(Year) %>%
  summarize(Per_Capita = mean(Per_Capita, na.rm = TRUE))

#Bahamas GDP Dataset
GDP_Bahamas <- read.csv("Bahamas_GDP.csv",header = TRUE)
GDP_Bahamas <- GDP_Bahamas[-1,]
colnames(GDP_Bahamas)[colnames(GDP_Bahamas) == "GDP.per.capita"] <- "Per_Capita"
GDP_Bahamas$Per_Capita <- as.numeric(gsub("\\$|,", "", GDP_Bahamas$Per_Capita))
GDP_Bahamas <- GDP_Bahamas[1:11, ]
GDP_Bahamas$Year <- as.numeric(GDP_Bahamas$Year)

#Cuba GDP Dataset
GDP_Cuba <- read.csv("Cuba_GDP.csv", header = TRUE)
GDP_Cuba$Year <- as.numeric(GDP_Cuba$Year)
colnames(GDP_Cuba)[colnames(GDP_Cuba) == "GDP.Per.Capita..US..."] <- "Per_Capita"
GDP_Cuba$Per_Capita <- as.numeric(gsub("\\$|,", "", GDP_Cuba$Per_Capita))

#Dominican GDP Dataset
GDP_Dominican <- read.csv("Dominican_GDP.csv", header = TRUE)
GDP_Dominican <- GDP_Dominican[-1,]
colnames(GDP_Dominican)[colnames(GDP_Dominican) == "GDP.per.capita"] <- "Per_Capita"
GDP_Dominican$Per_Capita <- as.numeric(gsub("\\$|,", "", GDP_Dominican$Per_Capita))

ui <- dashboardPage(
  dashboardHeader(title = "The Turks and Caicos Islands"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome", icon = icon("home")),
      menuItem("Overview", tabName = "Island_overview", icon = icon("map")),
      menuItem("Statistics", tabName = "stats", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      # Welcome Page
      tabItem(tabName = "welcome",
              h1("Welcome to the Turks and Caicos Islands"),
              # Use fluidRow and column to layout the content
              fluidRow(
                column(6,  # Image in the left column (50% width)
                       tags$img(src = "Welcome_Turks.png", height = "500px", width = "100%")
                ),
                column(6, 
                       tags$img(src = "flag.png", height = "500px", width = "100%")
                )
              
              ),
           
      ),
      
      # Island Overview Page
      tabItem(tabName = "Island_overview",
              tabsetPanel(
                tabPanel("History",
                         h2("Historical Background"),
                         p("The Turks and Caicos were under the UK colony of Jamaica until 1962 where it became 
                         a separate Crown colony after Jamaica's independence. It was overseen by the governor of 
                         The Bahamas from 1965 to 1973, and by its own governor from 1973 on. The Turks and Caicos 
                         was to receive its own independence in 1982 but the policy was reversed and it remains 
                         a British overseas territory. The name Turks is derived from a species of indigenous cactus, 
                          the Turk’s head (Melocactus intortus). The name Caicos is derived from caya hico, meaning 
                          “string of islands” in the language of the indigenous Lucayan (Arawak) people."), 
                         fluidPage(
                           titlePanel("Interactive Timeline of the Islands’ Major Events"),
                           p("Zoom in and take a scroll through the timeline for some historical facts!"),
                           # Add custom CSS to reduce spacing
                           tags$style(HTML("
                            .timevis-container {
                            margin-bottom: 10px;
                           }
                            #event_details {
                            margin-top: 10px;
                           }
                            ")),
                           timevisOutput("timeline", height = "400px"),
                           
                           # Event description area
                           h4("Event Details"),
                           verbatimTextOutput("event_details")
                         
                         )
                   
                ),
                tabPanel("Geography",
                            h2("Location"),
                            p("The Turks and Caicos consists of two groups of islands located southeast of The Bahamas. 
                              The Turks group is composed of Grand Turk Island, Salt Cay, and lesser cays. The Caicos group 
                              is located northwest of the Turks, separated by a 22-mile, 7,000 ft deep marine trench called 
                              the “Turks Island Passage”, or “The Wall”. The Caicos group consists of South Caicos, East Caicos,
                              Middle Caicos, North Caicos, Providenciales (Provo), and West Caicos Islands. The majority of the 
                              population resides on three islands: South Caicos, Providenciales, and Grand Turk. The capital of 
                              the Turks and Caicos Islands is Cockburn Town, located in the Grand Turk."),
                            tags$div(
                              style = "margin-bottom: 30px;",  # Add spacing below the first map
                              leafletOutput("turks_map", height = "500px")
                            ),
                            tags$div(
                              style = "margin-top: 30px;",  # Add spacing above the second map
                              leafletOutput("turks_surrounding_map", height = "500px")
                            )
                      
                  
                ),
                tabPanel("Government",
                         h2("Structure of Government"),
                         p("The Turks and Caicos is an overseas territory of the UK in the Caribbean. The government 
                           has three main branches: Executive, Legislative, and Judicial. The Executive branch is 
                           comprised of the chief of state (the current monarch of the UK), governor appointed by monarch, 
                           head of government and a cabinet appointed by the governor from the House of Assembly. 
                           The Legislative branch is comprised wholly of the House of Assembly with 21 seats and the 
                           legal system is a mix of English common law and civil law. The judicial branch is comprised 
                           of a Supreme Court with a chief justice and other judges determined by the governor, 
                           a Court of Appeal with a court president and 2 justices, and magistrates' courts."),
                     
                         fluidPage(
                           titlePanel("The Current Cabinet of the Turks and Caicos"),
                           sidebarLayout(
                             sidebarPanel(
                               selectInput(
                                 inputId = "position",
                                 label = "Select a Cabinet Position:",
                                 choices = c(
                                   "Governor" = "governor",
                                   "Premier and Minister of Finance, Investment, and Trade" = "premier",
                                   "Deputy Governor" = "deputy_governor",
                                   "Deputy Premier and Minister of Physical Planning and Infrastructure Development" = "deputy_premier",
                                   "Minister for Tourism, Enviroment, Maritime, Culture, Heritage, and Religios Affairs" = "minister_tour",
                                   "Minister for Home Affairs, Transportation, Broadcasting, Energy and Utilities and Telecommunications Commission" = "minister_home",
                                   "Minister for Immigration & Border Services, Customs, Disasters Management, TCI Port Authority, Civil Aviation & Airport Authority" = "minister_immigration",
                                   "Minister for Education, Labour, Employment, & Customer Service" = "minister_education",
                                   "Minister of Health & Human Services" = "minister_health",
                                   "Minister of Public Safety and Utilities" = "minister_public_safety", 
                                   "Attorney General" = "attorney_general"
                                 )
                               )
                             ),
                             
                             mainPanel(
                               h3("Cabinet Member"),
                               uiOutput("name_output"),  # Output for the name
                             )
                           )
                         )
                ),
                tabPanel("Economy",
                         h2("Economic Status"),
                         p("The Turks and Caicos uses the US dollar and is heavily dependent on the tourism industry 
                           with 90.6% of its GDP in the services industry as of 2017. The Turks and Caicos economy 
                           was hit hard by the Covid-19 pandemic with the GDP growth rate for 2019, 2020, and 2021 being 
                           5.32%, -26.78%, and 2.1%, respectively. All energy on the island is produced from fossil fuels."),
                         fluidPage(
                           titlePanel("Interactive Nested Bar Charts of Visitor Data"),
                           
                           # Main plots
                           h4("Total Visitors Per Year"),
                           p("Click on the bar for each year to display monthly visitor info!"),
                           plotlyOutput("year_plot"),  # Yearly totals
                           
                           h4("Monthly Visitors for Selected Year"),
                           plotlyOutput("month_plot")  # Monthly breakdown
                         ),
                         # Add spacing before the image
                         tags$div(
                           style = "margin-top: 40px;",  # Space of 40px above the image
                           titlePanel("Trend Chart of GDP and Per Capita"),
                           tags$img(src = "GDP_plot.png", height = "700px")
                         )
                         
                         
                ),
              ),
             
              
      ),
      
      # Stats Page
      tabItem(tabName = "stats",
              h2("Per Capita Comparisons"),
              p("Hover over the points on the graphs for exact values"),
              fluidPage(
                titlePanel("Interactive Per Capita Graphs"),
                
                # Plot 1
                div(
                  style = "margin-bottom: 50px;",  # Adds vertical spacing below the first graph
                  plotlyOutput("per_capita_plot_turks", height = "400px")
                ),
                
                # Plot 2
                div(
                  style = "margin-bottom: 50px;",
                  plotlyOutput("per_capita_plot_bahamas", height = "400px")
                ),
                # Plot 3
                div(
                  style = "margin-bottom: 50px;",
                  plotlyOutput("per_capita_plot_cuba", height = "400px")
                ),
                # Plot 4
                div(
                  style = "margin-bottom: 50px;",
                  plotlyOutput("per_capita_plot_dominican", height = "400px")
                )
              )
              
      )
     
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Map
  output$turks_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -71.8, lat = 21.8, zoom = 8) %>%
      addMarkers(lng = -71.1389, lat = 21.4667, popup = "Grand Turk - Capital") %>%
      addMarkers(lng = -72.11, lat = 21.77, popup = "Providenciales - Main tourist hub")
  })
  output$turks_surrounding_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -72.0, lat = 21.5, zoom = 6) %>%  # Center on Turks and Caicos
      
      # Add markers for Turks and Caicos
      addMarkers(lng = -71.1389, lat = 21.4667, popup = "Grand Turk - Capital") %>%
      addMarkers(lng = -72.11, lat = 21.77, popup = "Providenciales - Main tourist hub") %>%
      
      # Add polygons to outline regions
      addPolygons(
        lng = c(-72.5, -71.0, -71.0, -72.5),  # Adjusted longitude values
        lat = c(21.0, 21.0, 22.5, 22.5),      # Adjusted latitude values
        color = "red",
        fillOpacity = 0.3,
        popup = "Turks and Caicos Boundary"
      )
    
  })
  visitor_df <- read.csv("visitor_data.csv", header = TRUE)
  #Clean the year columns (remove commas and convert values to numeric)
  visitor_data_clean <- visitor_df %>%
    mutate(across(starts_with("YR"), ~ as.numeric(gsub(",", "", .))))  # Remove commas and convert
  
  #Rename the year columns to numeric values
  colnames(visitor_data_clean) <- gsub("YR\\.", "", colnames(visitor_data_clean))  # Remove "YR."
  colnames(visitor_data_clean) <- gsub("p", "", colnames(visitor_data_clean))      # Remove "p" (projections)
  visitor_data_long <- visitor_data_clean %>%
    pivot_longer(cols = -MONTH, names_to = "Year", values_to = "Visitors") %>%
    mutate(Year = as.numeric(Year))  # Ensure Year is numeric
  # Define the correct order for months
month_order <- c("January", "February", "March", "April", "May", "June", 
                 "July", "August", "September", "October", "November", "December")

# Convert MONTH to a factor with specified levels
visitor_data_long$MONTH <- factor(visitor_data_long$MONTH, levels = month_order)

# Reactive value to store the selected year
selected_year <- reactiveVal(NULL)

# Main yearly bar chart
output$year_plot <- renderPlotly({
  yearly_data <- visitor_data_long %>%
    group_by(Year) %>%
    summarise(Total_Visitors = sum(Visitors, na.rm = TRUE))
  
  # Convert Year to a factor for proper x-axis display
  yearly_data$Year <- factor(yearly_data$Year)
  
  # Plot total visitors per year
  p <- ggplot(yearly_data, aes(x = Year, y = Total_Visitors)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Total Visitors Per Year", x = "Year", y = "Total Visitors") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Add interactivity
  ggplotly(p, source = "A") %>% 
    event_register("plotly_click")  # Register the click event
})

# Observe the click event to update the selected year
observe({
  event_data <- event_data("plotly_click", source = "A")
  
  if (!is.null(event_data)) {
    # Extract the correct year value using levels of the factor
    clicked_year <- levels(factor(visitor_data_long$Year))[as.numeric(event_data$x)]
    print(paste("Selected year:", clicked_year))  # Debugging
    selected_year(as.numeric(clicked_year))       # Update reactive value
  }
})

# Render monthly bar chart for the selected year
output$month_plot <- renderPlotly({
  req(selected_year())  # Ensure a valid year is selected
  print(paste("Rendering monthly plot for year:", selected_year()))  # Debugging
  
  # Filter data for the selected year
  monthly_data <- visitor_data_long %>%
    filter(Year == selected_year())
  
  # Plot monthly visitors
  p <- ggplot(monthly_data, aes(x = MONTH, y = Visitors, fill = MONTH)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Monthly Visitors for Year", selected_year()), 
         x = "Month", y = "Number of Visitors") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(p)
})

output$timeline <- renderTimevis({
  timevis(
    timeline_data,
    options = list(
      start = "0400-01-01",  # Default visible range starts at 400 AD
      end   = "2000-01-01",  # Default visible range ends at 2000 AD
      min   = "0400-01-01",  # User cannot scroll before 400 AD
      max   = "2000-01-01",  # User cannot scroll beyond 2000 AD
      zoomMin = 1000 * 60 * 60 * 24 * 365 * 50,   # Minimum zoom: 50 years
      zoomMax = 1000 * 60 * 60 * 24 * 365 * 1600  # Maximum zoom: 1600 years
    )
  )
})

# Observe clicks on the timeline and display event descriptions
output$event_details <- renderText({
  clicked_event <- input$timeline_selected
  if (!is.null(clicked_event) && clicked_event != "") {
    return(event_descriptions[[as.character(clicked_event)]])
  } else {
    return("Click on an event to see its details.")
  }
})

cabinet_names <- list(
  governor = list(
    name = "Her Excellency Dileeni Daniel-Selvaratnam",
    tags$img(src = "governor.PNG")
  ),
  premier = list(
    name = "Hon. Charles Washington Misick (OBE)",
    tags$img(src = "premier.PNG")
  ),
  deputy_governor = list(
    name = "Her Excellency Anya Williams",
    tags$img(src = "deputy_governor.PNG")
  ),
  deputy_premier = list(
    name = "Hon. Jamell Rayan Robinson",
    tags$img(src = "deputy_premier.PNG")
  ),
  minister_tour = list(
    name = "Hon. Josephine Olivia Connolly",
    tags$img(src = "minister_tour.PNG")
  ),
  minister_home = list(
    name = "Hon. Otis Chuck Morris",
    tags$img(src = "minister_home.PNG")
  ),
  minister_immigration = list(
    name = "Hon. Arlington Alexander Musgrove",
    tags$img(src = "minister_immigration.PNG")
  ),
  minister_education = list(
    name = "Hon. Rachel Marshal Taylor",
    tags$img(src = "minister_education.PNG")
  ),
  minister_health = list(
    name = "Hon. Shaun David Malcolm",
    tags$img(src = "minister_health.PNG")
  ),
  minister_public_safety = list(
    name = "Hon. Kyle Knowles",
    tags$img(src = "minister_public_safety.PNG")
  ),
  attorney_general = list(
    name = "Hon. Rhondalee Braithwaite-Knowles",
    tags$img(src = "attorney_general.PNG")
  )
)

output$name_output <- renderUI({
  selected_position <- input$position
  
  # Extract name and image from the list
  member_name <- cabinet_names[[selected_position]]$name
  member_image <- cabinet_names[[selected_position]][[2]]  # Extract the image tag
  
  # Combine name and image, with the name displayed on top
  tagList(
    h4(member_name),   # Name of the cabinet member
    member_image       # Image of the cabinet member
  )
})

# Define a common x-axis range
x_limits <- c(min(GDP_Turks$Year), max(GDP_Turks$Year))

# Define a common y-axis range (find min and max of Per_Capita across both datasets)
y_limits <- c(
  min(min(GDP_Turks$Per_Capita, na.rm = TRUE), min(GDP_Bahamas$Per_Capita, na.rm = TRUE),
      min(GDP_Cuba$Per_Capita, na.rm = TRUE), min(GDP_Dominican$Per_Capita, na.rm = TRUE)),
  max(max(GDP_Turks$Per_Capita, na.rm = TRUE), max(GDP_Bahamas$Per_Capita, na.rm = TRUE),
      max(GDP_Cuba$Per_Capita, na.rm = TRUE), max(GDP_Dominican$Per_Capita, na.rm = TRUE))
)

# Turks and Caicos Plot
output$per_capita_plot_turks <- renderPlotly({
  p <- ggplot(GDP_Turks, aes(x = Year, y = Per_Capita, group = 1)) +
    geom_line(color = "red", size = 1) +
    geom_point(color = "blue", size = 2) +
    scale_x_continuous(limits = x_limits) +  # Set x-axis limits
    scale_y_continuous(limits = y_limits) +  # Set y-axis limits
    labs(
      title = "Per Capita Over the Years of the Turks and Caicos",
      x = "Year",
      y = "Per Capita (USD)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(p, tooltip = c("x", "y"))
})

# Bahamas Plot
output$per_capita_plot_bahamas <- renderPlotly({
  p <- ggplot(GDP_Bahamas, aes(x = Year, y = Per_Capita, group = 1)) +
    geom_line(color = "red", size = 1) +
    geom_point(color = "blue", size = 2) +
    scale_x_continuous(limits = x_limits) +  # Set x-axis limits
    scale_y_continuous(limits = y_limits) +  # Set y-axis limits
    labs(
      title = "Per Capita Over the Years of the Bahamas",
      x = "Year",
      y = "Per Capita (USD)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(p, tooltip = c("x", "y"))
})

output$per_capita_plot_cuba <- renderPlotly({
  p <- ggplot(GDP_Cuba, aes(x = Year, y = Per_Capita, group = 1)) +
    geom_line(color = "red", size = 1) +
    geom_point(color = "blue", size = 2) +
    scale_x_continuous(limits = x_limits) +  # Set x-axis limits
    scale_y_continuous(limits = y_limits) +  # Set y-axis limits
    labs(
      title = "Per Capita Over the Years of Cuba",
      x = "Year",
      y = "Per Capita (USD)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(p, tooltip = c("x", "y"))
})

output$per_capita_plot_dominican <- renderPlotly({
  p <- ggplot(GDP_Dominican, aes(x = Year, y = Per_Capita, group = 1)) +
    geom_line(color = "red", size = 1) +
    geom_point(color = "blue", size = 2) +
    scale_x_continuous(limits = x_limits) +  # Set x-axis limits
    scale_y_continuous(limits = y_limits) +  # Set y-axis limits
    labs(
      title = "Per Capita Over the Years of the Dominican Republic",
      x = "Year",
      y = "Per Capita (USD)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(p, tooltip = c("x", "y"))
})
}


# Run the application
shinyApp(ui = ui, server = server)
