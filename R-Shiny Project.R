install.packages("gganimate")
install.packages("ggplot2")
install.packages("shiny")
install.packages("plotly")
install.packages("plyr")
install.packages("dplyr")
install.packages("shinydashboard")

library(ggplot2)
library(shiny)
library(gganimate)
library(plyr)
library(plotly)
library(dplyr)
library(shinydashboard)

pop_data = read.csv(file.choose())
head(pop_data)

theme_set(theme_bw())

ui <- dashboardPage(
  dashboardHeader(title = "Population Dashboard"),
  dashboardSidebar(),
  dashboardBody(plot_ly(
    x = ~pop_data$Year, 
    y = ~pop_data$population, 
    color = ~pop_data$region, 
    frame = ~pop_data$Year, 
    text = ~pop_data$region, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  ) %>%
    layout(
      xaxis = list(
        type = "log"
      )
    )
  )
)

server <- function(input, output) {
  output$plot1 <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')
    
    # now make the animation
    p = ggplot(pop_data, aes(gdp, population, size = population, 
                             color = region, frame = Year)) + geom_point() + scale_x_log10()
    
    gg_animate(p,"outfile.gif")
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)}

shinyApp(ui, server)

#---------------------------DISCUSSION CODE--------------------

regions = aggregate(pop_data, by=list(region=pop_data$region, year=pop_data$Year), FUN=mean, na.rm=TRUE)

America = subset(regions, region=='America')$population

east_asia_pac = subset(regions, region=='East Asia & Pacific')$population

eur_cen_asia = subset(regions, region=='Europe & Central Asia')$population

middle_east_na = subset(regions, region=='Middle East & North Africa')$population

south_asia = subset(regions, region=='South Asia')$population

sub_sah_africa = subset(regions, region=='Sub-Saharan Africa')$population

years = c(1964:2013)

comparison = data.frame(years,America,east_asia_pac,eur_cen_asia,
                        
                        middle_east_na,south_asia,sub_sah_africa)

head(comparison)

population=pop_data$population



ui <- dashboardPage(
  
  dashboardHeader(title = "Population Dashboard"),
  
  dashboardSidebar(),
  
  dashboardBody(plot_ly(comparison, x=~years, y=~sub_sah_africa, type='scatter', mode='lines', name='Sub-Saharan Africa') %>%
                  
                  add_trace(x=~years, y=~America, type='scatter', mode='lines', name='America') %>%
                  
                  add_trace(x=~years, y=~east_asia_pac, type='scatter', mode='lines', name='East Asia & Pacific') %>%
                  
                  add_trace(x=~years, y=~eur_cen_asia, type='scatter', mode='lines', name='Europe & Central Asia') %>%
                  
                  add_trace(x=~years, y=~middle_east_na, type='scatter', mode='lines', name='Middle East & North Africa') %>%
                  
                  add_trace(x=~years, y=~south_asia, type='scatter', mode='lines', name='South Asia') %>%
                  
                  layout(title = "Comparing Average Global Population from 1964 to 2013",
                         
                         xaxis = list(title = "Year"),
                         
                         yaxis = list (title = "Average Population"))
                
                
                
  )
  
)



server <- function(input, output) { }

shinyApp(ui, server)

#--------------------------------------------------------
  library(shiny)
library(dplyr)
library(ggplot2)

pop_data <- read.csv(file.choose())
head(pop_data)


# Define UI for application inspecting Boston property values
ui <- fluidPage(
  
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      # Select variable for y-axis
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("Population" = "population"), 
                  selected = "population"),
      
      # Select variable for x-axis
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("Fertility" = "fertility", 
                              "Life expectancy" = "life",
                              "Population" = "population",
                              "Child Mortality" = "child_mortality",
                              "GDP per capita" = "gdp"), 
                  selected = "child_mortality"),
      
      # Select variable for size
      selectInput(inputId = "z",
                  label = "Size:",
                  choices = c("Fertility" = "fertility", 
                              "Life expectancy" = "life",
                              "Population" = "population",
                              "Child Mortality" = "child_mortality",
                              "GDP per capita" = "gdp"),
                  selected = "population"),
      # Select variable for region
      selectInput(inputId = "w",
                  label = "Region:",
                  choices = c("South Asia" = "South Asia", 
                              "Europe & Central Asia" = "Europe & Central Asia",
                              "Middle East & North Africa" = "Middle East & North Africa",
                              "Sub-Saharan Africa" = "Sub-Saharan Africa",
                              "America" = "America",
                              "East Asia & Pacific" = "East Asia & Pacific"),
                  selected = "South Asia"),
      
      # Copy the line below to make a select box 
      selectInput(inputId = "year", 
                  label = "Year", 
                  choices = c(1964:2013),
                  selected = 2013
      )
    ),
    
    
    # Output
    mainPanel(
      plotOutput(outputId = "scatterplot", click = "plot_click"),
      textOutput(outputId = "correlation")
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create the scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    ggplot(data = subset(pop_data, pop_data$Year==input$year), aes_string(x = input$x, 
                                                                          y = input$y, 
                                                                          size = input$z, 
                                                                          color = "region")) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", se=FALSE, show.legend=FALSE)
    
  })
  
  # Create text output stating the correlation between the two ploted 
  output$correlation <- renderText({
    r1 <- round(cor(subset(pop_data, pop_data$Year==input$year & pop_data$region==selected[1])[, input$x], 
                    subset(pop_data, pop_data$Year==input$year & pop_data$region==selected[1])[, input$y], use = "pairwise"), 3)
    r2 <- round(cor(subset(pop_data, pop_data$Year==input$year & pop_data$region==selected[2])[, input$x], 
                    subset(pop_data, pop_data$Year==input$year & pop_data$region==selected[2])[, input$y], use = "pairwise"), 3)
    paste0(selected[1], " Correlation = ", r1, ".  ", 
           selected[2], " Correlation = ", r2, 
           ".  Note: If the relationship between the two variables is not linear, the correlation coefficient will not be meaningful.")
  })
  
}

# Create a Shiny app object
shinyApp(ui,server)
