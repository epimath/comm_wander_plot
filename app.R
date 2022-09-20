#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggrepel)
library(lubridate)

full_info <- read.csv("data/sample_cdc_community_data.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("CDC Community Level Data Comparison"),

    # Sidebar  
    sidebarLayout(
        sidebarPanel(
            selectInput("select_area", h6(strong("Location:")), 
                        choices = unique(full_info$area), selected = "State"), 
            dateInput("select_date", 
                      h6(strong("As of Date:")), 
                      value = max(full_info$date)), 
            numericInput("select_days_back", 
                         h6(strong("Number of Days Previous:")), 
                         value = 5),
            br(),
            h6(HTML("The data displayed here is pulled from <a href='https://www.mistartmap.info/cdc-community-levels-indicators'>MI Safe Start Map</a>."))
        ),

        # Show a plot
        mainPanel(
           htmlOutput("cdc_data_statement"),
           h6(HTML("<b>To zoom in on a chart, trace an outline on the chart and double-click.</b>"), 
              style="text-align:center"),
           plotOutput("hosp_admit",  dblclick = "hosp_admit_dblclick",
                      brush = brushOpts(
                          id = "hosp_admit_brush",
                          resetOnNew = TRUE
                      )), 
           br(), 
           plotOutput("hosp_inp",  dblclick = "hosp_inp_dblclick",
                      brush = brushOpts(
                          id = "hosp_inp_brush",
                          resetOnNew = TRUE
                      ))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    cdc_data <- reactive({
        
        one_day <- filter(full_info, date >= (as_date(input$select_date) - input$select_days_back) & area == input$select_area)
        one_day <- one_day %>% arrange(date)
    })
    
    cdc_data_text <- reactive({
        one_day2 <- filter(full_info, date >= as_date(input$select_date) & area == input$select_area)
        one_day2 <- one_day2 %>% arrange(date)
        
    })
    
    output$cdc_data_statement <- renderUI({
            HTML(paste0("<p style='text-align:center'><b><font size='+1'>The CDC Community Level for ", input$select_area ," as of ", input$select_date, " is ", cdc_data_text()$covid_19_community_level, ".</font></b></p>"))
    })
    
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    output$hosp_admit <- renderPlot({
        
        Data <- cdc_data()

        ggplot() + 
            geom_ribbon(aes(x = c(0,10), ymin = c(0,0), ymax = c(200,200)), fill = "#00cc99" ) + # green
            geom_ribbon(aes(x = c(10,20), ymin = c(0,0), ymax = c(200,200)), fill = "#ffff99" ) + # yellow
            geom_ribbon(aes(x = c(20,30), ymin = c(0,0), ymax = c(200,200)), fill = "#fc8d59" ) + # yellow
            geom_ribbon(aes(x = c(0,10), ymin = c(200,200), ymax = c(600,600)), fill = "#ffff99" ) +  # yellow
            geom_ribbon(aes(x = c(10,20), ymin = c(200,200), ymax = c(600,600)), fill = "#fc8d59" ) +  # orange
            geom_ribbon(aes(x = c(20,30), ymin = c(200,200), ymax = c(600,600)), fill = "#fc8d59" ) +  # orange
            geom_point(data = Data, aes(x = covid_hospital_admissions_per_100k, y = covid_cases_per_100k), size = 1, alpha = 0.5) + 
            geom_path(data = Data, aes(x = covid_hospital_admissions_per_100k, y = covid_cases_per_100k),arrow = arrow(type = "open", angle = 30, length = unit(0.1, "inches")), size = 1) +
            geom_label_repel(data = tail(Data, n = 3), aes(x = covid_hospital_admissions_per_100k, y = covid_cases_per_100k, label = date), size = 4) + 
            # geom_text(aes(x = 2, y = 500), label = "Medium", size = 5) +
            # geom_text(aes(x = 18, y = 500), label = "High", size = 5) + 
            # geom_text(aes(x = 18, y = 100), label = "Medium", size = 5) + 
            # geom_text(aes(x = 2, y = 100), label = "Low", size = 5) + 
            # scale_color_continuous(guide = "none") +
            labs(x = "Hospital Admissions per 100K", y = "Cases per 100K", color = "Date", fill = "Date") + 
            # ggtitle("Cases vs. Hospitalizations in Washtenaw County") + 
            theme_bw(base_size = 16) + 
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
 
    })
    
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$hosp_admit_dblclick, {
        brush <- input$hosp_admit_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
    
    ranges2 <- reactiveValues(x = NULL, y = NULL)
    
    output$hosp_inp <- renderPlot({
        
        Data <- cdc_data()
        
        ggplot() + 
            geom_ribbon(aes(x = c(0,10), ymin = c(0,0), ymax = c(200,200)), fill = "#00cc99" ) + # green
            geom_ribbon(aes(x = c(10,15), ymin = c(0,0), ymax = c(200,200)), fill = "#ffff99" ) + # yellow
            geom_ribbon(aes(x = c(15,20), ymin = c(0,0), ymax = c(200,200)), fill = "#fc8d59" ) + # yellow
            geom_ribbon(aes(x = c(0,10), ymin = c(200,200), ymax = c(600,600)), fill = "#ffff99" ) +  # yellow
            geom_ribbon(aes(x = c(10,15), ymin = c(200,200), ymax = c(600,600)), fill = "#fc8d59" ) +  # orange
            geom_ribbon(aes(x = c(15,20), ymin = c(200,200), ymax = c(600,600)), fill = "#fc8d59" ) +  # orange
            geom_point(data = Data, aes(x = covid_inpatient_bed_utilization, y = covid_cases_per_100k), size = 1, alpha = 0.5) + 
            geom_path(data = Data, aes(x = covid_inpatient_bed_utilization, y = covid_cases_per_100k),arrow = arrow(type = "open", angle = 30, length = unit(0.1, "inches")), size = 1) +
            geom_label_repel(data = tail(Data, n = 3), aes(x = covid_inpatient_bed_utilization, y = covid_cases_per_100k, label = date), size = 4) + 
            # geom_text(aes(x = 2, y = 500), label = "Medium", size = 5) +
            # geom_text(aes(x = 18, y = 500), label = "High", size = 5) + 
            # geom_text(aes(x = 18, y = 100), label = "Medium", size = 5) + 
            # geom_text(aes(x = 2, y = 100), label = "Low", size = 5) + 
            # scale_color_continuous(guide = "none") +
            labs(x = "Hospital Inpatient Bed Utilization", y = "Cases per 100K", color = "Date", fill = "Date") + 
            # ggtitle("Cases vs. Hospitalizations in Washtenaw County") + 
            theme_bw(base_size = 16) + 
            coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
        
    })
    
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$hosp_inp_dblclick, {
        brush <- input$hosp_inp_brush
        if (!is.null(brush)) {
            ranges2$x <- c(brush$xmin, brush$xmax)
            ranges2$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges2$x <- NULL
            ranges2$y <- NULL
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
