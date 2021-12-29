library(shiny)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(patchwork)
library(bslib)


uiuc_covid <- read_csv("data/Illinois_shield_covid_data.csv")

uiuc_covid_updated <- uiuc_covid %>% mutate(totalCases = cumsum(totalNewCases)) %>% 
    mutate(total_tests_Taken = cumsum(totalNewTests)) %>%
    mutate(Positivity_rate = (totalNewCases) / (totalNewTests)) %>%
    mutate(Dates = dmy(uiuc_covid$time)) %>%  
    mutate(UG_Positivity_rate = undergradCases / undergradTests) %>%
    mutate(Grad_Positivity_rate = gradCases / gradTests) %>%
    mutate(Faculty_Positivity_rate = facStaffCases / facStaffTests) %>%
    select(Dates, totalNewCases:Faculty_Positivity_rate) 


UIUC_groups <- c("Undergraduates", 
                 "Graduates", 
                 "Faculty/Staff")

ui <- navbarPage(
    theme = bs_theme(bootswatch = "darkly"),
    title = "Page",
    tabPanel("App", 
             titlePanel("2020-2021 UIUC Covid Tracker"),
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "groups", label = "Group", choices = UIUC_groups), 
                     checkboxInput(inputId = "positivity", label = "Positivity Rate"),
                     dateRangeInput(inputId = "date", label = "Date Range",
                                    start  = "2020-08-17",
                                    end    = "2021-11-27",
                                    min    = "2020-08-17",
                                    max    = "2021-11-27",
                                    format = "mm/dd/yy",
                                    separator = " - "),
                 ),
                 
                 mainPanel(plotOutput("selectPlot"), plotOutput("sliderPlot")))),
    tabPanel("Table", dataTableOutput("table")),
    tabPanel("About", includeMarkdown("about.Rmd")))

as.numeric(uiuc_covid_updated$undergradCases)
as.numeric(uiuc_covid_updated$gradCases)
as.numeric(uiuc_covid_updated$facStaffCases)
as.numeric(uiuc_covid_updated$UG_Positivity_rate)
as.numeric(uiuc_covid_updated$Grad_Positivity_rate)
as.numeric(uiuc_covid_updated$Faculty_Positivity_rate)

server <- function(input, output) {
    
    daterange <- reactive({
        uiuc_covid_updated %>% filter(uiuc_covid_updated$Dates >= input$date[1] 
                                      & uiuc_covid_updated$Dates <= input$date[2])
    })
    
    tablereact <- reactive({ 
        if (input$groups == 'Undergraduates') { 
            uiuc_covid_updated %>% select(Dates, undergradCases, undergradTests, UG_Positivity_rate)
        } else if (input$groups == 'Graduates') { 
            uiuc_covid_updated %>% select(Dates, gradCases, gradTests, Grad_Positivity_rate)
        } else if (input$groups == 'Faculty/Staff') { 
            uiuc_covid_updated %>% select(Dates, facStaffCases, facStaffTests, Faculty_Positivity_rate)
        } 
    })
    
    
    output$table = renderDataTable(tablereact())
    
    output$selectPlot <- renderPlot({
        
        if (input$groups == 'Undergraduates') { 
            cases_plot = ggplot(data = daterange(), aes(x = Dates, y = undergradCases)) + 
                geom_col(fill = 'blue', alpha = 0.6) + 
                theme_minimal(base_size = 14) +
                xlab("Date") + ylab("Daily Cases") + scale_x_date(date_labels = "%Y-%m-%d")
            cases_plot
            
        } else if (input$groups == 'Graduates') { 
            cases_plot = ggplot(data = daterange(), aes(x = Dates, y = gradCases)) + 
                geom_col(fill = 'dark red', alpha = 0.6) + 
                theme_minimal(base_size = 14) +
                xlab("Date") + ylab("Daily Cases") + scale_x_date(date_labels = "%Y-%m-%d")
            cases_plot
            
        } else if (input$groups == 'Faculty/Staff') { 
            cases_plot = ggplot(data = daterange(), aes(x = Dates, y = facStaffCases)) + 
                geom_col(fill = 'dark green', alpha = 0.6) + 
                theme_minimal(base_size = 14) +
                xlab("Date") + ylab("Daily Cases") + scale_x_date(date_labels = "%Y-%m-%d")
            cases_plot
        }
        
    })
    
    
    checkreactive <- reactive({ 
        if (input$positivity) { 
            if (input$groups == 'Undergraduates') { 
                pr_plot <- ggplot(data = daterange(), aes(x = Dates, y = UG_Positivity_rate)) + 
                    geom_line() + xlab("Date") + ylab("Positivity Rate (%)")
                pr_plot
            } else if (input$groups == 'Graduates') { 
                pr_plot <- ggplot(data = daterange(), aes(x = Dates, y = Grad_Positivity_rate)) + 
                    geom_line() + xlab("Date") + ylab("Positivity Rate (%)")
                pr_plot
            } else if (input$groups == 'Faculty/Staff') { 
                pr_plot <- ggplot(data = daterange(), aes(x = Dates, y = Faculty_Positivity_rate)) + 
                    geom_line() + xlab("Date") + ylab("Positivity Rate (%)") 
                pr_plot
            }
        }
        
    })
    
    output$sliderPlot <- renderPlot({ 
        checkreactive()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
