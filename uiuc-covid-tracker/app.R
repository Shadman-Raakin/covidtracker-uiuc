

library(shiny)

ui <- navbarPage(
  theme = bs_theme(bootswatch = "darkly"),
  title = "Main Page",
  tabPanel("Main App",
           titlePanel("2020-2022 UIUC Covid Case Tracker"),
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "groups", label = "Group", choices =  Groups),
               checkboxInput(inputId = "positivity", label = "Positivity Rate"),
               dateRangeInput(inputId = "date", label = "Date Range",
                              start  = "2020-08-17",
                              end    = "2022-10-13",
                              min    = "2020-08-17",
                              max    = "2022-10-13",
                              format = "mm/dd/yy",
                              separator = " - "),
             ),
             
             mainPanel(plotOutput("selectPlot"), plotOutput("sliderPlot")))),
  tabPanel("Table", dataTableOutput("table"))
)

server <- function(input, output) {
  daterange <- reactive({
    Updated_Covid_Dataset %>% filter(Updated_Covid_Dataset$Date >= input$date[1] &
                                       Updated_Covid_Dataset$Date <= input$date[2])
  })
  
  output$selectPlot <- renderPlot({
    
    if (input$groups == 'Undergraduates') { 
      cases_plot = ggplot(data = daterange(), aes(x = Date, y = undergradCases)) + 
        geom_col(fill = 'blue', alpha = 1.0) + 
        theme_minimal(base_size = 14) +
        xlab("Date") + ylab("Daily Cases") + scale_x_date(date_labels = "%Y-%m-%d") + ggtitle("Undergraduate Covid-19 Cases")
      cases_plot
      
    } else if (input$groups == 'Graduates') { 
      cases_plot = ggplot(data = daterange(), aes(x = Date, y = gradCases)) + 
        geom_col(fill = 'dark red', alpha = 1.0) + 
        theme_minimal(base_size = 14) +
        xlab("Date") + ylab("Daily Cases") + scale_x_date(date_labels = "%Y-%m-%d") + ggtitle("Graduate Covid-19 Cases")
      cases_plot
      
    } else if (input$groups == 'Faculty/Staff') { 
      cases_plot = ggplot(data = daterange(), aes(x = Date, y = facStaffCases)) + 
        geom_col(fill = 'dark green', alpha = 1.0) + 
        theme_minimal(base_size = 14) +
        xlab("Date") + ylab("Daily Cases") + scale_x_date(date_labels = "%Y-%m-%d") + ggtitle("Faculty Covid-19 Cases")
      cases_plot
    }
    
  })
  
  tablereact <- reactive({ 
    if (input$groups == 'Undergraduates') { 
      Updated_Covid_Dataset %>% select(Date, undergradCases, undergradTests, UG_Positivity_Rate)
    } else if (input$groups == 'Graduates') { 
      Updated_Covid_Dataset %>% select(Date, gradCases, gradTests, Grad_Positivity_Rate)
    } else if (input$groups == 'Faculty/Staff') { 
      Updated_Covid_Dataset %>% select(Date, facStaffCases, facStaffTests, Faculty_Positivity_Rate)
    } 
  })
  
  
  output$table = renderDataTable(tablereact())
  
  
  checkreactive <- reactive({ 
    if (input$positivity) { 
      if (input$groups == 'Undergraduates') { 
        PR_plot <- ggplot(data = daterange(), aes(x = Date, y = UG_Positivity_Rate)) + 
          geom_line() + xlab("Date") + ylab("Positivity Rate (%)") + ggtitle("Positivity Rate")
        PR_plot
      } else if (input$groups == 'Graduates') { 
        PR_plot <- ggplot(data = daterange(), aes(x = Date, y = Grad_Positivity_Rate)) + 
          geom_line() + xlab("Date") + ylab("Positivity Rate (%)") + ggtitle("Positivity Rate")
        PR_plot
      } else if (input$groups == 'Faculty/Staff') { 
        PR_plot <- ggplot(data = daterange(), aes(x = Date, y = Faculty_Positivity_Rate)) + 
          geom_line(alpha = 1.0) + xlab("Date") + ylab("Positivity Rate (%)") + ggtitle("Positivity Rate")
        PR_plot
      }
    }
    
  })
  
  output$sliderPlot <- renderPlot({ 
    checkreactive()
  })
  
}
shinyApp(ui = ui, server = server)
