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
library(ggplot2)

all_projection <- readRDS("shiny.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Lifetime Age Pension Projection in Australia"),

    # Sidebar 
    sidebarLayout(
        
      sidebarPanel(
        selectInput(inputId = "frequency", width = '100%', 
                    label = "Projection Frequency",
                    choices = c("1 year", "5 years")),
        
        # fluidRow(column(3, checkboxInput("state", "ACT", TRUE)),
        #          column(3, checkboxInput("state", "NSW", TRUE)),
        #          column(3, checkboxInput("state", "NT", TRUE)),
        #          column(3, checkboxInput("state", "QLD", TRUE)),
        #          column(3, checkboxInput("state", "SA", TRUE)),
        #          column(3, checkboxInput("state", "TAS", TRUE)),
        #          column(3, checkboxInput("state", "VIC", TRUE)),
        #          column(3, checkboxInput("state", "WA", TRUE))
        #          ),
        
        checkboxGroupInput("state", "States in plots",
                           c("ACT", "NSW", "QLD", "WA", "NT", "VIC", "TAS", "SA"), 
                           selected = c("ACT", "NSW", "QLD", "WA", "NT", "VIC", "TAS", "SA")),
        
        # # Button
        # downloadButton("download_1", "Download 1-year projections"), 
        # 
        # downloadButton("download_2", "Download 5-year projections")
        ),

        # Plot
        mainPanel(
          tabsetPanel(
            tabPanel("Visualisation", 
                     fluidRow(column(10, plotOutput(outputId = "plot_female"))), 
                     fluidRow(column(10, plotOutput(outputId = "plot_male"))), 
                     fluidRow(column(10, plotOutput(outputId = "plot_mixed"))), 
                     ),
            tabPanel("Projections", h1("Lifetime Income Projections"), 
                     h2("1-year frequency"),
                     dataTableOutput("table_1"),
                     h2("5-year frequency"),
                     dataTableOutput("table_2"))
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$plot_female <- renderPlot({
      if(input$frequency == "1 year")
      {
        df_all_f = data.frame(State = rep(c("ACT", "NSW", "QLD", "WA", "NT", "VIC", "TAS", "SA"), 10), 
                        Year = rep(2023:2032, each = 8), Income = round(as.vector(all_projection$one_year$female)/1000))
        
        limits_val = c(2023, 2032)
        breaks_val = c(2023, 2025, 2027, 2029, 2031)
      }
      
      if(input$frequency == "5 years")
      {
        df_all_f = data.frame(State = rep(c("ACT", "NSW", "QLD", "WA", "NT", "VIC", "TAS", "SA"), 7),
                            Year = rep(c(2022, 2027, 2032, 2037, 2042, 2047, 2051), each = 8), Income = round(as.vector(all_projection$five_year$female)/1000))
        
        limits_val = c(2022, 2051)
        breaks_val = c(2025, 2030, 2035, 2040, 2045, 2050)
      }
      
      df_f = df_all_f %>% filter(State %in% input$state)
      
      ggplot(df_f, aes(x = Year, y = Income, colour = State)) + geom_line() +
        scale_x_continuous(limits = limits_val, breaks = breaks_val) +
        ylab("Lifetime Age Pension Income ($AUD in thousands)") + ggtitle("Female") + 
        theme(axis.text = element_text(size=10), axis.title.x = element_text(size=15), axis.title.y = element_text(size=15), 
              legend.text = element_text(size=10), legend.title = element_text(size=10)) + 
        theme(plot.title = element_text(size=22)) + theme(
          legend.position = c(1, 0),
          legend.justification = c("right", "bottom"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6)
        ) + guides(colour=guide_legend(ncol=2))
    })
  
  
    output$plot_male <- renderPlot({
      if(input$frequency == "1 year")
      {
        df_all_m = data.frame(State = rep(c("ACT", "NSW", "QLD", "WA", "NT", "VIC", "TAS", "SA"), 10), 
                        Year = rep(2023:2032, each = 8), Income = round(as.vector(all_projection$one_year$male)/1000))
        
        limits_val = c(2023, 2032)
        breaks_val = c(2023, 2025, 2027, 2029, 2031)
      }
      
      if(input$frequency == "5 years")
      {
        df_all_m = data.frame(State = rep(c("ACT", "NSW", "QLD", "WA", "NT", "VIC", "TAS", "SA"), 7),
                            Year = rep(c(2022, 2027, 2032, 2037, 2042, 2047, 2051), each = 8), Income = round(as.vector(all_projection$five_year$male)/1000))
        
        limits_val = c(2022, 2051)
        breaks_val = c(2025, 2030, 2035, 2040, 2045, 2050)
      }
      
      df_m = df_all_m %>% filter(State %in% input$state)
      
      ggplot(df_m, aes(x = Year, y = Income, colour = State)) + geom_line() +
        scale_x_continuous(limits = limits_val, breaks = breaks_val) +
        ylab("Lifetime Age Pension Income ($AUD in thousands)") + ggtitle("Male") + 
        theme(axis.text = element_text(size=10), axis.title.x = element_text(size=15), axis.title.y = element_text(size=15), 
              legend.text = element_text(size=10), legend.title = element_text(size=10)) + 
        theme(plot.title = element_text(size=22))+ theme(
          legend.position = c(1, 0),
          legend.justification = c("right", "bottom"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6)
        ) + guides(colour=guide_legend(ncol=2))
    })
    
    output$plot_mixed <- renderPlot({
      if(input$frequency == "1 year")
      {
        df_all_c = data.frame(State = rep(c("ACT", "NSW", "QLD", "WA", "NT", "VIC", "TAS", "SA"), 10), 
                        Year = rep(2023:2032, each = 8), Income = round(as.vector(all_projection$one_year$mixed)/1000))
        limits_val = c(2023, 2032)
        breaks_val = c(2023, 2025, 2027, 2029, 2031)
      }
      
      if(input$frequency == "5 years")
      {
        df_all_c = data.frame(State = rep(c("ACT", "NSW", "QLD", "WA", "NT", "VIC", "TAS", "SA"), 7),
                            Year = rep(c(2022, 2027, 2032, 2037, 2042, 2047, 2051), each = 8), Income = round(as.vector(all_projection$five_year$mixed)/1000))
        
        limits_val = c(2022, 2051)
        breaks_val = c(2025, 2030, 2035, 2040, 2045, 2050)
      }
      
      df_c = df_all_c %>% filter(State %in% input$state)
      
      ggplot(df_c, aes(x = Year, y = Income, colour = State)) + geom_line() +
        scale_x_continuous(limits = limits_val, breaks = breaks_val) +
        ylab("Lifetime Age Pension Income ($AUD in thousands)") + ggtitle("Couple Each") + 
        theme(axis.text = element_text(size=10), axis.title.x = element_text(size=15), axis.title.y = element_text(size=15), 
              legend.text = element_text(size=10), legend.title = element_text(size=10)) + 
        theme(plot.title = element_text(size=22))+ theme(
          legend.position = c(1, 0),
          legend.justification = c("right", "bottom"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6)
        ) + guides(colour=guide_legend(ncol=2))
    })
    
    ## table to show data
    output$table_1 <- renderDataTable({
      df_combined_one_year = data.frame(State = rep(c("ACT", "NSW", "QLD", "WA", "NT", "VIC", "TAS", "SA"), 10), 
                                        Year = rep(2023:2032, each = 8), 
                                        Female = as.vector(all_projection$one_year$female), 
                                        Male = as.vector(all_projection$one_year$male), 
                                        Couple_each = as.vector(all_projection$one_year$mixed))
      df_combined_one_year = df_combined_one_year %>% filter(State %in% input$state)
    })
    
    output$table_2 <- renderDataTable({
      df_combined_five_year = data.frame(State = rep(c("ACT", "NSW", "QLD", "WA", "NT", "VIC", "TAS", "SA"), 7), 
                                         Year = rep(c(2022, 2027, 2032, 2037, 2042, 2047, 2051), each = 8), 
                                         Female = as.vector(all_projection$five_year$female), 
                                         Male = as.vector(all_projection$five_year$male), 
                                         Couple_each = as.vector(all_projection$five_year$mixed))
      
      df_combined_five_year = df_combined_five_year %>% filter(State %in% input$state)
    })
    
    # ## The requested dataset
    # data <- reactive({
    #   df_combined_one_year = data.frame(State = rep(c("ACT", "NSW", "QLD", "WA", "NT", "VIC", "TAS", "SA"), 10),
    #                                     Year = rep(2023:2032, each = 8),
    #                                     Female = as.vector(all_projection$one_year$female),
    #                                     Male = as.vector(all_projection$one_year$male),
    #                                     Couple_each = as.vector(all_projection$one_year$mixed))
    #   df_combined_one_year = df_combined_one_year %>% filter(State %in% input$state)
    # })
    # 
    # ## download csv of selected data
    # output$download_1 = downloadHandler(
    #   # df_combined_one_year = data.frame(State = rep(c("ACT", "NSW", "QLD", "WA", "NT", "VIC", "TAS", "SA"), 10), 
    #   #                                   Year = rep(2023:2032, each = 8), 
    #   #                                   Female = as.vector(all_projection$one_year$female), 
    #   #                                   Male = as.vector(all_projection$one_year$male), 
    #   #                                   Couple_each = as.vector(all_projection$one_year$mixed))
    #   # df_combined_one_year = df_combined_one_year %>% filter(State %in% input$state)
    #   
    #   # filename = function() {
    #   #   # Use the selected dataset as the suggested file name
    #   #   paste0("1-year_pension_income_projection", ".csv")
    #   # }, 
    #   
    #   content = function(file) {
    #     # Write the dataset to the `file` that will be downloaded
    #     write.csv(df_combined_one_year, "1-year_pension_income_projection.csv")
    #   }
    #   # 
    #   # content = function(file) {
    #   #   # Write the dataset to the `file` that will be downloaded
    #   #   write.csv(df_combined_one_year, file)
    #   # }
    #   # 
    #   # write.csv(df_combined_one_year, "1-year_pension_income_projection.csv")
    # )
}

# Run the application 
shinyApp(ui = ui, server = server)



