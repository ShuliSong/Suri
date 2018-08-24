

library(shiny)

shinyUI(navbarPage(
  
  title = ("SIMULATOR 2.0"),
  windowTitle="Simulator for PNP",
  
  ####  Tab1 Input  ####  
  tabPanel('Input', 
           fluidPage(
             titlePanel("Edit and Submit"),
             helpText(h5("Double Click To Edit")),
             helpText(h5("Remeber to !SUBMIT! the table to see the results.")),
             fluidRow(
               column(2, actionButton("reset", "Reset the Table",icon = icon("repeat"))),
               column(2, actionButton("submit", "Submit the Table",icon = icon("upload")))
               ),
             hr(),
            DT::dataTableOutput('test')
            ) 
           ),
  ####  Tab2 results  ####  
  tabPanel('Results',
           fluidPage(
             checkboxGroupInput("levels", "Select Aggregate Levels (at least one):",
                                names(DF)[3:6], selected = names(DF)[5], inline = TRUE),
             hr(),
             DT::dataTableOutput('aggregate_table')
             )
           )
  ))
  
