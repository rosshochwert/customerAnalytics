library(shiny)

shinyUI(navbarPage("Customer Analytics",
  tabPanel("Churn",
           fluidPage(titlePanel("Customer Churn Analysis"),
                       sidebarLayout(
                         sidebarPanel(
                           fileInput('file1', 'Choose CSV File',
                                     accept=c('text/csv', 
                                              'text/comma-separated-values,text/plain', 
                                              '.csv')),
                           tags$hr(),
                           radioButtons('model', 'Model',
                                        c(sBG='sBG',
                                          BdW='BdW',
                                          dW='dW'))
                         ),
                         mainPanel(
                           tabsetPanel(
                             tabPanel("Plot",  plotOutput("churnGraph")), 
                             tabPanel("Summary", htmlOutput("parameters")), 
                             tabPanel("Table", tableOutput("table"))
                           )
                         )
                       ))),
  tabPanel("Count",
           fluidPage(titlePanel("Count Model Analysis"),
                     sidebarLayout(
                       sidebarPanel(
                         fileInput('file2', 'Choose CSV File',
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv')),
                         tags$hr(),
                         radioButtons('count_model', 'Model',
                                      c(NBD='NBD',
                                        ZNBD='ZNBD'))
                       ),
                       mainPanel(
                         tabsetPanel(
                           tabPanel("Plot",  plotOutput("barplot")), 
                           tabPanel("Summary", htmlOutput("count_parameters")), 
                           tabPanel("Table", tableOutput("count_table"))
                         )
                       )
                     ))),
  tabPanel("Trial",
           fluidPage(titlePanel("Trial Data Analysis"),
                     sidebarLayout(
                       sidebarPanel(
                         fileInput('file3', 'Choose CSV File',
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv')),
                         tags$hr(),
                         radioButtons('trial_model', 'Model',
                                      c('Exponential'='Exponential',
                                        'Exponential Gamma'='Exponential_Gamma',
                                        'Weibull Gamma' = 'Weibull_Gamma'
                                        )),
                         numericInput("sample", "Sample:", 10,
                                      min = 1),
                         checkboxInput("nt", "Never-Triers?", FALSE)
                         ),
                         mainPanel(
                           tabsetPanel(
                             tabPanel("Plot",  plotOutput("trial_forecast")), 
                             tabPanel("Summary", htmlOutput("trial_parameters")), 
                             tabPanel("Table", tableOutput("trial_table"))
                           )
                         )
                       ))
    )
))