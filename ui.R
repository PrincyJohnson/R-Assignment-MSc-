shinyUI(fluidPage(
  
  titlePanel("Shiny for Assignment 2"),
  br(),
  br(),
  br(),
  
  sidebarPanel(
        # SlidebarPanel for file upload tab
    conditionalPanel(condition = "$('li.active a').first().html()==='Data View'",
                     fileInput('file1', 'Choose CSV File',
                               accept=c('text/csv', 
                                        'text/comma-separated-values,text/plain', 
                                        '.csv')),
                     tags$hr(),
                     checkboxInput('header', 'Header', TRUE),
                     radioButtons('sep', 'Separator',
                                  c(Comma=',',
                                    Semicolon=';',
                                    Tab='\t'),
                                  ','),),
    
    # SliderbarPanel for t-test tab
    conditionalPanel(condition = "$('li.active a').first().html()==='T-test'",
                    
                     radioButtons("sample",
                                  "Performing a two sample t test:",
                                  choices = c("Two sample" = "twoSamp")),
                    
    conditionalPanel(condition = "input.sample == 'twoSamp'",
                     selectInput("var1", 
                                 label = "Select a Variable",
                                 ""),
                     selectInput("var2", 
                                 label = "Select a Variable",
                                 ""),
                    
                     ),
                     selectInput("tail",
                                 label = "Please Select a relationship you
                                 want to test:",
                                 choices = c("Equal" = "two.sided", 
                                             "Less" = "less",
                                             "Greater" = "greater")
                                 ),
    conditionalPanel(condition = "input.sample == 'oneSamp'"),
                     h2("Confidence level",
                        p("The confidence level for the t test is set at 0.95")),
                     ),
    
    #sliderbar for heatmap
    conditionalPanel(condition = "$('li.active a').first().html()==='Heatmap'",
                                 h2 ("Press the below tab to get heatmap"),
                                 actionButton('getHmap', 'get heatmap'),
                     
                     ),
    
  ),
  mainPanel(
    tabsetPanel(
      
      tabPanel('Data View', 
               fluidRow(column(10, offset = 1,
                               h2("Data Table"),
                               tableOutput('contents')))      
      ),           
      tabPanel('T-test',
               fluidRow(column(8, offset = 1,
                               h2("summary table"),
                               p("sample statistics:"),
                               tableOutput('parametric'),
                               h2("T-test values"),
                               p("The observed t-test:"),
                               textOutput('tvalue'),
                               p("The p-value :"),
                               textOutput('pvalue'),
                               ))
      ),
      tabPanel('Heatmap',
              fluidRow(column(8, offset = 1,
                              h2("Heatmap"),
                              plotOutput("Heatplot"),
                              ))),
      tabPanel('Diagnosis',
               fluidPage(column(8, offset = 1),
                               h2("Controls"), tableOutput('outputld2'),
               fluidRow(column(8, offset = 1)),
               h3("Samples"), tableOutput('outputld3'),
               fluidRow(column(8, offset =1)), 
               h3("T-Test for controls vs samples"), tableOutput('pvalue2'),
               fluidRow(column(8, offset =1)), 
               h3("Diagnosis"), tableOutput('diagnosis')) 
      )
      
  )
)))