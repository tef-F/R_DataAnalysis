
ui <- dashboardPage(
  
  dashboardHeader(title = "Heart dashboard"),
  
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("home")),
      menuItem("Charts", tabName = "charts", icon = icon("chart-simple")),
      menuItem("Predict", tabName = "predict", icon = icon("gears")),
      menuItem("About", tabName = "about", icon = icon("circle-info"))
    )
  ),
  
  
  
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidPage(
                theme = shinytheme("yeti"),
                titlePanel("Healthcare Analystics"),
                sidebarLayout(
                  sidebarPanel(
                    width = 4,
                    tabsetPanel(
                      tabPanel("DATASET",
                               # checkboxGroupInput("cb_clean",
                               #                    "Clean data:",
                               #                    choices = list("N/A" = 1,
                               #                                   "Type" = 2
                               #                    )
                               # ),
                               checkboxInput("checkNa", "Clean N/A", value = FALSE),
                               checkboxGroupInput("field",
                                                  "Column data:",
                                                  choices = list("age" = 1,
                                                                 "sex" = 2
                                                  )
                               ),
                      )
                      # ,
                      # tabPanel("WORKING",
                      #          radioButtons("dist",
                      #                       "Distribution type:",
                      #                       c("Normal" = "norm",
                      #                         "Uniform" = "unif",
                      #                         "Log-normal" = "l-norm",
                      #                         "Exponential" = "exp")
                      #          ),
                      #          sliderInput(inputId = "bins", 
                      #                      label = "Number of bins:",
                      #                      min = 1,
                      #                      max = 100,
                      #                      value = sample(1:100, 1)
                      #          ),
                      # )
                    ),
                    verbatimTextOutput("result"),
                  ),
                  mainPanel(
                    width = 8,
                    tabsetPanel(
                      id = 'dataset',
                      tabPanel("Table",
                               dataTableOutput('table')
                      ),
                      tabPanel("Plot",
                               plotlyOutput("plot")
                      ),
                      tabPanel("Summary", verbatimTextOutput("summary")),
                      tabPanel("TestTable",
                               dataTableOutput('test_table')
                      ),
                    )
                  )
                )
              )
              
              
      ),
      
      # Second tab content
      tabItem(tabName = "charts",
              tabsetPanel(
                id = 'charts',
                tabPanel("Long Fact",
                         h5("Select categorical vars, recode them to their character values, convert to long format"),
                         fluidRow(
                           column(width = 12,
                                  plotlyOutput("longFact", height = 1500)
                           ),
                         )
                ),
                tabPanel("BoxPlots Numberic",
                         h5("BoxPlots for evaluating the numeric variables"),
                         fluidRow(
                           column(width = 12,
                                  plotlyOutput("boxPlotNumeric", height = 1000)
                           ),
                         )
                ),
                tabPanel("Correlation Matrix",
                         h5("Correlation matrix using Kendall method and Pearson method"),
                         fluidRow(
                           column(width = 6,
                                  plotOutput("matrixKendall", height = 600)
                           ),
                           column(width = 6,
                                  plotOutput("matrixPearson", height = 600)
                           ),
                         ),
                         plotlyOutput("matrixKendall_1", height = 600),
                         plotlyOutput("matrixPearson_1", height = 600)
                ),
                tabPanel("Orthers",
                        
                ),
              ),
      ),
      tabItem(tabName = "predict",
              fluidPage(
                h4("Look at model coefficients and odds ratio for interpretability"),
                fluidRow(
                  dataTableOutput("tablePredict", width = "100%", height = "auto")
                ),
                hr(),
                fluidRow(
                  column(width = 12,
                         h4("Logistic regression"),
                         htmlOutput("tableCoefficients", height = 600),
                  ),
                ),
                hr(),
                fluidRow(
                  column(width = 6,
                         box(width = 12,
                             h5("Table show number of false positives and false negatives"),
                             htmlOutput("tablePrediction", height = 300,),
                         ),
                         box(width = 12,
                             h5("Table display perfomance summary as kable"),
                             htmlOutput("perfomanceSummary", height = 300)
                             
                         )
                  ),
                  column(width = 6,
                         plotlyOutput("matrixConfusion", height = 600)
                  ),
                ),
              ),
              hr(),
              fluidRow(
                column(width = 6,
                       box(width = 12,
                           h5("Model Metrics, 10-Fold Cross Validation"),
                           htmlOutput("tableMetrics", height = 600),
                       ),
                ),
                column(width = 6,
                       plotlyOutput("boxPlot10", height = 600)
                ),
              ),
      ),
      tabItem(tabName = "about",
              includeMarkdown("README.MD")
              
      )
    )
  )
)