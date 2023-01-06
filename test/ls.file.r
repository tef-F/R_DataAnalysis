# a = list.files(path = "./dataset", pattern = NULL, all.files = FALSE,
#            full.names = FALSE, recursive = FALSE,
#            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
# a

# library(ggcorrplot)
# library(shiny)
# library(plotly)

# ui <- bootstrapPage(
#   #numericInput('n', 'Number of obs', n),
#   plotOutput('plot'),
#   plotlyOutput('plotly')
# )

# # Define the server code
# server <- function(input, output) {
#   output$plot <- renderPlot({
#     cor <- cor(matrix(rnorm(100), ncol = 10))
#     ggcorrplot(cor)
#   })
#   output$plotly <- renderPlotly({
#     cor <- cor(matrix(rnorm(100), ncol = 10))
#     ggplotly(ggcorrplot(cor))
#   })
# }

# shinyApp(ui, server)

library(dplyr)
library(tidyverse)
library(kableExtra)

d = read.csv("dataset/heart_statlog.data.csv")
summary(d)
d %>% glimpse()
d %>% 
  drop_na() %>%
  group_by(target) %>%
  count() %>% 
  ungroup() %>%
  kable(align = rep("c", 2)) %>% kable_styling("full_width" = F)

sample = sample(c(TRUE, FALSE), nrow(d), replace=TRUE, prob=c(0.7,0.3))
table(sample)

mauXaydung = d[sample, ]
mauKiemdinh = d[!sample, ]

moHinh1 = lm(age~ ., data = d)
summary(moHinh1)
