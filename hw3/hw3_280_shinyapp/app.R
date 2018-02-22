library(shiny)
library(ggplot2)
library(knitr)
library(dplyr)

df <- readRDS("/home/sarahh.jii/biostat-m280-2018-winter/hw3/LA_payroll.rds") 

df1 = df %>% mutate(id = row_number()) %>%
  select(names(df)[c(1)], id) %>% gather(variable, value, -id)
df2 = df %>% mutate(id = row_number()) %>%
  select(names(df)[c(3:7)], id) %>% gather(variable, value, -id)
df3 = left_join(df1, df2, by = "id")
names(df3) = c("id", "y", "Year","variable","Dollars")


ui <- fluidPage(
  titlePanel(title=h4("Payroll", align="center")),
  
  navbarPage("LA Payroll",
             tabPanel("Question 2",
                      pageWithSidebar(
                        headerPanel('Total LA Payroll'),
                        sidebarPanel(
                          selectInput('yr', 'Year',
                                      sort(levels(as.factor(df$Year)),
                                           decreasing = T))
                        ),
                        mainPanel(
                          tableOutput('summary'), plotOutput("plot1"), plotOutput("plot2")
                        ))),
             tabPanel("Question 3",
                      pageWithSidebar(
                        headerPanel('Who earned the most?'),
                        sidebarPanel(
                          selectInput('yr', 'Year',
                                      sort(levels(as.factor(df$Year)),
                                           decreasing = T)),
                          numericInput('obs1', 'Number of observations (n) to view:', value = 10,
                                       min = 1, max = nrow(df))
                        ),
                        mainPanel(
                          tableOutput('tablemost')
                        ))),
             tabPanel("Question 4",
                      pageWithSidebar(
                        headerPanel('Which departments earned the most?'),
                        sidebarPanel(
                          selectInput('yr', 'Year',
                                      sort(levels(as.factor(df$Year)),
                                           decreasing = T)),
                          numericInput('obs2', 'Number of observations (n) to view:', 5,
                                       min = 1, max = nrow(df)),
                          selectInput('meth', 'Choose Method:', c("Mean", "Median"))
                        ),
                        mainPanel(
                          tableOutput('depmost')
                        )))))


server <- function(input,output){
  # choose columns to display
  
data <- reactive({
  test2 <- df[df$Year %in% input$yr,] 
  print(test2)
})

data_plot2 <- reactive({
  test <- df3[df3$Year %in% input$yr,] 
  print(test)
})

output$summary <- renderTable({
sumstat =  kable(summary(data()[,3:7]))
head(sumstat)
})

 output$plot1 <- renderPlot({df %>% group_by(Year) %>% 
    summarise(`Over Pay` = sum(Overtime_Pay / 1000000, na.rm=T), 
              `Other Pay` = sum(Other_Pay / 1000000, na.rm=T), 
              `Base Pay` = sum(Base_Pay / 1000000, na.rm=T)) %>% 
    gather(type, pay, `Over Pay`:`Base Pay`, factor_key = T) %>% 
    ggplot(aes(x = Year, y = pay, fill = type)) + 
    geom_bar(stat = "identity") +
    labs(x = "Year", y = "Total Pay in Millions $", fill = "Type") }, height = 400, width = 600)
 
 output$plot2 <- renderPlot({
   ggplot(data_plot2(),aes(x= Dollars)) + geom_histogram(colour = "blue") + geom_rug() +
     facet_grid(variable ~ .)}, height = 400, width = 600)
 
 output$tablemost <- renderTable({
   head((df %>% filter(Year == as.numeric(input$yr)) %>% arrange(desc(Total_Payments)) %>% select(Department_Title, Total_Payments, Base_Pay, Overtime_Pay, Other_Pay)), n = input$obs1)
 })
 
# output$depmost <- renderTable({if(input$meth == "Mean"){
 # head((df %>% filter(Year == as.numeric(input$yr)) %>% group_by(Department_Title) %>% mutate(meanTP = mean(Total_Payments))) %>% distinct(meanTP, .keep_all = TRUE) %>% arrange(desc(meanTP))} 
  # else if(input$meth == "Median"){}
 #})
 
  }
   

shinyApp(ui, server)

