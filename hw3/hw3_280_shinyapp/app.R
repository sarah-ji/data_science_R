library(shiny)
library(ggplot2)
library(knitr)
library(dplyr)
library(tidyr)

df <- readRDS("/home/sarahh.jii/biostat-m280-2018-winter/hw3/LA_payroll.rds") 

df1 = df %>% mutate(id = row_number()) %>%
  select(names(df)[c(1)], id) %>% gather(variable, value, -id)
df2 = df %>% mutate(id = row_number()) %>%
  select(names(df)[c(3:7)], id) %>% gather(variable, value, -id)
df3 = left_join(df1, df2, by = "id")
names(df3) = c("id", "y", "Year","variable","Dollars")

df_exceed = df %>% filter(Total_Payments > Projected_Annual_Salary)

ui <- fluidPage(
  titlePanel(title=h4("Payroll", align="center")),
  
  navbarPage("LA Payroll",
             tabPanel("Question 1.2",
                      pageWithSidebar(
                        headerPanel('Total LA Payroll by Year'),
                        sidebarPanel(
                          selectInput('type_pay', 'Type of Payment:', 
                                      choices = names(df)[3:7]),
                          selectInput('yr1', 'Select Year:',
                                      sort(levels(as.factor(df$Year)),
                                           decreasing = T))
                        ),
                        mainPanel("Summary Statistics By Selected Year",
                                  fluidRow(
                          tableOutput('summary'), plotOutput("plot1"), splitLayout(plotOutput("plot2"), plotOutput("plot3")
                        ))))),
             tabPanel("Question 1.3",
                      pageWithSidebar(
                        headerPanel('Who earned the most?'),
                        sidebarPanel(
                          selectInput('yr2', 'Select Year:',
                                      sort(levels(as.factor(df$Year)),
                                           decreasing = T)),
                          numericInput('obs1', 'Top n earnings to view:', value = 10,
                                       min = 1, max = nrow(df))
                        ),
                        mainPanel(
                          tableOutput('tablemost')
                        ))),
             tabPanel("Question 1.4",
                      pageWithSidebar(
                        headerPanel('Which departments earned the most by Median or Mode?'),
                        sidebarPanel(
                          selectInput('yr3', 'Select Year:',
                                      sort(levels(as.factor(df$Year)),
                                           decreasing = T)),
                          numericInput(inputId ='obs2', label = 'Top n earning departments to view:', value = 5,
                                       min = 1, max = nrow(df)),
                          selectInput('meth', 'Choose Method:', choices = c("Median", "Mean"))
                        ),
                        mainPanel(
                          tableOutput('depmost')
                        ))),
             tabPanel("Question 1.5",
                      pageWithSidebar(
                        headerPanel('Which departments cost the most?'),
                        sidebarPanel(
                          selectInput('yr4', 'Select Year:',
                                      sort(levels(as.factor(df$Year)),
                                        decreasing = T)),
                          numericInput(inputId = 'obs3', label = 'Top n costly departments to view:',
                                       value = 5, min = 1, max = nrow(df))
                          ),
                        mainPanel(
                          tableOutput('depcostmost'))
                      )),
             tabPanel("Question 1.6",
                      pageWithSidebar(
                        headerPanel('Which individuals are paid more than their projected annual salary?'),
                        sidebarPanel(
                          selectInput('yr5', 'Select Year:',
                                      sort(levels(as.factor(
                                        df$Year)),
                                        decreasing = T)),
                          selectInput(inputId = 'dept', label = 'Select Department:',
                                      selected = 'Fire',
                                     sort(levels(as.factor(df_exceed$Department_Title)),
                                          decreasing = T)),
                          numericInput(inputId = 'obs4', label = 'Top n happy people earning more than expected:',
                                       value = 3, min = 1,
                                       max = nrow(df))),
                        mainPanel(
                          tableOutput('aboveprojected'))
             ))
))

server <- function(input,output){
  # choose columns to display

data_plot2 <- reactive({
  test <- df3[df3$Year %in% input$yr1,] 
  print(test)
})

df_ui_year_most <- reactive({
  test2 <- df[df$Year %in% input$yr2, ] %>% arrange(desc(Total_Payments)) %>% select(Department_Title, Total_Payments, Base_Pay, Overtime_Pay, Other_Pay)
  print(test2)
})

df_ui_year_depmost <- reactive({if (input$meth == "Mean"){
  test3 <- df[df$Year %in% input$yr3, ] %>% group_by(`Department Title` = Department_Title) %>%
            summarise(`Mean Total Payments` = mean(Total_Payments, na.rm = T),
                      `Mean Base Payments` = mean(Base_Pay, na.rm = T),
                      `Mean Overtime Payments` = mean(Overtime_Pay, na.rm = T),
                      `Mean Other Payments` = mean(Other_Pay, na.rm = T)) %>%
            arrange(desc(`Mean Total Payments`))
}
  else if (input$meth == "Median"){
    test3 <- df[df$Year %in% input$yr3, ] %>% group_by(`Department Title` = Department_Title) %>%
            summarise(`Median Total Payments` = median(Total_Payments),
                      `Median Base Payments` = median(Base_Pay),
                      `Median Overtime Payments` = median(Overtime_Pay),
                      `Median Other Payments` = median(Other_Pay)) %>%
            arrange(desc(`Median Total Payments`))
  }
  print(test3)
})

df_depcostmost <- reactive({
  test4 <- df[df$Year %in% input$yr4, ] %>% group_by(`Department Title` = Department_Title) %>%
                     summarise(`Total Yearly Cost` = sum(Total_Payments, na.rm=T),
                               `Yearly Base Cost` = sum(Base_Pay, na.rm=T),
                               `Yearly Overtime Cost` = sum(Overtime_Pay, na.rm=T),
                               `Yearly Other Cost` = sum(Other_Pay, na.rm=T)) %>%
                     arrange(desc(`Total Yearly Cost`))
  print(test4)
})

df_exceeded <- reactive({
  test5 <- df_exceed[df_exceed$Year %in% input$yr5, ] %>% filter(Department_Title == input$dept) %>% mutate(`Amount Exceeding Projected Salary` = Total_Payments - Projected_Annual_Salary) %>% arrange(desc(`Amount Exceeding Projected Salary`))
  print(test5)  
})

output$summary <- renderTable({
sumstat =  kable(summary(df[df$Year %in% input$yr1, 3:7]))
head(sumstat)
})

 output$plot1 <- renderPlot({df %>% group_by(Year) %>% 
    summarise(`Over Pay` = sum(Overtime_Pay / 1000000, na.rm=T), 
              `Other Pay` = sum(Other_Pay / 1000000, na.rm=T), 
              `Base Pay` = sum(Base_Pay / 1000000, na.rm=T)) %>% 
    gather(type, pay, `Over Pay`:`Base Pay`, factor_key = T) %>% 
    ggplot(aes(x = Year, y = pay, fill = type)) + 
    geom_bar(stat = "identity") +
    labs(x = "Year", y = "Total Pay in Millions $", fill = "Type", title = "Payroll by Year and Type") }, height = 400, width = 600)
 
 output$plot2 <- renderPlot({
   ggplot(data_plot2() ,aes(x= Dollars)) + geom_histogram(colour = "blue") + geom_rug() +
     facet_grid(variable ~ .) + labs(title = "Distribution of Payroll by Type and Selected Year")}, height = 400, width = 600)
 
 output$plot3 <-renderPlot({
   hist(as.numeric(unlist(df[df$Year == input$yr1, input$type_pay])),
        col = "lightblue", border = 'white',
        main = input$yr1,
        xlab = "Year",
        ylab = "Pay")
 })
 
 output$tablemost <- renderTable({
   head(df_ui_year_most() , n = input$obs1)
 })
 
 output$depmost = renderTable({
   head(df_ui_year_depmost(), n = input$obs2)
 })
 
 output$depcostmost = renderTable({
   head(df_depcostmost(), n = input$obs3)
 })
 
 output$aboveprojected = renderTable({
   head(df_exceeded(), n = input$obs4)
 })

  }
   

shinyApp(ui, server)

