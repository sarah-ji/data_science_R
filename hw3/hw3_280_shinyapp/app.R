library(shiny)
library(ggplot2)
library(knitr)
library(dplyr)
library(tidyr)
library(rsconnect)
#rsconnect::deployApp('~/biostat-m280-2018-winter/hw3/hw3_280_shinyapp')
#setwd("~/biostat-m280-2018-winter/hw3/hw3_280_shinyapp")
df <- readRDS("LA_payroll.rds") 

df1 = df %>% mutate(id = row_number()) %>%
  select(names(df)[c(1)], id) %>% gather(variable, value, -id)
df2 = df %>% mutate(id = row_number()) %>%
  select(names(df)[c(4:8)], id) %>% gather(variable, value, -id)
df3 = left_join(df1, df2, by = "id")
names(df3) = c("id", "y", "Year","variable","Dollars")

df_exceed = df %>% filter(Total_Payments > Projected_Salary)

ui <- fluidPage(
  titlePanel(title=h4("Payroll", align="center")),
  
  navbarPage("LA Payroll",
    tabPanel("Question 1.2",
      pageWithSidebar(
        headerPanel('Total LA Payroll by Year'),
          sidebarPanel(
          selectInput('type_pay', 'Type of Payment:', choices = names(df)[4:8]),
          selectInput('yr1', 'Select Year:',
            sort(levels(as.factor(df$Year)), decreasing = T)),
            fluidRow("Summary Statistics By Selected Year",
            tableOutput('summary'), width = "65")),
              mainPanel("Visualize the LA Payroll for your Favorite Year",
fluidRow(plotOutput("plot2"), 
         splitLayout(plotOutput("plot13"), plotOutput("plot3", width = "80%"), 
                     cellWidths = c("40%", "60%")))))),
    tabPanel("Question 1.3",
      pageWithSidebar(
        headerPanel('Who earned the most?'),
          sidebarPanel(
          selectInput('yr2', 'Select Year:', sort(levels(as.factor(df$Year)),
                                           decreasing = T)),
          numericInput('obs1', 'Top n earnings to view:', value = 10,
                                       min = 1, max = nrow(df))),
             mainPanel(tableOutput('tablemost'), plotOutput("plotmost")))),
    tabPanel("Question 1.4",
      pageWithSidebar(
        headerPanel('Which departments earned the most by Median or Mean?'),
          sidebarPanel(
          selectInput('yr3', 'Select Year:', sort(levels(as.factor(df$Year)),
                                           decreasing = T)),
          numericInput(inputId ='obs2', 
label = 'Top n earning departments:', value = 5, min = 1, max = nrow(df)),
          selectInput('meth', 'Choose Method:', choices = c("Median", "Mean"))),
            mainPanel(tableOutput('depmost'), plotOutput("plotdepmostmed")))),
      tabPanel("Question 1.5",
        pageWithSidebar(
          headerPanel('Which departments cost the most?'),
            sidebarPanel(
            selectInput('yr4', 'Select Year:', sort(levels(as.factor(df$Year)),
                                        decreasing = T)),
            numericInput(inputId = 'obs3', label = 'Top n costly departments:',
                                       value = 5, min = 1, max = nrow(df))),
            mainPanel(tableOutput('depcostmost'), plotOutput('plotdepcost')))),
      tabPanel("Question 1.6",
        pageWithSidebar(
        headerPanel('Which individuals are paid more than expected?'),
            sidebarPanel(
            selectInput('yr5', 'Select Year:', 
                        sort(levels(as.factor(df$Year)), decreasing = T)),
            numericInput(inputId = 'obs4', 
                        label = 'Top n individuals earning more than expected:',
                                       value = 5, min = 1,
                                       max = nrow(df))),
            mainPanel(tableOutput('aboveprojected'), plotOutput("plot69"))))
             
))

server <- function(input,output){
  # choose columns to display

data_plot2 <- reactive({
  test <- df3[df3$Year %in% input$yr1,] 
  print(test)
})

df_ui_year_most <- reactive({
  test2 <- df[df$Year %in% input$yr2, ] %>%
    arrange(desc(Total_Payments)) %>% 
    select(Department_Title, Job_Title, Total_Payments, Base_Pay, 
           Overtime_Pay, Other_Pay)
  print(test2)
})

df_ui_year_depmost <- reactive({if (input$meth == "Mean"){
  test3 <- df[df$Year %in% input$yr3, ] %>% 
    group_by(`Department Title` = Department_Title) %>%
      summarise(`Mean Total Payments` = mean(Total_Payments, na.rm = T),
                `Mean Base Payments` = mean(Base_Pay, na.rm = T),
                `Mean Overtime Payments` = mean(Overtime_Pay, na.rm = T),
                `Mean Other Payments` = mean(Other_Pay, na.rm = T)) %>%
        arrange(desc(`Mean Total Payments`))
}
  else if (input$meth == "Median"){
    test3 <- df[df$Year %in% input$yr3, ] %>% 
      group_by(`Department Title` = Department_Title) %>%
        summarise(`Median Total Payments` = median(Total_Payments, na.rm = T),
                  `Median Base Payments` = median(Base_Pay, na.rm = T),
                  `Median Overtime Payments` = median(Overtime_Pay, na.rm = T),
                  `Median Other Payments` = median(Other_Pay, na.rm = T)) %>%
          arrange(desc(`Median Total Payments`))
  }
  print(test3)
})

df_depcostmost <- reactive({
  test4 <- df[df$Year %in% input$yr4, ] %>% 
    group_by(`Department Title` = Department_Title) %>%
      summarise(`Total Yearly Cost` = sum(Total_Payments, na.rm=T),
                `Yearly Base Cost` = sum(Base_Pay, na.rm=T),
                `Yearly Overtime Cost` = sum(Overtime_Pay, na.rm=T),
                 `Yearly Other Cost` = sum(Other_Pay, na.rm=T)) %>%
        arrange(desc(`Total Yearly Cost`))
  print(test4)
})

df_exceeded <- reactive({
  test5 <- df_exceed[df_exceed$Year %in% input$yr5, ] %>%
    group_by(Department_Title) %>% 
mutate(`$ Exceeding Projected` = Total_Payments - Projected_Salary) %>%
    arrange(desc(`$ Exceeding Projected`)) %>% 
select(Department_Title, Projected_Salary, Total_Payments, Base_Pay,
       Overtime_Pay, Other_Pay, `$ Exceeding Projected`)
  print(test5)  
})


output$summary <- renderTable({
sumstat =  kable(summary(df[df$Year %in% input$yr1, 4:7]))
head(sumstat)
})
 
 output$plot2 <-renderPlot({
   hist(as.numeric(unlist(df[df$Year == input$yr1, input$type_pay])),
        col = "lightblue", border = 'white',
        main = input$yr1,
        xlab = "Pay",
        ylab = "Count")
 })
 
 output$plot3 <- renderPlot({
   ggplot(data_plot2() ,aes(x= Dollars)) + geom_histogram(colour = "blue") + 
     geom_rug() +
     facet_grid(variable ~ .) + 
     labs(title = "Distribution of Payroll by Type and Selected Year")},
   height = 400, width = 600)
 
 output$plot13 <- renderPlot({
   df %>% filter(Year == as.numeric(input$yr1)) %>%
     select(`Total Pay` = Total_Payments, `Base Pay` = Base_Pay,
            `Overtime Pay` = Overtime_Pay, `Other Pay` = Other_Pay) %>%
     mutate_all(., funs(. / 1000)) %>%
     gather(Type, pay, `Total Pay`:`Other Pay`, factor_key = T) %>%
     ggplot(mapping = aes(x = pay, colour = Type)) +
     geom_freqpoly() + coord_cartesian(xlim = c(0, 300)) +
     labs(title = input$yr0, x = "Payroll in Thousands of Dollars $", y = "Count",
          colour = "Type")
 })
 
 output$tablemost <- renderTable({
   head(df_ui_year_most() , n = input$obs1)
 })
 
 output$plotmost <-renderPlot({
   hist(as.numeric(unlist(df_ui_year_most()[, 2])),
        breaks = 50,
        col = "lightblue", border = 'white',
        main = input$yr2,
        xlab = "Total Yearly Cost")
 })
 
 output$depmost = renderTable({
   head(df_ui_year_depmost(), n = input$obs2)
 })

 output$plotdepmostmed <-renderPlot({if(input$meth == "Median"){
   hist(as.numeric(unlist(df_ui_year_depmost()[, 2])),
        breaks = 50,
        col = "lightblue", border = 'white',
        main = c(input$yr3, input$meth),
        xlab = "Total Payments")}
   else if (input$meth == "Mean"){ 
     hist(as.numeric(unlist(df_ui_year_depmost()[, 2])),
          breaks = 50,
          col = "lavender", border = 'white',
          main = c(input$yr3, input$meth),
          xlab = "Total Payments")}
 })
 
 output$depcostmost = renderTable({
   head(df_depcostmost(), n = input$obs3)
 })
 
 output$plotdepcost <-renderPlot({
   hist(as.numeric(unlist(df_depcostmost()[,2])),
        breaks = 100,
        col = "lightblue", border = 'white',
        main = input$yr4,
        xlab = "Total Yearly Cost")
 })
 
 
 output$aboveprojected = renderTable({
   head(df_exceeded()[,c(1:3,7)], n = input$obs4)
 })
 
 output$plot69 <-renderPlot({
   hist(as.numeric(unlist(df_exceeded()[,7])),
        breaks = 50,
        col = "lightblue", border = 'white',
        main = input$yr5,
        xlab = "Total Payments - Projected Annual Salary")
 })

 output$depbenefit = renderTable({
   head(df_depbenefit(), n = input$obs5)
 })
 
 
 output$plot5 <-renderPlot({
   hist(as.numeric(unlist(df_depbenefit()[1:input$obs5, 2])),
        breaks = 50,
        col = "lightblue", border = 'white',
        main = input$yr6,
        xlab = "Average Benefit Cost")
 })
 
  }
   

shinyApp(ui, server)

