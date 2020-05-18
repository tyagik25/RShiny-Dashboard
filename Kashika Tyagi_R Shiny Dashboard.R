
#"ALY6070 Week 5 Assignment- R Shiny Dashboard"
# author: Kashika Tyagi, Northeastern University"
# date: "5/10/2020"

library(shiny)  #for web applications
library(shinydashboard)
library(DataExplorer)
library(scales)
library(dplyr)
library(ggplot2)

data = read.csv("C:\\Users\\Kashika\\Desktop\\hotel_bookings_data.csv",stringsAsFactors = FALSE, header = TRUE)
#head(data)

header <- dashboardHeader(title = "Hotel Demand Analysis", titleWidth=350)  

library("tidyr")

data$children[is.na(data$children)] <- mean(data$children, na.rm = TRUE)

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    
  )
)

frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
  ,valueBoxOutput("value4")
  ,valueBoxOutput("value5")
)

frow2 <- fluidRow(
  
  
  box(
    title = "Average Daily Rate by Hotel Type"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("priceofroom", height = "300px")
  )
  
  ,box(
    title = "Count of bookings made in each month"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("monthlybookingscount", height = "300px")
  ) 
)

# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Hotel Booking Analysis', header, sidebar, body, skin='blue')


# create the server functions for the dashboard  
server <- function(input, output) { 
  
  #some data manipulation to derive the values on the boxes on top
  totalCancellations <- sum(data$is_canceled)
  
  totalWeekNights <- sum(data$stays_in_week_nights)
  
  totalWeekEndNights <- sum(data$stays_in_weekend_nights)
  
  totalPeopleStayed <- floor(sum(data$children))+sum(data$adults)
  
  guestFromCountry <- length(unique(data$country))
  
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(totalCancellations, format="d", big.mark=',')
      ,'Total Cancellations'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "light-blue")
  })
  
  output$value2 <- renderValueBox({
    
    valueBox(
      formatC(totalWeekNights, format="d", big.mark=',')
      ,'Total weeknight stays'
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "navy")
  })
  
  output$value3 <- renderValueBox({
    
    valueBox(
      formatC(totalWeekEndNights, format="d", big.mark=',')
      ,'Total Weekend stays'
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")
  })
  
  output$value4 <- renderValueBox({
    
    valueBox(
      formatC(totalPeopleStayed, format="d", big.mark=',')
      ,'Total people stayed'
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "red")
  })
  
  output$value4 <- renderValueBox({
    
    valueBox(
      formatC(totalPeopleStayed, format="d", big.mark=',')
      ,'Total People Stayed'
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "green")
  })
  
  output$value5 <- renderValueBox({
    
    valueBox(
      formatC(guestFromCountry, format="d", big.mark=',')
      ,'Origin of Guests'
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "red")
  })
  
  data$adr_pp <- data$adr/(data$adults+data$children)
  full_guest_data <- filter(data,data$is_canceled==0)
  room_prices <- full_guest_data %>% select(hotel,reserved_room_type, adr_pp)
  room_prices <- room_prices[order(room_prices$reserved_room_type),]
  
  output$priceofroom <- renderPlot({ 
    ggplot(room_prices, aes(x = reserved_room_type, y = adr_pp, fill = hotel)) + 
      geom_boxplot(position = position_dodge()) + 
      labs(title = "Average rental income per paid occupied room by hotel type",
           x = "Room type",
           y = "AVG ADR") + theme_classic()
  }) 
  
  
  
  # note this data use the booking confirmation and not the check ins, so this graph shows the
  # booking made for particular month and not the confirmed check ins.
  
  output$monthlybookingscount <- renderPlot({
    ggplot(data, aes(arrival_date_month, fill = hotel)) +
      geom_bar(position = position_dodge()) +
      labs(title = "Booking Status by Month",
           x = "Month",
           y = "Count") + theme_bw()
  })
  
}

shinyApp(ui, server)

