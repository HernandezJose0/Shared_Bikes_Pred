# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)
# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction.R")


test_weather_data_generation<-function(){
  #Test generate_city_weather_bike_data() function
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

# Create a RShiny server
shinyServer(function(input, output){
  # Define a city list
  
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  radius_levels <- colorFactor(c(6, 10, 12), 
                              levels = c("small", "medium", "large"))
  city_weather_bike_df <- test_weather_data_generation()
  # Create another data frame called `cities_max_bike` with each row contains city location info and max bike
  # prediction for the city
  cities_max_bike<-city_weather_bike_df %>% group_by(CITY_ASCII) %>% 
    filter(BIKE_PREDICTION==max(BIKE_PREDICTION))
  city_weather_bike_df<-city_weather_bike_df%>% mutate(FORECASTDATETIME=as.POSIXct(FORECASTDATETIME))%>%
    mutate(BIKE_PREDICTION=as.integer(BIKE_PREDICTION))
  # Observe drop-down even
  # Then render output plots with an id defined in ui.R
  # If just one specific city was selected, then render a leaflet map with one marker
  # on the map and a popup with DETAILED_LABEL displayed
  observeEvent(input$city_dropdown, {
    if(input$city_dropdown != 'All') {
      city_df<-city_weather_bike_df%>%filter(CITY_ASCII==input$city_dropdown)
      #Render the city overview map
      output$city_bike_map <- renderLeaflet({
        # Complete this function to render a leaflet map
        leaflet(data=cities_max_bike%>%filter(CITY_ASCII==input$city_dropdown))%>%
          addTiles()%>%
          addPopups(~LNG,~LAT,~DETAILED_LABEL)%>%
          addMarkers(~LNG,~LAT)
        
      })
      output$temp_line<-renderPlot({
        ggplot(city_df,
               aes(y=TEMPERATURE,x=1:length(TEMPERATURE)))+geom_line(color="green")+
          geom_point(color="yellow")+ geom_text(aes(label=TEMPERATURE))+
          labs(x="Time (3 hours ahead)",y="Temperature (°C)",title="Temperature chart")
      })
      output$bike_line<-renderPlot({
        ggplot(city_df,
               aes(x=FORECASTDATETIME, y=BIKE_PREDICTION))+
          geom_point()+geom_text(aes(label=BIKE_PREDICTION))+geom_line(color="red")+
          labs(x="Time (3 hours ahead)",y="Predicted Bike Count")
      })
      output$bike_date_output <- renderText({
        paste("Time: ",as.Date.POSIXct(input$plot_click$x),format(as.POSIXct.Date(input$plot_click$x), format = "%H:%M:%S"),"\nBike Count Prediction: ",as.integer(input$plot_click$y))
      })
      output$humidity_pred_chart<-renderPlot({
        ggplot(city_df, aes(x=HUMIDITY, y= BIKE_PREDICTION))+geom_point()+
          geom_smooth(method = "lm", formula =  y ~ poly(x, 6))
      })
    }else{
      output$city_bike_map <- renderLeaflet({
        # Complete this function to render a leaflet map
        leaflet(data=cities_max_bike)%>%addTiles()%>%
          addPopups(~LNG,~LAT,~LABEL)%>%
          addCircleMarkers(radius=~ifelse(BIKE_PREDICTION_LEVEL=="small", 6,
                                          ifelse(BIKE_PREDICTION_LEVEL=="medium",
                                                 10,12)),
                           color=~color_levels(BIKE_PREDICTION_LEVEL))
        
      })
    }

  })
})
