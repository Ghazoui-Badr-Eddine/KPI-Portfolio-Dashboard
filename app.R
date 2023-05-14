library(geojsonR)
library(geojsonio)
library(geojson)
library(leaflet)
library(shiny)
library(sp)
library(shinydashboard)
library(sf)
library(RColorBrewer)
library(stringr)
library(widgetframe)
library(dplyr)
library(rgdal)
library(ggplot2)
library(ggthemes)
library(RSQL)
library(gsubfn)
library(proto)
library(RSQLite)
library(sqldf)
library(lattice) 
require(tidyverse)
require(plotly)
library(Hmisc)
library(shinyWidgets)





regions<-geojsonio::geojson_read("file/location", what="sp")   # Use the path to the map data file
dataframe <- read.table("file/location",TRUE,";")                # Use the path to your dataset ( a csv file, separated by a semicolon ';')
citiesframe <- read.table("file/location",TRUE,";")                      # Use the path to the Cities data file
df <- merge(dataframe,citiesframe,by="VILLES")                                                # This line of code merges the cities data with your dataset, should be ran before the next line
for(i in seq(1:nrow(citiesframe))){
  citiesframe$Coords[i]<-st_sfc(st_point(c(citiesframe$LONGITUDE[i],citiesframe$LATITUDE[i])), crs = 4326)      # This loop converts the lat and lon data from integers to spatial point so we can use them to display the markers on map
} 

# These instructions changes the content of some dataset's columns as it was requested
levels(df$CODE_PRODUIT_POLICE) <- c(levels(df$CODE_PRODUIT_POLICE), "AAP")
df$CODE_PRODUIT_POLICE[df$CODE_PRODUIT_POLICE == 'ZA'] <- 'AAP'

levels(df$CODE_PRODUIT_POLICE) <- c(levels(df$CODE_PRODUIT_POLICE), "AUTRE")
df$CODE_PRODUIT_POLICE[df$CODE_PRODUIT_POLICE %in% c('F1','CD','AB','CA','FGT','HM')] <- 'AUTRE'

levels(df$CODE_PRODUIT_POLICE) <- c(levels(df$CODE_PRODUIT_POLICE), "AAF")
df$CODE_PRODUIT_POLICE[df$CODE_PRODUIT_POLICE == 'PP'] <- 'AAF'

levels(df$CODE_PRODUIT_POLICE) <- c(levels(df$CODE_PRODUIT_POLICE), "AAC")
df$CODE_PRODUIT_POLICE[df$CODE_PRODUIT_POLICE == 'ZAC'] <- 'AAC'

levels(df$CODE_USAGE) <- c(levels(df$CODE_USAGE), "AUTRE")
df$CODE_USAGE[df$CODE_USAGE %in% c('D10','D7','D13','D14','D2','D3','D6','D8','D5','D11','D4','D1-EH','D1-EE','')] <- 'AUTRE'

# These lines converts some columns of the frame from FACTOR type to numeric, we need this conversion of type to be able to calculate the KPI's values
df[["GEP.GLOBAL"]] <- as.numeric(sub(",", ".",  df[["GEP.GLOBAL"]] , fixed = TRUE))
df[["CHARGE.GLOBAL"]] <- as.numeric(sub(",", ".",  df[["CHARGE.GLOBAL"]] , fixed = TRUE))
df[["JOUR.GLOBAL"]] <- as.numeric(sub(",", ".",  df[["JOUR.GLOBAL"]] , fixed = TRUE))
df[["NB.SIN.GLOBAL"]] <- as.numeric(sub(",", ".",  df[["NB.SIN.GLOBAL"]] , fixed = TRUE))

# This fucntion represents the user interface, it contains any component that will be displayed in the interface and represent every component's output to call it back from the srever side function.
ui <- fluidPage(
  #background color of the dashboard body :
  setBackgroundColor("#dee5ed"),   
  # This tags instructions let us change the design of some components using CSS, we use this methode only if the design can't be fixed using SHINY
  tags$head(tags$style(HTML( ".control-label h4 { color:white; font-weight: bold; display: inline}"))),
  tags$head(tags$style(HTML("div.info.legend.leaflet-control br {clear: both;}"))),
  tags$head(tags$style(HTML(".shiny-split-layout>div {overflow-x: hidden; }  .shiny-split-layout>div {overflow-y: hidden; }"))),
  tags$style(type = "text/css", "#AZmap {height: 801px !important; background-color:#B8B8B8;}"), 
  tags$style(type = "text/css", "#stack_plot_filter {color: black !important;;}"),
  # The fluid row that represents the header split to column's different in with, it contains the filters used in our app
  fluidRow(style = "background-color:#0073b7;",
           column(2,
                  # NOTICE THIS !!!!! in order to display the image properly run the App by clicking Run App in Rstudio not by selecting the code.
                  img(src="allianz.png", align = "center",width = "100%", height= "100%") 
           ),
           column(3,
                  selectInput("filter_KPI",label= h4("KPI : "),
                              choices = c("CHIFFRE D'AFFAIRE" = "CHIFFREDAFFAIRE", "LOSS RATIO" = "LOSSRATIO", "FREQUENCE" = "FREQUENCE"),
                              multiple = FALSE
                  )),
           column(2,
                  selectInput("filter_Product",label= h4("Produit : "),
                              choices = unique(df[3]),
                              selected = "Produit",multiple = TRUE
                  )),
           column(2,
                  selectInput("filter_Usage",label= h4("Usage : "),
                              choices = unique(df[4]),
                              selected = "Usage",multiple = TRUE
                  )),
           column(2,
                  selectInput("filter_equiper",label= h4("Equipement : "),
                              choices = unique(df[5]),
                              selected = "Equipement",multiple = TRUE
                  )),
           column(1)
  ),
  
  HTML('</br></br>'),
  useShinydashboard(),  # This function let us use the Shiny dashboard components in our shiny fluid page such as ( box function )
  splitLayout( cellWidths = c("50%", "50%"),
               verticalLayout(
                 # The box where the map is going to be displayed, it contains other components needed to control the map and its content
                 box(leafletOutput(outputId = "AZmap" ), 
                     absolutePanel( top = 200, left = 20,draggable = FALSE,
                                    checkboxInput("legend", "Show legend", TRUE),
                                    radioButtons("year",label = h4("YEAR :"),choices = list("2001" = 2001, "2004" = 2004, "2008" = 2008, "2013" = 2013 ),selected = 2001)),
                     background = "blue",solidHeader = TRUE,title = textOutput("maptitle"),   width = 12, height = 900 )
               ),
               
               verticalLayout(
                 # The box where the bar plot is going to be displayed
                 box(plotlyOutput(outputId = "AZbarplot", height = 220), background = "blue",title = textOutput("barplottitle") ,  width = 12, height = 440),
                 # The box where the stack plot is going to be displayed 
                 box(plotlyOutput(outputId = "AZstackplot", height = 220),background = "blue",title = textOutput("stackplottitle") ,absolutePanel( top = 50 ,radioButtons("stack_plot_filter",
                                                                                                                                                                          label = NULL,inline = TRUE ,choices = list("Produit"="CODE_PRODUIT_POLICE","Usage"="CODE_USAGE","Equipement"="equiper"),selected="CODE_PRODUIT_POLICE")), width = 12, height = 440)
               )
  ),
  # The box where the table plot is going to be displayed
  box(tableOutput(outputId = "AZtable" ),background = "blue",solidHeader = TRUE,title = textOutput("tabletitle"),   width = 12, height = 270 )
)

server <- function(input, output, session) {
  # The function isShapeClicked() is developed to get the click's on the map lat and lng value, groups them as a POINT like this : POINT(lat lng)  
  # with the function st_point, then it uses the function st_sfc to change this point clicked to a  sf coordinates data
  # after that we use as_Spatial function to to turn our sf click to a spatial data so we can be able to compare it with 
  # our geojson data that contains the polygons data using a method called over() that needs the data to be spatial to check if 
  # our clicked lies within the specified polygon
  is_Shape_Clicked <- eventReactive(input$AZmap_shape_click,{
    proxy <- leafletProxy("AZmap", data=regions)
    shape_click_lon <- c(input$AZmap_shape_click$lng)
    shape_click_lat <- c(input$AZmap_shape_click$lat)
    shape_click <- st_sfc(st_point(c(shape_click_lon,shape_click_lat)), crs = 4326)
    Click_Spatial <-as_Spatial(shape_click)
    
    Polygon_Spatial <- SpatialPolygons(regions@polygons, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    in_Shape <- over(Click_Spatial,Polygon_Spatial)
    print(shape_click)
    return(in_Shape)
  })
  
  Product_data <- reactive({ # This function is used to group the data based on product selection 
    if(is.null(input$filter_Product)  ){
      product_frame <- df
    }else{
      product_frame <- df %>% group_by(CODE_PRODUIT_POLICE) %>% filter( input$filter_Product == CODE_PRODUIT_POLICE  )
    }
    return( product_frame)
  })
  Usage_data <- reactive({ # This function is used to group the data based on usage selection 
    if(is.null(input$filter_Usage)  ){
      usage_frame <- df
    }else{
      usage_frame <- df %>% group_by(CODE_USAGE) %>% filter( input$filter_Usage == CODE_USAGE  )
    }
    return(usage_frame)
  })
  Equiper_data <- reactive({ # This function is used to group the data based on equipment selection 
    if(is.null(input$filter_equiper)  ){
      equiper_frame <- df
    }else{
      equiper_frame <- df %>% group_by(equiper) %>% filter( input$filter_equiper == equiper )
    }
    return(equiper_frame)
  })
  Group_by_Region <- reactive({ # This function is used to group the data based on the clicked region selection 
    if(is.null(input$AZmap_shape_click)){
      frame_region <- df%>% group_by(REGION) %>% filter(REGION == "Casablanca-Settat")
    }else{
      frame_region <- df%>% group_by(REGION) %>% filter(REGION == regions$Name[is_Shape_Clicked()])
    }
    return(frame_region)
  })
  Join_filters <- reactive({ # This function is used to join all selected data from the frames above to be displayed in the charts 
    combine_data_frame <- Reduce(intersect , list(Group_by_Region(), Product_data(),Usage_data(),Equiper_data()))    
    return(combine_data_frame)
  })
  temp_df <- reactive({ # Returns a frame that combines the filter selections other than the KPI filter, will be used in the function  map_data()
    temp_df <- Reduce(intersect , list( Product_data(),Usage_data(),Equiper_data()))
    return(temp_df)
  })
  map_data <- reactive({ # This function calculates the KPI's based on user selections of the filters, then returns them in a frame used to display this data on the map 
    map_data_result <- aggregate(list(CHIFFREDAFFAIRE=temp_df()$GEP.GLOBAL,CHARGEGLOBAL=temp_df()$CHARGE.GLOBAL,NB.SIN.GLOBAL=temp_df()$NB.SIN.GLOBAL,JOUR.GLOBAL=temp_df()$JOUR.GLOBAL),by = list(REGION=temp_df()$REGION,earnannee=temp_df()$earnannee),FUN= sum)
    map_data_result$LOSSRATIO <- map_data_result$CHARGEGLOBAL / map_data_result$CHIFFREDAFFAIRE
    map_data_result$FREQUENCE <- map_data_result$NB.SIN.GLOBAL / (map_data_result$JOUR.GLOBAL/365)
    map_data_result <- map_data_result %>% filter(input$year == map_data_result$earnannee)
    index <- which(!(regions$Name %in% map_data_result$REGION))
    map_data_result<- map_data_result %>% add_row(REGION = regions$Name[index])
    map_data_result<-  map_data_result[order(map_data_result$REGION),]
    return(map_data_result)
  })
  
  colorpal <- reactive({ # This functon sets up the color for the map and the legend
    colorpal_switch <- switch(input$filter_KPI, "CHIFFREDAFFAIRE" = "RdYlGn", "LOSSRATIO"= "YlGnBu", "FREQUENCE"= "YlGnBu")
    colorNumeric(colorpal_switch, map_data()[[input$filter_KPI]])
  })
  map_title <- reactive({
    # This sets up the title of the map
    if(is.null(input$AZmap_shape_click)){
      paste("Map Geospatial de ",kpi_title()," pour l'année ",input$year," dans la région de: Casablanca-Settat")
      
    }else{
      paste("Map Geospatial de ",kpi_title()," pour l'année ",input$year," dans la région de:", regions$Name[is_Shape_Clicked()])
    }
  })
  map_unit <- reactive({ # This sets up units to display on legend and graphes
    if ( input$filter_KPI == "LOSSRATIO" | input$filter_KPI == "FREQUENCE"){
      unit <- "%" 
    }else{
      unit <-  ""
    }
    return(unit)
  })
  legend  <- eventReactive(c(input$filter_KPI,input$year,input$legend,input$filter_Product,input$filter_Usage,input$filter_equiper),{
    # This function sets up the legend on map with the needed colors and units
    proxy <- leafletProxy("AZmap", data = regions)
    
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",opacity =1,labFormat = labelFormat(suffix = map_unit()),title = paste("<Strong>",input$filter_KPI,"<Strong>"," in ",map_unit()),
                          pal = pal, values = ~map_data()[[input$filter_KPI]]
      )
    }
    if (input$legend == FALSE){
      proxy %>% clearControls()
    }
  })
  azmap <- eventReactive(c(input$filter_KPI,input$year,input$legend, input$filter_Product, input$filter_Usage ,input$filter_equiper ), {
    pal <- colorpal()
    lab <- paste("<Strong>", regions$Name,"</Strong>","<br/><br/>",  kpi_title() ,":",  map_data()[[input$filter_KPI]]) %>% lapply(htmltools::HTML) # The label shown on toolkit when highlighting the map's polygons
    # This should call our leaflet map object with proxy so we can access the leaflet functions in this function
    proxy <- leafletProxy("AZmap", data=regions)
    proxy %>%  
      clearControls()%>%
      setView(-8, 29, 6)%>%
      addPolygons(
        fillColor = ~pal(map_data()[[input$filter_KPI]]),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "1",
        fillOpacity = 50,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 50,
          bringToFront = TRUE),
        label = lab,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>%
      addMarkers(~citiesframe$LONGITUDE,~citiesframe$LATITUDE,
                 clusterOptions = markerClusterOptions(),
                 label = ~citiesframe$VILLES
      )
  })
  
  bar_plot_data <- reactive({ # This function is used calculate the data of a bar plot based on the frame returned from join_filter() funtion
    groupbyC_result <- aggregate(list(CHIFFREDAFFAIRE=Join_filters()$GEP.GLOBAL,CHARGEGLOBAL=Join_filters()$CHARGE.GLOBAL,NB.SIN.GLOBAL=Join_filters()$NB.SIN.GLOBAL,JOUR.GLOBAL=Join_filters()$JOUR.GLOBAL),by = list(VILLES=Join_filters()$VILLES,earnannee=Join_filters()$earnannee,REGION=Join_filters()$REGION),FUN= sum )
    groupbyC_result$LOSSRATIO <- groupbyC_result$CHARGEGLOBAL / groupbyC_result$CHIFFREDAFFAIRE
    groupbyC_result$FREQUENCE <- groupbyC_result$NB.SIN.GLOBAL / (groupbyC_result$JOUR.GLOBAL/365)
    return(groupbyC_result)
  })
  stack_plot_data <- reactive({# This function is used calculate the data of a stack plot based on the frame returned from join_filter() funtion
    stack_plot_result <- aggregate(list(CHIFFREDAFFAIRE=Join_filters()$GEP.GLOBAL),by = list(earnannee=Join_filters()$earnannee,col_filter=Join_filters()[[input$stack_plot_filter]]),FUN= sum)
    # This line is used to sum the GWP data to divide the GWP of each selected filter by the calculated sum, so we can display the GWP as pourcentage on a stack plot
    sum_gwp <- aggregate(list(CHIFFREDAFFAIRE=stack_plot_result$CHIFFREDAFFAIRE), by= list(earnanne=stack_plot_result$earnanne), FUN=sum)
    stack_plot_result$CHIFFREDAFFAIRE <- stack_plot_result$CHIFFREDAFFAIRE / sum_gwp$CHIFFREDAFFAIRE
    colnames(stack_plot_result)[colnames(stack_plot_result) == "col_filter"] <- input$stack_plot_filter 
    return(stack_plot_result)
  })
  bar_plot_title <- reactive({
    # This sets up the title of the bar plot
    if(is.null(input$AZmap_shape_click)){
      paste("Graphe de ",kpi_title()," par ville dans la région de: Casablanca-Settat")
      
    }else{
      paste("Graphe de ",kpi_title()," par ville dans la région de: ",regions$Name[is_Shape_Clicked()])
    }
  })
  stack_plot_title <- reactive({
    # This sets up the title of the stack plot
    title_choice <- names(which(input$stack_plot_filter == list("Produit"="CODE_PRODUIT_POLICE","Usage"="CODE_USAGE","Equipement"="equiper") ))
    if(is.null(input$AZmap_shape_click)){
      paste("Graphe de CHIFFRE D'AFFAIRE par Année groupé par ",title_choice," dans la région de: Casablanca-Settat")
      
    }else{
      paste("Graphe de CHIFFRE D'AFFAIRE par Année groupé par ",title_choice," dans la région de: ",regions$Name[is_Shape_Clicked()])
    }
  })
  barplot_reactive <- eventReactive(c(input$AZmap_shape_click,input$filter_KPI,input$filter_Product,input$filter_Usage,input$filter_equiper,input$year),{
    color_switch <- switch(input$filter_KPI, "CHIFFREDAFFAIRE" = "Blues", "LOSSRATIO" =  "YlGnBu" , "FREQUENCE" =  "PuBuGn" )
    # This function is used to display the data in a bar plot grouped by cities
    print(map_data())
    plot_ly(bar_plot_data(),x = ~as.character(bar_plot_data()[["VILLES"]]), y = ~bar_plot_data()[[input$filter_KPI]], type = 'bar', name = bar_plot_data()[["earnannee"]],height = 370,colors = color_switch,color= as.factor(bar_plot_data()[["earnannee"]]))%>%
      layout(barmode = "group", xaxis = list(title = "VILLES",tickangle = 20), yaxis = list(title = kpi_title(),exponentformat = "E",tickformat = map_unit()))
  
    })
  stackplot_reactive <- eventReactive(c(input$AZmap_shape_click,input$filter_Product,input$filter_Usage,input$filter_equiper,input$year,input$stack_plot_filter),{
    # This function is used to display the data in a stack plot grouped by year 
    print(stack_plot_data())
    plot_ly(stack_plot_data(),x = ~as.factor(stack_plot_data()[["earnannee"]]), y = ~stack_plot_data()[["CHIFFREDAFFAIRE"]], type = 'bar',color = ~stack_plot_data()[[input$stack_plot_filter]],colors = "Blues",name = stack_plot_data()[[input$stack_plot_filter]], height = 370)%>%
      layout(barmode = "stack", xaxis = list(title = "Année",tickangle = 20), yaxis = list(title = "CHIFFRE D'AFFAIRE"))
  })
  kpi_title <- reactive({
    # This function recovers the name of the selected KPI to be displayed in each title dynamically 
    kpi_title_text <- names(which(input$filter_KPI == list("CHIFFRE D'AFFAIRE" = "CHIFFREDAFFAIRE", "LOSS RATIO" = "LOSSRATIO", "FREQUENCE" = "FREQUENCE") ))
    return(kpi_title_text)
  })
  table_title <- reactive({
    # This sets up the title of the table
    if(is.null(input$AZmap_shape_click)){
      paste("Tableaux de ",kpi_title()," par année dans la région de: Casablanca-Settat")
      
    }else{
      paste("Tableaux de ",kpi_title()," par année dans la région de: ",regions$Name[is_Shape_Clicked()])
    }
  })
  table_data <- reactive({
    # This function is used calculate the data of a table based on the frame returned from join_filter() funtion
    tab <-aggregate(list(CHIFFREDAFFAIRE=Join_filters()$GEP.GLOBAL,CHARGEGLOBAL=Join_filters()$CHARGE.GLOBAL,NB.SIN.GLOBAL=Join_filters()$NB.SIN.GLOBAL,JOUR.GLOBAL=Join_filters()$JOUR.GLOBAL),by = list(REGION=Join_filters()$REGION,earnannee=Join_filters()$earnannee),FUN= sum)
    tab$LOSSRATIO <- tab$CHARGEGLOBAL / tab$CHIFFREDAFFAIRE
    tab$FREQUENCE <- tab$NB.SIN.GLOBAL / (tab$JOUR.GLOBAL/365)
    return(tab)
  })
  table_ractive<- eventReactive(c(input$AZmap_shape_click,input$filter_KPI,input$filter_Product,input$filter_Usage,input$filter_equiper,input$year),{
    # This function is used to display the data in a table grouped by year
    table_data()[c("earnannee", "CHIFFREDAFFAIRE","LOSSRATIO", "FREQUENCE")]
  })
  
  # These are the ouput funtions to display all the above in the UI
  output$AZbarplot <- renderPlotly({
    barplot_reactive()
  })
  output$AZstackplot <- renderPlotly({
    stackplot_reactive()
  })
  output$AZtable <- renderTable({
    table_ractive()
  }, width = "100%")
  output$barplottitle <- renderText({
    bar_plot_title()
  })
  output$maptitle <- renderText({
    map_title()
  })
  output$tabletitle <- renderText({
    table_title()
  })
  output$stackplottitle <- renderText({
    stack_plot_title()
  })
  output$AZmap <- renderLeaflet({
    req(azmap())
    legend()
    leaflet(regions) %>%
      setView(-8, 30, 5) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')
      ))
    
  })
}

# This function creates the APP should be ran to launch the app
shinyApp(ui, server, options = list(host = "0.0.0.0", port = 8080))
