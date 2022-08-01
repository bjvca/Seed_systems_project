#to deploy this on shiny.io, in the directory of App.R, just type rsconnect::deployApp()

library(shiny)
library(leaflet)
library(sf)
library(leafpop)
library(ggplot2)
library(reshape2)

shops <- read.csv("shops_map.csv")
reviews <- read.csv("reviews_seed.csv")

shops <- merge(shops, reviews[c("general_corrected","yield_corrected","drought_resistent_corrected","disease_resistent_corrected","early_maturing_corrected","germination_corrected","score_corrected", "nr_reviews","shop_ID")],by.x="shop_ID", by.y="shop_ID")
#merge in ratings from latest data
shops$rating <- shops$score_corrected

names(shops)[names(shops) %in% c("general_corrected","yield_corrected","drought_resistent_corrected","disease_resistent_corrected","early_maturing_corrected","germination_corrected")] <- c("general","yield","drought_resistent","disease_resistent","early_maturing","germination")


dot_frame <- melt(shops[c("general","yield","drought_resistent","disease_resistent","early_maturing","germination","shop_ID","catchID")], id.vars=c("shop_ID", "catchID"))
names(dot_frame) <- c("shop_ID","catchID","attribute","score")
dot_frame$score <- (dot_frame$score - 1)/4*100

all_dot_frame <- aggregate(dot_frame,by=list(dot_frame$attribute),FUN=mean,na.rm=T)
all_dot_frame$attribute <- all_dot_frame$Group.1
all_dot_frame$Group.1 <- NULL
all_dot_frame$shop_ID <- "av"

### make a map with catchment ID coloring and pictures (not public)
pal <- colorFactor(
  palette = 'Dark2',
  domain = shops$catchID
)


ui <- fluidPage(
includeCSS("www/stars.css"),
titlePanel("Uganda Agro-input Seed Quality Advisor"),
  sidebarPanel(
     #Outputs
    uiOutput(outputId = "textR"),
    textOutput(outputId = "textR2"),
    tags$div(class = "ratings",
             tags$div(class = "empty-stars",
                      uiOutput("stars_ui")
             )
    ),


  uiOutput(outputId = "my_ui"),
  plotOutput("dot_plot",width=370, height = 150),
  radioButtons("radio_comp_select","Average ratings to compare to (in black)", c("Overall" = "all", "Catchment area" = "catch"), inline=T),
   width = 3
 ),
 mainPanel( 
  leafletOutput("mymap",width=1000, height=800)),
  fluidRow(verbatimTextOutput("map_marker_click"))
)


server <- function(input, output, session) {


  
  output$mymap <- renderLeaflet({
  leaflet() %>% setView(lat = 0.65, lng = 33.62, zoom=11)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=shops, layerId=~shop_ID ,  lng=~maize.owner.agree._gps_longitude, lat=~maize.owner.agree._gps_latitude,radius= 8, color=~pal(catchID), popup = ~as.character(catchID), group = "X_uuid")   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 
  })
  
  observeEvent(input$mymap_marker_click, { 
    input$mymap_marker_click  # typo was on this line
  })
  
   output$textR<-renderText({
   HTML(paste0("<b>",shops$maize.owner.agree.biz_name[shops$shop_ID == input$mymap_marker_click$id],"</b>"))

  })
  
     output$textR2<-renderText({
print(paste("number of reviews:", shops$nr_reviews[shops$shop_ID == input$mymap_marker_click$id], sep=" "))
  })


  output$my_ui<-renderUI({
   
      img(src=shops$maize.owner.agree.q13[shops$shop_ID == input$mymap_marker_click$id], height = '300px')

  })
      output$stars_ui <- renderUI({
        # to calculate our input %
    
        # element will look like this: <div class="full-stars" style="width:n%"></div>
        ### this is rated in a scale of 1 to 5 so map to 0 - 100 percent
        style_value <- sprintf("width:%s%%", (shops$rating[shops$shop_ID == input$mymap_marker_click$id] - 1)/4*100)
        tags$div(class = "full-stars", style = style_value)
    })
      output$dot_plot <-  renderPlot({
      if (input$radio_comp_select == "all") {
      ggplot(rbind(dot_frame[dot_frame$shop_ID == input$mymap_marker_click$id,], all_dot_frame), aes(attribute, score)) + geom_point(aes(color=shop_ID),size= 2.5) + coord_flip() + ylim(0,100) + geom_line(aes(group = attribute))  + scale_color_manual(values=c( "orange", "black")) + theme(legend.position = "none")+ theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
} else {      
catch_dot_frame <- aggregate(dot_frame[dot_frame$catchID ==  shops$catchID[shops$shop_ID==input$mymap_marker_click$id],],by=list(dot_frame[dot_frame$catchID ==  shops$catchID[shops$shop_ID==input$mymap_marker_click$id],]$attribute),FUN=mean,na.rm=T)

catch_dot_frame$attribute <- catch_dot_frame$Group.1
catch_dot_frame$Group.1 <- NULL
catch_dot_frame$shop_ID <- "catch_av"
      ggplot(rbind(dot_frame[dot_frame$shop_ID == input$mymap_marker_click$id,], catch_dot_frame), aes(attribute, score)) + geom_point(aes(color=shop_ID),size= 2.5) + coord_flip() + ylim(0,100) + geom_line(aes(group = attribute))  + scale_color_manual(values=c( "orange", "black")) + theme(legend.position = "none") +theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
      }
        
    
    })

}

shinyApp(ui, server)


