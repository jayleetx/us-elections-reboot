#SHINY


#ui 

ui <- fluidPage(
  titlePanel("ncVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with 
        data from the North Carolina voter registry."),
      
      selectInput("var", 
                  label = "Pick a demographic:",
                  choices = c("Density","Party","Age","Percent White", 
                              "Percent Black","Percent Hispanic","Percent Asian","Percent Multiracial"),
                  selected = "Density")
    ),
    
    mainPanel(imageOutput("map"))
  )
)



#server 

server <- function(input,output){
  
  output$map <- renderImage({
    
    if(input$var == "Density"){
      list(src = "density_map.png",
           contentType = "image/png",
           width = 1200,
           height = 750,
           alt = "Density Map")
    }
    else if(input$var == "Party"){
      list(src = "maps/party_map.png",
           contentType = "image/png",
           width = 1200,
           height = 750,
           alt = "Party Map")
    }
    else if(input$var == "Age"){
      list(src = "maps/age_map.png",
           contentType = "image/png",
           width = 1200,
           height = 750,
           alt = "Age Map")
    }
    else if(input$var == "Percent White"){
      list(src = "maps/white_map.png",
           contentType = "image/png",
           width = 1200,
           height = 750,
           alt = "White Map")
    }
    else if(input$var == "Percent Black"){
      list(src = "maps/black_map.png",
           contentType = "image/png",
           width = 1200,
           height = 750,
           alt = "Black Map")
    }
    else if(input$var == "Percent Hispanic"){
      list(src = "maps/hispanic_map.png",
           contentType = "image/png",
           width = 1200,
           height = 750,
           alt = "Hispanic Map")
    }
    else if(input$var == "Percent Asian"){
      list(src = "maps/asian_map.png",
           contentType = "image/png",
           width = 1200,
           height = 750,
           alt = "Asain Map")
    }
    else if(input$var == "Percent Multiracial"){
      list(src = "maps/mixed_map.png",
           contentType = "image/png",
           width = 1200,
           height = 750,
           alt = "Multiracial Map")
    }
  },deleteFile = FALSE)

}

shinyApp(ui=ui,server=server)