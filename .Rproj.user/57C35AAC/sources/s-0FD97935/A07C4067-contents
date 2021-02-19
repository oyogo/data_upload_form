
library(shiny)
library(shinydashboard)
library(RSQLite)
library(dplyr)
library(shinyWidgets)
library(shinyalert)


dbConn <- dbConnect(RSQLite::SQLite(), "data/makueni_test_db.db")

# Agriculture datasets
crops <- dbReadTable(dbConn,"combined_hortfood_crops_makueni")
livestock_ai <- dbReadTable(dbConn,"livestock_ai")
livestock_population <- dbReadTable(dbConn,"livestock_population")

# Water module datasets
boreholes <- dbReadTable(dbConn,"makueni_boreholes")
sanddams <- dbReadTable(dbConn,"makueni_sanddams")
earthdams <- dbReadTable(dbConn,"makueni_earthdams")
Makueni_total_sanddams <- dbReadTable(dbConn,"Makueni_total_sanddams")


dbDisconnect(dbConn)

ui <- fluidPage(
  
  useShinyalert(),
  
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "linear",
    direction = "bottom"
  ),
  
  #(title = "Data upload form"),
   sidebarLayout( 
     
     sidebarPanel( 
     
     
       shiny::selectInput("modules","select module: ", choices = c("Agriculture","Water","Education")),
       
       radioButtons("dataitems","Select file category to upload: ",c("Crop_production","Livestock breeding","Livestock population")),
    
     
       p("Please download a template with the button below 
             so that you can get to see how the data file looks and then format your file in like manner."),
       br(),
       shiny::downloadButton("downloadtemplate","Download Template"),
       br(),
       br(),
       wellPanel( style = "background: #4A708B",
                  fileInput( "myfile","Upload your file", buttonLabel = "Browse...", accept = ".csv")
                  
       ),
       
       shiny::actionButton("checkcols",label = "Check if files match"),
       
       uiOutput("cols"),
      
   ),
  mainPanel(
       div(
           class = "ui grid",
            div(
                class = "sixteen wide column",
                
                div(class = "ui center aligned segment",
                    h3("Uploaded file")),
                div( 
                    class = "ui teal inverted segment",
                    
                DT::dataTableOutput("uploadedfile", width = 1000)
                
                )
            ),
           div(
               class = "sixteen wide column",
               div(class = "ui center aligned segment",
                   h3("Database file")),
               div( 
                   class = "ui olive inverted segment",
                   
               DT::dataTableOutput("databasefile", width = 1000)
               
               )
           )
       )
   )
  )
)

## Server logic
server <- function(input, output, session) {

    
    df_upload <- reactive({
        
        inFile <- input$myfile 
        if(is.null(inFile))
            return(NULL)
        
        df <- read.csv(inFile$datapath,header = TRUE) %>%
            dplyr::select(sort(names(.)))
        
        return(df)
    })
    
  
    observeEvent(input$modules,{
      if(input$modules=="Water"){
        
        updateRadioButtons(session,"dataitems", choices = c("Dams and water projects","Boreholes","Earth dams","Sand dams"))
      } else if(input$modules=="Agriculture"){
        
        
        updateRadioButtons(session,"dataitems", choices = c("Crop_production","livestock breeding","Livestock population"))
      }
    })
   
    
    db_file <- reactive({
     
    switch(input$dataitems, 
            
              "Crop_production" = crops,
              "livestock breeding" = livestock_ai,
              "Livestock population" = livestock_population,
              "Boreholes" = boreholes,
              "Sand dams" = sanddams,
              "Earth dams" = earthdams,
              "Dams and water projects" = Makueni_total_sanddams
              )
 
   })
   
 
    observeEvent(input$myfile,{ 
    
    
    observeEvent(input$checkcols,{

     colmatch <-  all(colnames(df_upload()) %in% colnames(db_file()))
     
     output$cols <- renderUI({
  
     if(colmatch==TRUE ){
  
         wellPanel(
           
           h5("You can now submit your file!"),
           
           shiny::actionButton("submit","Submit file")
           
          
         )
     
     } else {
       
    
         wellPanel( style = "background: #009ACD",
       
       h3("please ensure the following column names match the ones on the database file"),
       
       verbatimTextOutput("results")
    
         )
      
     }
    
     })
 
     output$results <- renderPrint({

      writeLines(setdiff(names(df_upload()),names(db_file())))
       
     }) 
   
        })
    
})
    
    output$uploadedfile <- DT::renderDataTable(
        df_upload() ,
        rownames = FALSE,options = list(scrollX = TRUE)) 
    
    output$databasefile <- DT::renderDataTable(
      db_file(),
        rownames = FALSE,options = list(scrollX = TRUE))
    
    
    output$downloadtemplate <- downloadHandler(
      
      filename = function() {
        paste(input$dataitems,"_template.csv", sep = "")
      },
      content = function(file) {
        write.csv(db_file()[1:5], file, row.names = FALSE)
      }
      
     )
    
    
    # submit
    observeEvent(input$submit,{
      
      dbConn <- dbConnect(RSQLite::SQLite(), "data/makueni_test_db.db")
   
      a <- df_upload()
      b <- db_file()
      
      for (x in colnames(df_upload()))
      {
      
        
        a[,x] <- eval( call( paste0("as.", class(b[,x])), a[,x]) )
        
      }
      
      combined_table <-   a %>%
        dplyr::full_join(b)
      
      databasefile_name <- reactive({
        switch(input$dataitems,
               
               "Crop_production" = "combined_hortfood_crops_makueni",
               "livestock breeding" = "livestock_ai",
               "Livestock population" = "livestock_population",
               "Boreholes" = "makueni_boreholes",
               "Sand dams" = "makueni_sanddams",
               "Earth dams" = "makueni_earthdams",
               "Dams and water projects" = "Makueni_total_sanddams"
        )    
          
        
      })
 
      
      dbWriteTable(dbConn,paste(databasefile_name(), sep = ""), combined_table, overwrite = TRUE)
    
      
      dbDisconnect(dbConn)
      
      
      shinyalert::shinyalert("Successful!", paste0("Your file has been submited into the ",databasefile_name()," table in the database"), type = "success")
      
     
      
    })
    
  
}

# Run the application 
shinyApp(ui = ui, server = server)
