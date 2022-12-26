library(shiny)
library(tidyverse)
library(DT)


## button color
## Ability to add more columns or remove those unused?

v <- c()

ui <- fluidPage(
  
  ## Upload WSPs
  sidebarPanel(width = 3,
               fileInput("files", "Choose RDS File", accept = ".rds", multiple = F),
               
               hr(),
               
               textInput("exp", label = "Experiment*", value = ""),
               
               fluidRow(
                 column(6,
                        textInput("expIDM", label = "Experiment Group", value = "")
                 ),
                 column(6,
                        selectizeInput("expID",'.', c(Choose=''),multiple = F)
                 )
                 
               ),
               
               fluidRow(
                 column(6,
                        textInput("mouseM", label = "Mice", value = "")
                 ),
                 column(6,
                        selectizeInput("mouse",'.', c(Choose=''),multiple = T)
                 )
                 
               ),
               
               fluidRow(
                 column(6,
                        textInput("inhibitorM", label = "Inhibitors", value = "")
                 ),
                 column(6,
                        selectizeInput("inhibitor",'.', c(Choose=''),multiple = T)
                 )
                 
               ),
               
               fluidRow(
                 column(6,
                        textInput("dilutionM", label = "Dilution", value = "")
                 ),
                 column(6,
                        selectizeInput("dilution",'.', c(Choose=''),multiple = T)
                 )
                 
               ),
               
               fluidRow(
                 column(6,
                        textInput("stimM", label = "Stimulation", value = "")
                 ),
                 column(6,
                        selectizeInput("stim",'.', c(Choose=''),multiple = T)
                 )
                 
               ),
  
               fluidRow(
                 column(6,
                        textInput("concM", label = "Start_Conc.", value = "")
                 ),
                 column(6,
                        selectizeInput("conc",'.', c(Choose=''),multiple = T)
                 )
                 
               ),
               
               fluidRow(
                 column(6,
                        textInput("roM", label = "ReadOut", value = "")
                 ),
                 column(6,
                        selectizeInput("ro",'.', c(Choose=''),multiple = T)
                 )
                 
               ),

               dateInput("dateExp", "Date:" , format = "dd/mm/yy")
  ),
  
  mainPanel(
   
    tabsetPanel(type = "tabs",
                tabPanel("Past Experiments", 
                         
                         fluidRow(
                           column(3,
                                  textAreaInput("purpose", label = "Purpose", value = "",resize = "none")
                           ),
                           column(3,
                                  textAreaInput("design", label = "Design", value = "",resize = "none")
                           ),
                           column(3,
                                  textAreaInput("results", label = "Results", value = "",resize = "none")
                           ),
                           column(3,
                                  textAreaInput("notes", label = "Notes", value = "",resize = "none")
                           )
                           
                         ),
                         
                         fluidRow(
                           column(width = 1,
                                  actionButton("cleanInput",class="btn btn-info", 
                                               label = tags$em("Clean Input"))
                           ),
                           
                           column(offset = 3, width = 8,
                                  column(offset = 8,width = 1,
                                         actionButton("updatePE",class="btn btn-info", 
                                                      label = tags$em("Update Table"))
                                  ),
                                  column(offset = 1,width = 1,
                                         downloadButton("downloadPE",class="btn btn-info", 
                                                        label = tags$em("Save Table"))
                                  
                                  )
                             
                           )
                           
                           ),
                                  
                         div(dataTableOutput("petable"), style = "font-size: 80%; width: 50%")
                         
                         
                ),
                tabPanel("Future Experiments", 
                         verbatimTextOutput("summary")
                         )
    )
  
    
  )
  
  
)


server <- function(input, output, session) {
  
  pastexpTable <- reactiveVal()
  
  observeEvent(input$files,{
    
    file <- readRDS(file = input$files[1,4])
    
    
    #file <- file %>%
    #          mutate(across(c(Mouse_lines,Inhibitors,Dilutions,Stimulation,Concentration, Read_out),~any(str_detect(., fixed(' '))), ~list(str_split(., fixed(' '))) ))
    updateSelectizeInput(session, "mouse",
                         choices = unlist(file$Mouse_lines),
                         selected = ""
    )
    updateSelectizeInput(session, "expID",
                         choices = file$Exp_group,
                         selected = ""
    )
    updateSelectizeInput(session, "inhibitor",
                         choices = unlist(file$Inhibitors),
                         selected = ""
    )
    updateSelectizeInput(session, "dilution",
                         choices = unlist(file$Dilutions),
                         selected = ""
    )
    updateSelectizeInput(session, "stim",
                         choices = unlist(file$Stimulation),
                         selected = ""
    )
    updateSelectizeInput(session, "conc",
                         choices = unlist(file$Concentration),
                         selected = ""
    )
    updateSelectizeInput(session, "ro",
                         choices = unlist(file$Read_out),
                         selected = ""
    )
    
print(unlist(file$Mouse_lines))
    
      pastexpTable(file)

  })

  
  ## display the experiment overviews if there is one before

  
  output$petable <- renderDataTable({
    
    if(!is.null(pastexpTable())){
      pastexpTable()
    }
  
  })
  
  observeEvent(input$mouseM,{
    
    v <- str_split(input$mouseM,pattern = " ")[[1]]
    
    updateSelectizeInput(session, "mouse",
                         choices = c(unlist(pastexpTable()$Mouse_lines), v),
                         selected = c(input$mouse,v)
                         )

    
  })
  
  observeEvent(input$expIDM,{
    
    v <- input$expIDM
    
    updateSelectizeInput(session, "expID",
                         choices = c(unlist(pastexpTable()$Exp_group), v),
                         selected = v
    )
    
    
  })
  
  observeEvent(input$inhibitorM,{
    
    v <- str_split(input$inhibitorM,pattern = " ")[[1]]
    
    updateSelectizeInput(session, "inhibitor",
                         choices =  c(unlist(pastexpTable()$Inhibitors), v),
                         selected = c(input$inhibitor,v)
    )
    
    
  })
  
  observeEvent(input$dilutionM,{
    
    v <- str_split(input$dilutionM,pattern = " ")[[1]]
    
    updateSelectizeInput(session, "dilution",
                         choices =  c(unlist(pastexpTable()$Dilutions), v),
                         selected = c(input$dilution,v)
    )
    
    
  })
  
  observeEvent(input$stimM,{
    
    v <- str_split(input$stimM,pattern = " ")[[1]]
    
    updateSelectizeInput(session, "stim",
                         choices =  c(unlist(pastexpTable()$Stimulation), v),
                         selected = c(input$stim,v)
    )
    
    
  })
  
  observeEvent(input$concM,{
    
    v <- str_split(input$concM,pattern = " ")[[1]]
    
    updateSelectizeInput(session, "conc",
                         choices =  c(unlist(pastexpTable()$Concentration), v),
                         selected = c(input$conc,v)
    )
    
    
  })
  
  observeEvent(input$roM,{
    
    v <- str_split(input$roM,pattern = " ")[[1]]
    
    updateSelectizeInput(session, "ro",
                         choices =  c(unlist(pastexpTable()$Read_out), v),
                         selected = c(input$ro,v)
    )
    
    
  })
  

  observeEvent(input$cleanInput,{
    source("cleanInput.R", local = T)
  })
  
  
  observeEvent(input$updatePE,{
    
    if(input$exp != ""){
      
      inpast <- tibble(Experiment = input$exp,
                       Exp_group = input$expID,
                       Mouse_lines = list(input$mouse),
                       Inhibitors = list(input$inhibitor),
                       Dilutions = list(input$dilution),
                       Stimulation = list(input$stim),
                       Concentration = list(input$conc),
                       Read_out = list(input$ro),
                       Date = input$dateExp,
                       Purpose = input$purpose,
                       Design = input$design, 
                       Results = input$results, 
                       Notes = input$notes)
      

      
        
        if(is.null(pastexpTable()) ){
          
          pastexpTable(inpast)
          
        } else{
          print(inpast)
          print(pastexpTable)
          
          combined <- bind_rows(inpast, pastexpTable())
          
          pastexpTable(combined)
        }
      
    }
    
  })
  
  output$downloadPE <- downloadHandler(
    filename = function() {
      paste("Experiments Overview", ".rds")
    },
    content = function(file) {
      
      
      saveRDS(pastexpTable(),file)
      #unlistPE <- pastexpTable() %>% 
      #              rowwise() %>% 
      #              mutate_if(is.list, ~paste(unlist(.), collapse = ' ')) 
      #
      #write.csv(unlistPE, file, row.names = FALSE)
    }   
  )
  
  
}


shinyApp(ui, server)