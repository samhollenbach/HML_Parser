library(shiny)

setwd(dirname(parent.frame(2)$ofile))

source("script1.R")

ui <- fluidPage(
  
  checkboxGroupInput(inputId = "loci", label = h3("Select Loci"),
                     choices=list("3DL2","3DL3"), selected = c("3DL2","3DL3")),
  
  hr(),
  
  radioButtons(inputId = "print", label = "Choose Output Type", 
               choices = list("Seperate loci files"=1, "Single file with combined loci"=2),selected = 1),
  
  fileInput(inputId = "file", label = "Choose HML Files", multiple = TRUE ,accept = '.xml'),
  
  verbatimTextOutput('tb')
)

server <- function(input, output){
  
  
  
  data <- reactive({
    locus <- input$loci
    out_type <- input$print
    
    fileNames <- input$file[['name']]
    
    if(is.null(fileNames)){
      return(NULL)
    }
    file.copy(input$file[['datapath']], paste0("tmp/", input$file[['name']]))
    
    returnValue(fileNames)
    
    
    
    run.parser(list.files("tmp",full=TRUE),out_type,locus)
  })
  
  #run.parser(inFiles,out_type,loci)
  
  
  output$tb <- renderPrint({
    if(is.null(data())){
      
    }
    data()
  })
  
  
  #SO RIGHT NOW IT CREATES THE FILES FROM ALL THE TEMP FILES STORED IN /tmp SO NEED TO FIND A WAY TO UNLOAD/KEEP WHICH ONES I WAN
  #BUT GOOD JOB SO FAR :D
  
  
}

shinyApp(server = server, ui = ui)


