library(shiny)
library(shinyFiles)

setwd(dirname(parent.frame(2)$ofile))

source("script1.R")

ui <- fluidPage(
  
  titlePanel(h2("HML Parser")),
  
  hr(),
  
  sidebarLayout(
  
    sidebarPanel(
      checkboxGroupInput(inputId = "loci", label = h4("Select Accepted Loci"),
                     choices=list("3DL2","3DL3"), selected = c("3DL2","3DL3")),
      hr(),
  
      radioButtons(inputId = "print", label = h4("Choose Output Type"), 
               choices = list("Seperate loci files"=1, "Single file with combined loci"=2),selected = 1)
    ),
    mainPanel(
      shinyDirButton('directory', label = "Choose HML Files to Parse", title="Select directory containing your HML files"),
      hr(),
      verbatimTextOutput('tb')
    )
  )
)

server <- shinyServer(function(input, output, session){
  
  roots = c(wd='.')
  
  shinyDirChoose(input,'directory',session=session,roots=roots)
  
  
  data <- reactive({
    locus <- input$loci
    out_type <- input$print
    
    if(is.null(input$directory)){
      return(NULL)
    }
    
    dir <- parseDirPath(roots,input$directory)
    
    if(is.null(dir)){
      return(NULL)
    }
    
    observe(print(dir))
    files <- list.files(dir,full=TRUE,pattern = ".xml")
    
    if(is.null(files)){
      return(NULL)
    }
    
    if(length(files) > 0){
      run.parser(files,out_type,locus)
    }
    
    
    return(files)
    
    
    
  })
  
  
  output$tb <- renderText({
    if(is.null(data())){
      return("No Directory Selected")
    }
    if(length(data()) == 0){
      return("Directory contains no .xml files, nothing to parse")
    }else{
      return(paste0(length(data()), " .xml file(s) selected and parsed"))
    }
    
    
  })
  
  
  
})

shinyApp(server = server, ui = ui)


