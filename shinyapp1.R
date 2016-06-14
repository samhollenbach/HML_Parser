library(shiny)
library(shinyFiles)
library(shinythemes)

setwd(dirname(parent.frame(2)$ofile))

source("script1.R")

lociChoices <- c('KIR2DL1','KIR2DL2/3','KIR2DL4','KIR2DL5','KIR2DS1','KIR2DS2',
  'KIR2DS4','KIR2DS3/5','KIR3DL1/S1','KIR3DL2','KIR3DL3','KIR2DP1','KIR3DP1')

ui <- fluidPage(
  theme = shinytheme("spacelab"),
                
  
                
  
  titlePanel(
    strong(tags$u(style="color:darkred","HML Parser"))),
  
  
  hr(),
  
  sidebarLayout(
  
    #Sidebar panel contains acceptable locus checkboxes and output type buttons
    sidebarPanel(
      
      
      
      checkboxGroupInput(inputId = "loci", label = h4(em("Select Accepted Loci")),
                     choices=lociChoices, selected = lociChoices),
      #hr(),
      radioButtons(inputId = "print", label = h4(em("Choose Output Type")), 
               choices = list("Seperate loci files"=1, "Single file with combined loci"=2),selected = 1)
      
    ),
    
    #Main panel contains Choose Directory button and text output
    mainPanel(
      h2("Choose HML Files to Parse"),
      shinyDirButton('directory', label = "Choose Directory", title="Select directory containing your HML files"),
      hr(),
      verbatimTextOutput('tb'),
      hr(),
      tags$div(
        h2("What is This?"),
        
        tags$span(
          HTML(paste0(tags$strong(tags$u(style="color:darkred","HML Parser")), " reads in a folder of HML files and parses them into output text files. 
                      This Shiny app was created by Sam Hollenbach under the supervision of Jill Hollenbach for ", 
                      em(style = "color:darkblue","UCSF Department of Neuroscience"), ". For more information see:"))
        ),
        tags$a(href="https://github.com/samhollenbach/HML_Parser","https://github.com/samhollenbach/HML_Parser"),
        
        h4("Accepted Loci"),
        tags$span(
          p("Only HML files with a locus matching one of the selected loci will be parsed.")
        ),
        
        
        h4("Output Type"),
        tags$span(
          HTML(paste0("• Selecting ",strong("Seperate Loci Files"), " will create multiple \"genos_", em("locus"), 
                      ".txt\" files for each unique locus read from the HML files, containing a list of ", em("samples"),
                      " and corresponding ", em("glstrings."))),
          tags$br(),
          HTML(paste0("• Selecting ",strong("Single file with combined loci"), " will create a single file \"genos.txt\ 
                      containing a list of each unique ", em("sample"), " followed by ", em("glstrings"), " from each available ",
                      em("locus"),  ", appended by ~."))
        )
      )
    )
  )
)

server <- shinyServer(function(input, output, session){
  
  roots = c(wd='.')
  
  #Controls the shinyFiles directory chooser
  shinyDirChoose(input,'directory',session=session,roots=roots)
  
  #Reactive function which controls the 3 variables (files, out_type, locus)
  data <- reactive({
    
    #Reads locus and output type from shiny widget inputs
    locus <- input$loci
    out_type <- input$print
    
    #Returns null if user has not yet selected a directory
    if(is.null(input$directory)){
      return(NULL)
    }
    
    #Gets selected directory and returns NULL if the directory is NULL
    dir <- parseDirPath(roots,input$directory)
    if(is.null(dir)){
      return(NULL)
    }
    
    #Gets all .xml files in dir
    files <- list.files(dir,full=TRUE,pattern = ".xml")
    
    #Returns NULL if there are no .xml files in the directory
    if(is.null(files)){
      return(NULL)
    }
    
    #Runs parser if there are files to parse
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


