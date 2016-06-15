library(shiny)
library(shinyFiles)

setwd(dirname(parent.frame(2)$ofile))

source("script1.R")

lociChoices <- c('KIR2DL1','KIR2DL2/3','KIR2DL4','KIR2DL5','KIR2DS1','KIR2DS2',
                 'KIR2DS4','KIR2DS3/5','KIR3DL1/S1','KIR3DL2','KIR3DL3','KIR2DP1','KIR3DP1')

ui <- fluidPage(
  
  titlePanel(
    strong(tags$u(style="color:darkred","HML Parser"))
  ),
  
  hr(),
  
  sidebarLayout(position = 'right',
    
    #Sidebar panel contains acceptable locus checkboxes and output type buttons
    sidebarPanel(
      radioButtons(inputId = "print", label = h4(em("Choose Output Type")), 
                   choices = list("Seperate loci files"=1, "Single file with combined loci"=2),selected = 1),
      checkboxGroupInput(inputId = "loci", label = h4(em("Select Accepted Loci")),
                         choices=lociChoices, selected = lociChoices)
      
    ),
    
    #Main panel contains Choose Directory button and text output
    mainPanel(
      h2("Choose HML Files to Parse"),
      shinyDirButton('directory', label = "Choose Directory", title="Select directory containing your HML files"),
      hr(),
      # tags$br(),
      # tags$br(),
      verbatimTextOutput('tb'),
      hr(),
      tags$div(
        h3("What is This?"),
        
        HTML(paste0(em("Histoimmunogenetics Markup Language (HML)"),
                      " is a type of XML file specifically used for electronically transmitting HLA testing data.",
                      tags$strong(tags$u(style="color:darkred","HML Parser")), 
                      " reads in a folder of HML files and parses them into output text files.
                      Used in conjunction with ", tags$span(tags$a(href="https://github.com/jillah/HML", "HML Maker")),
                      ", transmitting HLA testing data is incredibly streamlined and intuitive.")
        ),
        
        tags$br(),
        tags$br(),
        HTML(paste0("This RStudio Shiny app was created by Sam Hollenbach, June 2016, under the supervision of Jill Hollenbach for ", 
                    strong(style = "color:darkblue","UCSF Department of Neurology"))
        ),
        
        
        tags$br(),
        tags$br(),
        tags$span("For more information see "),
        tags$a(href="https://github.com/samhollenbach/HML_Parser","https://github.com/samhollenbach/HML_Parser."),
        tags$br(),
        tags$br(),
        
        h4(HTML(paste0("How to use ", tags$strong(tags$u(style="color:darkred","HML Parser"))))),
        tags$span(
          tags$li(HTML(paste0("Select desired folder containing one or more HML files."))),
          tags$li(HTML(paste0("Selecting ",strong("Seperate loci files"), " will create multiple \'genos_", em("locus"), 
                      ".txt\' files for each unique locus read from the HML files, containing a list of ", em("samples"),
                      " and corresponding ", em("glstrings.")))),
          tags$li(HTML(paste0("Selecting ",strong("Single file with combined loci"), " will create a single file \'genos.txt\' 
                      containing a list of each unique ", em("sample"), " followed by ", em("glstrings"), " from each available ",
                      em("locus"),  ", appended by ~."))),
          tags$li(HTML(paste0("Both options produce a \'metadata.txt\' file containing all other data from the HML files."))),
          tags$li(HTML(paste0("Only HML files with a ",
                              em("locus"), " matching one of the selected loci will be parsed.")))
        )
      )
    )
  )
)

server <- shinyServer(function(input, output, session){
  
  roots = c(wd='.')
  
  
  #Controls the shinyFiles directory chooser
  shinyDirChoose(input,id='directory',session=session,roots=roots)
  
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
      return("Directory contains no \'.xml\' files, nothing to parse")
    }else{
      return(paste0(length(data()), " .xml file(s) selected and parsed"))
    }
    
  })
  
  
})

shinyApp(server = server, ui = ui)


