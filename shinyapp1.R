library(shiny)

ui <- fluidPage(
  
  checkboxGroupInput(inputId = "loci", label = h3("Select Loci"),
                     choices=list("3DL2","3DL3"), selected = c("3DL2","3DL3")),
  
  
  hr(),
  
  radioButtons(inputId = "print", label = "Choose Output Type", 
               choices = list("Seperate loci files"=1, "Single file with combined loci"=2),selected = 1)
  
  
)

server <- function(input, output){
  
}

shinyApp(server = server, ui = ui)