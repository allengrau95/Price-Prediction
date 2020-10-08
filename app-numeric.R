############################################

############################################
setwd("O:/Chem/CDCM/Chemistry/Diplom/2020 Allen Grau/06_Price Prediction Web App")
# Import libraries
library(shiny)
library(data.table)
library(randomForest)

# Read in the RF model
model <- readRDS("model.rds")


####################################
# User interface                   #
####################################

ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Proposal Price Prediction'),
  
  # Input values
  sidebarPanel(
    #HTML("<h3>Input parameters</h3>"),
    tags$label(h3('Input parameters')),
    selectInput("Supplier","Supplier",
                c("Aarti","Patheon","ALMAC","Aptuit","Arran","Asymchen","Carbogen Amcis","Carbosynth","Chiralquest","CML","Corden","Dishman","Dottikon","Evonik","Flamma"
                                        ,"Helsinn","Hikal","Hovione","Inalco","Inogent","Jiuzhou","Langhua","Lonza","Novasep","Patheon","PharmaBlock","Porton","Raybow","SAI","Rohner",
                                        "Siegfried","SK Biotek", "Syngene")),
                 
    selectInput("Location","Location",
                c("Europe","China","Switzerland","India","USA")),
                
    numericInput("GMP",
                 label = "GMP", 
                 value = 3),
    numericInput("Kilo", 
                 label = "Kilo", 
                 value = 3000),
    numericInput("Batches",
                 label="Batches",
                 value=6),
    numericInput("Time",
                 label="Time",
                 value=35),
    numericInput("Complexity",
                 label="Complexity",
                 value= 18),
    actionButton("submitbutton", "Submit", 
                 class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("Supplier",
               "Location",
               "GMP",
               "Kilo",
               "Batches",
               "Time",
               "Complexity"),
      Value = as.character(c(input$Supplier,
                             input$Location,
                             input$GMP,
                             input$Kilo,
                             input$Batches,
                             input$Time,
                             input$Complexity)),
      stringsAsFactors = FALSE)
    
  Price<-0
   df <- rbind(df, Price)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    Output <- data.frame(Prediction=predict(model,test))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)
