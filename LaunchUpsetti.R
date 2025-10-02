library(tidyverse)
library(UpSetR)
library(R6)
library(shiny)
library(shinycssloaders)
library(shinyjs)
setwd("D:/Bioinfo/UpsettiPlotSpaghetti")


ui = fluidPage(
  useShinyjs(),
  tags$style(HTML("
    
    body {
      background-color: #f0f4f8; /*soft blue-gray */
      color: #333;
    }

    .container-fluid {
      padding: 20px;
    }

    .btn {
      background-color: #1e88e5; /*vibrant blue*/
      color: white;
      border: none;
      border-radius: 8px;
      font-weight: bold;
      box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
      transition: all 0.2s ease;
    }
    
    .btn:hover {
      background-color: #1565c0; /*darker blue on hover*/
      transform: translateY(-2px);
      box-shadow: 0 6px 8px rgba(0, 0, 0, 0.15);
    }
    
    .dataTables_wrapper .dt-buttons {
      float: right;
    }
    
    .dataTable th {
      background-color: #004d40 !important; /*dark teal header*/
      color: white !important;
      font-weight: bold;
      border-bottom: 2px solid #00251a;
    }
    
    .dataTable tr:nth-child(even) {
      background-color: #e8f5e9; /*light green even rows*/
    }
    
    .dataTable tr:nth-child(odd) {
      background-color: #ffffff; /*white odd rows*/
    }
    
    .dataTables_wrapper {
        border-radius: 8px;
        overflow: hidden;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
    }
    
    #gets_representation {
        background-color: #d9534f; 
        color: white; 
        border-color: #ac2925;
        padding: 10px 16px;
        font-size: 16px;
        border-radius: 6px;
      }

    #gets_representation:hover {
        background-color: #c9302c;
        border-color: #761c19;
      }
  ")),
  
  titlePanel(div(title = "UpsettiPlotSpaghetti", 
            h3 = "An Upset plotting device with bioinformatics focus",
            style = "color:grey;")),
  fileInput("Upsetti_Input", label = "Upload csv file", accept = c(".csv", ".xlsm", ".xlsx")),
  actionButton("Upsetti_File_Submit", label = "Start Plotting"),
  conditionalPanel(condition = "input.Upsetti_File_Submit > 0",
                  numericInput("Show_Num", label = "How many bars to show?", min = 0, max = 1000, value = 0)),
  div(style = "display: none;",numericInput("FilterCutoff", label = "Filter Cutoff value: ", value = 1)),
  br(),
  plotOutput(outputId = "Upsetti_plot", width = 1600, height = 800),
  uiOutput("Upsetti_Elements")
)
  
server = function(input, output, session) {
  
  Current_File_Upsetti = reactiveVal("UpsettiBuffer")
  Upsetti_sheet = reactiveVal(NULL)
  Elem_list = reactiveVal("elemlistBuff")
  only_1_0_check = reactiveVal(T)
  UpsettiObj_react = reactiveVal("ObjBuffer")
  FirstPass = reactiveVal(T)
  
  observeEvent(input$Upsetti_Input, {
    
    Current_File_Upsetti(input$Upsetti_Input)
    
    if(str_detect(Current_File_Upsetti()$datapath, ".xlsm|.xlsx")) {

      showModal(modalDialog(
        title = "Please Select Sheet Name",
        textInput("Upsetti_sheet_input", "Sheet: "),
        easyClose = T,
        footer = modalButton("Submit Sheet Selection")
      ))
    } else{Upsetti_sheet(NULL)}
    
    Upsetti_sheet(input$Upsetti_sheet_input)
   
    
  })
  
  observeEvent(input$Upsetti_File_Submit, {
    source("upsetti_utils.R", local = T)
    
    Upsetti_Test = UpsettiObj$new(JobName = "TEST", PathToFile = Current_File_Upsetti()$datapath)
    Upsetti_Test$ReadFile()
    
    UpsettiObj_react(Upsetti_Test)
    Check_list = Input_checker(UpsettiObj_react()$InputDF)
    only_1_0_check(Check_list[[3]])
    
    if (!Check_list[[1]] || !Check_list[[2]]) {
      showModal(modalDialog(
        title = "Warning, invalid input file structure",
        paste("Either the column names aren't unique or the element names aren't unique."),
        easyClose = T,
        footer = modalButton("OK")
      ))
    }
    if(FirstPass()) {
      
      if (!only_1_0_check()) {
        
        showModal(modalDialog(
          title = "Warning, values higher than 1 or 0 detected in input file.",
          paste("Since there are values bigger than 1 or 0, please select a Filter Cutoff value to convert data to binary and press Start Plotting again."),
          numericInput("FilterCutoff", label = "Filter Cutoff value: ", value = 2),
          easyClose = T,
          footer = modalButton("OK")
        )); FirstPass(F); return(NULL)
      } else {
        showModal(modalDialog(
          title = "Binay data detected",
          #div(style = "display: none;",numericInput("FilterCutoff", label = "Filter Cutoff value: ", value = 1)),
          easyClose = T,
          footer = modalButton("OK")
        ))
      }
    }
 
    Upsetti_list = PlotUpsetti(UpsettiObj_react()$InputDF, input$FilterCutoff, Show_Number_Bars = input$Show_Num)
    UpsettiTest = Upsetti_list[[1]]
    Upsetti_bar_colors = Upsetti_list[[2]]
    elemlisttest = Upsetti_list[[3]]
    
    if(input$Show_Num <= length(elemlisttest) && input$Show_Num >= 0 && !identical(UpsettiTest, "Warning_ShowNum_2Big")) {
   
      if (!identical(elemlisttest[[1]], "Warning_ShowNum_2Big")) {
        Elem_list(elemlisttest)
      }
      
      output$Upsetti_plot = renderPlot(UpsettiTest)
      
      if (input$Show_Num != 0) {
        
        output$Upsetti_Elements = renderUI({
          
          elements_html <- mapply(function(elem_group, color, index) {
            intersect_header <- paste0("Barchart Section ", index, ": (length:", length(elem_group), ")")
            intersect_content <- paste(elem_group, collapse = ", ")
            
            tags$div(
              style = paste0("color:", color, "; margin-bottom: 15px; border-left: 3px solid ", color, "; padding-left: 10px;"),
              tags$p(style = "font-weight:bold; font-size:1.1em; margin-bottom: 5px;", intersect_header),
              tags$p(style = "margin: 0;", intersect_content)
            )
            
          }, Elem_list()[1:(input$Show_Num)], Upsetti_bar_colors[1:(input$Show_Num)],
          seq_along(Elem_list()[1:(input$Show_Num)]), SIMPLIFY = FALSE)
          
          tagList(elements_html)
        })}
 
    } else {showModal(modalDialog(
      title = "Warning, invalid number of bars selected for coloring.",
      paste("You've selected ", input$Show_Num, " bars for coloring, this is over the total amount of interesctions found, which is: ", length(Elem_list()),
            ". Please select a valid number from 1 to ", length(Elem_list()), "."),
      easyClose = T,
      footer = modalButton("OK")
    ))}
  })
}

  
shinyApp(ui, server)
