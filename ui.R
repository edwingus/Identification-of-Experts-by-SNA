
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(dashboardPage(

  # Application title
  dashboardHeader(title = "Expert Identification"),

  # Sidebar with a slider input for number of bins
  # sidebarLayout(
  dashboardSidebar(
      tags$head(
        tags$style(type="text/css", '#t_result tfoot, #t_selrow tfoot, #t_queue tfoot {display:none;}'),
        tags$style(type="text/css", '#t_result tr, #t_selrow tr, #t_queue tr {border:1px solid blue;}'),
        tags$style(type="text/css", "#btn_analysis, #btn_add {color:green;border:1px solid black;}"),
        tags$style(type="text/css", "#btn_remove, #btn_clear  {color:red;border:1px solid black;}"),
        tags$style(type="text/css", ".shiny-progress .bar { background-color: #FF0000; .opacity = 0.8;}
                                     .shiny-progress .progress { height:7px;}
                                     .shiny-progress .progress-text .progress-message { font-size: 100%;}")
      ),
      fluidRow(
      box(
        width=12,
        title = "File Upload", status = "primary", solidHeader = FALSE, collapsible = TRUE,
        selectInput('s_ftype', 'File Type:', db.l),
        fileInput('data_file', 'Choose File:', multiple = TRUE,
                  accept=c('.csv','text/csv', 'text/comma-separated-values','text/plain', '.txt'))
      ),
      box(
        width=12,
        title = "Settings", status = "primary", solidHeader = FALSE, collapsible = TRUE,
        selectInput('s_ntype', 'Node Type:', node.type.l),
        conditionalPanel(
          condition = "input.s_ntype == 'term'",
          # If using term node type, select the column to use to extract terms
          # [1] title, [2] abstract, [3] keyword
          selectInput('s_ttype', 'Term Type:', term.type.l),
          # Specify which weighting function to use for the term-document matrix if 
          # term selected as node.type.
          selectInput('s_tweight', 'Term Weight:', term.weight.l),
          # Specify the minimum term length to include in the matrix
          numericInput("n_tlength","Min. Term Length:",1, 1, 10, 1)
        ),
        selectInput('s_displaycols', 'Display Columns (All Results):', choices = display.cols.l, selected = display.cols.l,
                    multiple=TRUE),
        #selectInput('s_plot', 'Plot Results:', c('Yes', 'No')),
        conditionalPanel(
          condition = "input.s_plot == 'Yes'",
          selectInput('s_plotby', 'Plot by highest:', c('Betweenness', 'Degree')),
          numericInput("n_pcount","Number of Nodes to Plot:", 20, 1, 100, 1),
          selectInput('s_pedges', 'Plot All Edges:', c('Yes', 'No'))
        )
      )),
      # br(),
      actionButton("btn_analysis","Analysis")
    ),

    # Show a plot of the generated distribution
  dashboardBody(
      uiOutput("down_buttons"),
      HTML("<br>"),
      fluidRow(
        
        box(
          width=12,
          title = "Please upload your data and click on ANALYSIS - After the analysis bar is completed, use the menu on the top to download your results", status = "primary", solidHeader = FALSE, collapsible = TRUE,
          # HTML("Results Summary"),
          # dataTableOutput("t_result"),
          #uiOutput("add_button")
        ),
        
        
        
        # box(
       #   width=12,
      #    title = "All Results (Click Row to Select)", status = "primary", solidHeader = FALSE, collapsible = TRUE,
     # # HTML("Results Summary"),
      #    #dataTableOutput("t_result"),
      #    uiOutput("add_button")
    #    ),
      #  box(
        #  width=12,
       #   title = "Selected Row", status = "primary", solidHeader = FALSE, collapsible = TRUE,
   #   # HTML("<hr>"),
   #   # HTML("Selected Row"),
   #       dataTableOutput("t_selrow")
     #   ),
        box(
          width=12,
          title = "Selected Results", status = "primary", solidHeader = FALSE, collapsible = TRUE,
      # textOutput('rows_out'),
      # HTML("<hr>"),
      # HTML("Selected Results"),
          dataTableOutput("t_queue"),
          uiOutput("queue_buttons")
        ))
    )
  # )
))
