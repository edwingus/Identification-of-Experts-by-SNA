#******************************************************************************
#
# ui.R
# Generates the webpage interface for interaction with the user. The page is 
# separated into two main panels, the left sidebar panel for entering setup 
# information and the right main panel for displaying the results.
#
#******************************************************************************
shinyUI(pageWithSidebar(
  headerPanel("Social Network Analysis - Expert Identification"),
  sidebarPanel(
     tags$head(
#        tags$style(type='text/css', "#snaplot { border: 1px solid black;}")
#        tags$style(type='text/css', ".datatables table td { height:20px; }")
       tags$script(HTML("Shiny.addCustomMessageHandler('jsCode', function(message) {eval(message.code);});")),
       tags$style(type='text/css', ".datatables table { text-align: center; border: 1px solid black; float:centre; }"),
       tags$style(type='text/css', "#dt_gstats { height : 70px; }")
     ),
    progressInit(),
    tabsetPanel(id="setup_tabset",
                tabPanel("FILE UPLOAD",value="up_down_tab",
                         selectInput('sel_filetype', 'Select File Format:', list('Compendex (txt)' = 'com', 'Previous (csv)' = 'csv')),
                         fileInput('data_file', 'Choose File:',
                                   accept=c('.csv','text/csv', 'text/comma-separated-values','text/plain')),
                         selectInput('sel_group', 'Combine Authors By:', list('(2) or (5)' = 'all',
                                                                              '(1) Full Name and Email or Affiliation' = 'full_both', 
                                                                              '(2) Surname, Initials and Email or Affiliation' = 'initn_both',
                                                                              '(3) Surname, Initials and Affiliation' = 'initn_affl', 
                                                                              '(4) Surname, Initials and Email' = 'initn_email',
                                                                              '(5) Full Name' = 'full_name', 
                                                                              '(6) Surname and Initials' = 'init_name',
                                                                              'None' = 'none'), 'all', multiple = FALSE),
                         uiOutput("sel_dupicate"),
                         HTML("<i><b>NOTE 1:</b> 'Combine Author By' is used to find the same author by the selected criteria, since the names 
                              supplied by Compendex may differ.</i>"),
                         br(),
                         HTML("<i><b>NOTE 2:</b> 'Remove Duplicates By' is used to remove duplicated articles (i.e. selecting Title and 
                                Volume will remove article(s) with the same title and volume, only leaving one.)</i>"),
                         br(),br(),
                         checkboxInput('usa_auth_b', 'Only USA Authors', FALSE),
                         br(),
                         actionButton("btn_upload","Upload"),
                         br(), br(),
                         uiOutput("delete_article")
                         )
    )
  ),
  mainPanel( 
    tabsetPanel(id="display_tabset",
#                 tabPanel("Dataset", value="dataset_tab",
#                          h5("DATASET"),
#                          br(),
#                          chartOutput('dt_alldata', 'datatables'),
#                          br()
#                 ),
#                 tabPanel("Authors", value="authors_tab",
#                          h5("AUTHORS"),
# #                          uiOutput("btn_datadown"),
#                          chartOutput('dt_authors', 'datatables'),
#                          br()
#                 ),
#                 tabPanel("SNA Plot", value="snaplot_tab",
#                          h5("SNA PLOT OUTPUT"),
#                          div(class='row-fluid',
#                              div(class="span3", numericInput("num_degree", "Minimum Degree Centrality to Plot:", 0, min = 0, step = 1)),
#                              div(class="span1"),
#                              div(class="span3", selectInput('graph_layout', 'Select Method for Generating Layout:', 
#                                                             list(Auto = "layout.auto", Random = "layout.random", 
#                                                                  Circle = "layout.circle", Fruchterman_Reingold = "layout.fruchterman.reingold",
#                                                                  Kamada_Kawai = "layout.kamada.kawai", Spring = "layout.spring",
#                                                                  Reingold_Tilford = "layout.reingold.tilford",
#                                                                  DrL = "layout.drl"), "Fruchterman_Reingold"))
#                          ),
#                          checkboxInput('sna_label_b', 'Show Vertex Labels', TRUE),
#                          HTML("<i><b>NOTE:</b> To save plot - right click SNA plot, then click Save Image As...</i>"),
#                          plotOutput("sna_plot", height = "600px", width = "600px"),
#                          br()
#                 ),
                tabPanel("SNA Summary", value="snaresult_tab",
                      h5("SNA Graph Stats"),
                      chartOutput("dt_gstats", "datatables"),
                      HTML("<i><b>Density:</b> Sum of existing ties divided by the number of all possible ties.</i>"),
                         
                      selectInput('sel_result', 'Select Plot:', list("Degree", "Betweenness", "Closeness", "EigenVector",
                                                                     "Local Cluster Coefficient" = "Local_Cluster")),
                      plotOutput("result_plot")
                ),
                tabPanel("Download Results", value="download_tab",
                  h5("DOWNLOAD RESULTS"),
                  uiOutput("btn_datadown")
                )
    )
  )
))
  