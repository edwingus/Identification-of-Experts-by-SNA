
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyServer(function(input, output) {
  #-------------------------------------------------------------------------------
  # When analysis button is clicked, upload and analyze data
  #-------------------------------------------------------------------------------
  observe({
    if (input$btn_analysis != 0){
      isolate({
        g_db <<- input$s_ftype
        # Load data form specified locations
        g_data <<- load.data(input$data_file$datapath, g_db)
        # Extract nodes (actors, terms, affiliations, categories) from uploaded data
        nodes <- extract.nodes(g_data, input$s_ftype, input$s_ntype, input$s_ttype)
        
        # Create the Node-Document matrix
        mat <- create.matrix(nodes, input$s_ntype, input$s_ftype, input$n_tlength, 
                                input$s_tweight)
        
        if (input$s_ntype == "actor") {
          actors <- mat$actors
          mat <- mat$mat
        }
        
        # Create SNA network
        net <- create.network(mat)
        
        g_edge.list <<- cbind(get.data.frame(net), "undirected")
        colnames(g_edge.list) <<- c("Source", "Target", "Weight", "Type")
        g_res.summary <<- summary.results(net)

        if (input$s_ntype == "actor") {
          node.results <- data.frame(Author = row.names(g_res.summary$node), g_res.summary$node)
          node.results <- merge(actors, node.results, by = "Author")
          g_res.summary$node <<- data.frame(ID = seq(1:nrow(g_res.summary$node)), node.results)
        }
        
        #         if (input$s_plot) {
        #           plot.results(res.summary$node, plot.by, plot.count, plot.edges.b)
        #         }
      })
    }
  })
  
  #-------------------------------------------------------------------------------
  # When add button is clicked, add to queue
  #-------------------------------------------------------------------------------
  observe({
    if (length(input$btn_add) != 0)
      if (input$btn_add != 0)
        isolate({
          g_queue$data <<- unique(rbind(g_queue$data, g_selected$row))
        })
  })
  
  #-------------------------------------------------------------------------------
  # When remove button is clicked, remove last from queue
  #-------------------------------------------------------------------------------
  observe({
    if (length(input$btn_remove) != 0)
      if (input$btn_remove != 0)
        isolate({
          g_queue$data <<- g_queue$data[-which(g_queue$data$ID == as.numeric(input$queue.rows[1])), ]
        })
  })
  
  #-------------------------------------------------------------------------------
  # When clear button is clicked, clear all from queue
  #-------------------------------------------------------------------------------
  observe({
    if (length(input$btn_clear) != 0)
      if (input$btn_clear != 0)
        isolate({
          g_queue$data <<- data.frame()
        })
  })
  
  #-------------------------------------------------------------------------------
  # When row selected from all results, update global queue dataframe
  #-------------------------------------------------------------------------------
  observe({
    if (!is.null(input$res.rows))
      g_selected$row <<- g_res.summary$node[which(g_res.summary$node$ID == as.numeric(input$res.rows[1])), ]
  })
  
  
  #-------------------------------------------------------------------------------
  # Shows download buttons
  #-------------------------------------------------------------------------------
  output$down_buttons <- renderUI({
    if (input$btn_analysis == 0)
      if (nrow(g_res.summary$node) == 0)
        return(NULL)

    list(downloadButton('btn_data', 'Download Raw Data'),
         downloadButton('btn_all.res', 'Download All Results'),
         downloadButton('btn_sel.res', 'Download Selected Results'),
         downloadButton('btn_edgelist', 'Download Edge List'))
  })
  
  #-------------------------------------------------------------------------------
  # Saves raw data to csv file when button pressed
  #-------------------------------------------------------------------------------
  output$btn_data <- downloadHandler(
    filename = function() {
      paste0('data_', Sys.Date(), '.csv')
    },
    content = function(file) {
      write.csv(g_data, file, row.names = FALSE)
    },
    contentType = "text/csv"
  ) 
  
  #-------------------------------------------------------------------------------
  # Saves all results to csv file when button pressed
  #-------------------------------------------------------------------------------
  output$btn_all.res <- downloadHandler(
    filename = function() {
      paste0('results_', Sys.Date(), '.csv')
    },
    content = function(file) {
      write.csv(cbind(g_res.summary$node), file, row.names = FALSE)
    },
    contentType = "text/csv"
  ) 
  
  #-------------------------------------------------------------------------------
  # Saves edge list to csv file when button pressed
  #-------------------------------------------------------------------------------
  output$btn_edgelist <- downloadHandler(
    filename = function() {
      paste0('edgelist_', Sys.Date(), '.csv')
    },
    content = function(file) {
      write.csv(g_edge.list, file, row.names = FALSE)
    },
    contentType = "text/csv"
  ) 
  
  #-------------------------------------------------------------------------------
  # Saves selected results to csv file when button pressed
  #-------------------------------------------------------------------------------
  output$btn_sel.res <- downloadHandler(
    filename = function() {
      paste0('sel_results', Sys.Date(), '.csv')
    },
    content = function(file) {
      write.csv(g_queue$data, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  #-------------------------------------------------------------------------------
  # Display results in a table
  #-------------------------------------------------------------------------------
  output$t_result <- renderDataTable({
    if (input$btn_analysis == 0)
      if (nrow(g_res.summary$node) == 0)
        return(NULL)
    
#     if (g_db == "wos")
#       r <- g_res.summary$node[,-which(colnames(g_res.summary$node) %in% "DOI")]
#     if (g_db == "com")
#       r <- g_res.summary$node[,-which(colnames(g_res.summary$node) %in% c("DOI", "C.Author", "Affiliation"))]
    if (g_db == "wos")  
      display.cols <- c("ID", "Author", "Article.ID", "Total.Citations", input$s_displaycols)
    if (g_db == "com")
      display.cols <- c("ID", "Author", "Article.ID", input$s_displaycols)

    r <- g_res.summary$node[, display.cols]
#     if (nrow(r) != 0) {
#       row_names <- rownames(r)
#       r <- cbind(row_names, r)
#       colnames(r)[1] <- "ID"
      return(r)
  }, options = list(searching=1, ordering=1, processing=0, 
                    lengthMenu = c(5, 10, 20, 30, 40), pageLength = 5, orderClasses = TRUE, info = TRUE,
                    stateSave = TRUE, pagingType = "simple_numbers"),
    callback = "function(table) {
                    table.on('click.dt', 'tr', function() {
                    $(this).closest('table').find('.selected').each(function(){
                      $(this).removeClass('selected');
                    });
                    $(this).toggleClass('selected');
                    Shiny.onInputChange('res.rows',
                      table.rows('.selected').data().toArray());
                    });
                }"
  )
  
#-------------------------------------------------------------------------------
# Display selected row data in a table
#-------------------------------------------------------------------------------
output$t_selrow <- renderDataTable({
  if (is.null(input$res.rows))
    return(NULL)
  
  sel.node <- g_res.summary$node[which(g_res.summary$node$ID == as.numeric(input$res.rows[1])), ]
  sel.articles <- as.numeric(unlist(strsplit(as.character(sel.node[, "Article.ID"]), split = "|", fixed = TRUE)))
  
  sel.cols <- switch(g_db,
                     "com" = c(1,2,3),
                     "wos" = c(1, 10, 24))
  
  g_selected$row <<- sel.node
  
  r <- g_data[which(g_data$ID %in% sel.articles), sel.cols]
  
#     if (nrow(g_res.summary$node) == 0)
#       return(NULL)
    return(r)
}, options = list(searching=0, ordering=1, processing=0, paging=1, info=0,
                  pagingType = "simple", lengthMenu = c(5, 10, 20), pageLength = 5))


#   output$rows_out <- renderText({
#     paste(c('You selected these rows on the page:', input$res.rows),
#           collapse = ' ')
#   })

#-------------------------------------------------------------------------------
# When add button is clicked, add selection to queue
#-------------------------------------------------------------------------------
output$t_queue <- renderDataTable({
  return(g_queue$data)
}, options = list(searching=0, ordering=1, processing=0, paging=1, info=0,
                  pagingType = "simple", lengthMenu = c(5, 10, 20), pageLength = 5),
   callback = "function(table) {
                    table.on('click.dt', 'tr', function() {
                    $(this).closest('table').find('.selected').each(function(){
                      $(this).removeClass('selected');
                    });
                    $(this).toggleClass('selected');
                    Shiny.onInputChange('queue.rows',
                      table.rows('.selected').data().toArray());
                    });
                }")


# output$rows_out <- renderText({
#   paste(c('You selected these rows on the page:', input$res.rows),
#         collapse = ' ')
# })

#-------------------------------------------------------------------------------
# Add add button for selected results
#-------------------------------------------------------------------------------
output$add_button <- renderUI({  
  if (input$btn_analysis == 0)
      return(NULL)
  
  list(actionButton("btn_add","Add to Queue"))
})

#-------------------------------------------------------------------------------
# Add remove and clear button for selected results
#-------------------------------------------------------------------------------
output$queue_buttons <- renderUI({  
  if (input$btn_analysis == 0)
    return(NULL)
  
  list(actionButton("btn_remove","Remove Selected"),
      actionButton("btn_clear","Clear Results"))
})

})
