#******************************************************************************
#
# server.R
# Contain the script that will run on the server when the user interacts 
# with the webpage interface. 
#
#******************************************************************************
shinyServer(function(input, output, session) {
  col_names = c("Article","Accession number","Title","Authors","Author affiliation",
                "Corresponding author","Source title","Abbreviated source title",
                "Volume","Issue","Issue date","Publication year","Pages",
                "Language","ISSN","CODEN","Document type","Abstract","Number of references",
                "Main heading","Controlled terms","Uncontrolled terms","Classification code",
                "Treatment","Database")
  
  dataset <- data.frame()
  author_data <- data.frame()
  sna_stats <- data.frame()
  keywords <- vector()
  sna_plot <- list()
  mat_graph <- array()
  del_count <- 0
  
  #-------------------------------------------------------------------------------
  # Disables all buttons during data upload and TFDEA/LR analysis, to prevents
  # user interaction
  #-------------------------------------------------------------------------------
  elem.disable <- function(id, disable_b = FALSE, class_b = FALSE) {
    if (class_b) msg_list <- paste0("$('.", id, "').prop('disabled',", tolower(disable_b), ")")
    else msg_list <- paste0("$('#", id, "').prop('disabled',", tolower(disable_b), ")")
    session$sendCustomMessage(type="jsCode", list (code = msg_list))
  }
  
  #-------------------------------------------------------------------------------
  # 
  #-------------------------------------------------------------------------------
  observe({
    if (input$btn_upload != 0){
      isolate({
#         debug(data.load)
        elem.disable("btn", TRUE, TRUE)
        withProgress(session, min = 1, max = 15, {
          setProgress(message = 'Calculation in progress',
                      detail = 'This may take a while...')
          data.load(format = input$sel_filetype)
        })
#         data.load() # Load data into global data_set dataframe
#         undebug(data.load)
        elem.disable("btn", FALSE, TRUE)
      })
      updateTabsetPanel(session, "display_tabset", selected = "dataset_tab")
    }
  }) # observe upload
  
  #-------------------------------------------------------------------------------
  # 
  #-------------------------------------------------------------------------------
  observe({
    if (input$btn_delete != 0 && nrow(dataset) != 0){
      isolate({
        elem.disable("btn", TRUE, TRUE)
        withProgress(session, min = 1, max = 15, {
          setProgress(message = 'Calculation in progress',
                      detail = 'This may take a while...')
          del_rows <- unlist(strsplit(input$num_delete, split = ",", fixed = TRUE))
          dataset <<- dataset[-as.numeric(del_rows), ]
          data.load(FALSE)
        })
        elem.disable("btn", FALSE, TRUE)
      })
    }
  }) # observe upload
  
  #-------------------------------------------------------------------------------
  # 
  #-------------------------------------------------------------------------------
  data.load <- function(load_data = TRUE, format = "com"){
    if (load_data)
      df <- load.dataframe(format)
    else
      df <- dataset
    
    if (names(df)[1] != "ERROR" && names(df)[1] != "MESSAGE")
      df[, 1] <- seq(1, nrow(df))
    
    # Initilaize dataframe that will contain all authors' data
    author_data <- data.frame()
    mat_2x      <- array()
        
    if (nrow(df) != 0)
      if (names(df)[1] != "ERROR" && names(df)[1] != "MESSAGE"){
        author_data <- load.authors(df)
#         print(system.time(load.authors(df)))
        mat_2x <- load.matrix(author_data[, 2], rownames(df), author_data[, 1]) 
    
        sna_graph <- graph.adjacency(mat_2x, weighted = TRUE, mode = "undirected")
        sna_graph <- simplify(sna_graph) 
        V(sna_graph)$degree <- degree(sna_graph)
        V(sna_graph)$size <- 5
        #       V(sna_plot)$label.cex <- 2.2 * V(sna_plot)$degree / max(V(sna_plot)$degree)+ .2
        edge_width <- (log(E(sna_graph)$weight) + 0.4) / max(log(E(sna_graph)$weight) + 0.4)
        E(sna_graph)$width <- edge_width
        updateNumericInput(session, "num_degree", "Minimum Degree Centrality to Plot:", 
                           value = max(degree(sna_graph))/2, 
                            min = 0, max = max(degree(sna_graph)), step = 1)
        keywds_c  <- unique(tolower(unlist(strsplit(df[which(!is.na(df[21])), 21], split = " - ", 
                                                    fixed = TRUE))))
        keywds_uc <- unique(tolower(unlist(strsplit(df[which(!is.na(df[22])), 22], split = " - ", 
                                                    fixed = TRUE))))
        
        sna_stats <<- data.frame(Nodes = length(V(sna_graph)), Edges = length(E(sna_graph)), 
                            Density = graph.density(sna_graph), Islands = clusters(sna_graph)$no,
                            Global_Cluster = transitivity(sna_graph, type="global"), 
                            Edge_Connectivity = edge.connectivity(sna_graph), 
                            Adhesion = graph.adhesion(sna_graph))
                             
        author_data <<- cbind(author_data, Degree = degree(sna_graph), Betweenness = betweenness(sna_graph), 
                              Closeness = closeness(sna_graph), EigenVector = evcent(sna_graph)$vector, 
                              Local_Cluster = transitivity(sna_graph, type = "local"))
        mat_graph <<- mat_2x
        sna_plot <<- sna_graph
        keywords <<- unique(c(keywds_c, keywds_uc))
      }
    dataset <<- df
  }
  
  #******************************************************************************
  # 
  #******************************************************************************
  load.dataframe <- function(format){
    in_file <- input$data_file
    # Determine whether a file has been chosen by the user. Message displayed to select a file
    if (is.null(in_file))
      return(data.frame(MESSAGE = "No File Chosen. Select a file using the 'Choose File' button"))
    # Read data from selected file
    
    if (format == "com"){
      table_data <- tryCatch(
        read.table(in_file$datapath, sep = "\n", fill = FALSE, strip.white = TRUE, quote = "", comment.char = "",
                  header = FALSE, stringsAsFactors = FALSE, blank.lines.skip = TRUE, colClasses = "character"),
        error = function(e)
          return(data.frame(ERROR = paste("<b style='color:red'>",toupper(conditionMessage(e)),"</b>",sep="")))
      )
      # Find row numbers for the start of each record
      record_count <- grep("<RECORD", unlist(table_data), fixed = TRUE)
      # Initilaize dataframe that will contain all record (article) data
      df <- data.frame(matrix(NA, nrow = 1, ncol = length(col_names)), stringsAsFactors = FALSE)
      names(df) <- col_names
#     row.names = paste("Article_", seq(length(record_count)), sep = "")
    
      k <- 1
      # Iterate for each record (article) and load data into a dataframe (df)
      for (i in 1:length(record_count)){
        start_rec <- record_count[i]
        end_rec   <- ifelse(!is.na(record_count[i+1]), record_count[i+1], nrow(table_data))
        # Select subset that contains record i data
        split_rows <- strsplit(table_data[start_rec:(end_rec - 1),], "\n", fixed = TRUE)  # Split article entry by row
        split_entry <- strsplit(unlist(split_rows), ":", fixed = TRUE) # Split row by name and value
        names <- sapply(split_entry, "[", 1)  # Extract name from file
        values <- sapply(split_entry, "[", 2) # Extract value from file
        col_ind <- match(col_names, names)    # Match names extracted from file with column names to get col index
        if (length(input$sel_dup) == 0) {
          df[k, ]  <- values[col_ind]     # Set article values
          df[k, 1] <- k                   # Set article number
          k <- k + 1
        }
        else {
          match_ind <- match(input$sel_dup[1], names) # Find column index in extracted names to look for duplicates
          # Look if there are any matches for each column specified by col_check
          dup_row <- match(tolower(values[match_ind]), tolower(df[,input$sel_dup[1]]))
          if (!is.na(dup_row)) { 
            matches <- match(tolower(values[match(input$sel_dup, names)]), tolower(df[dup_row,input$sel_dup]))
            if (sum(any(matches, na.rm = TRUE)) == length(input$sel_dup)) {
              blank_data <- names(df)[which(is.na(df[dup_row, ]))]
              match_ind <- match(blank_data, names)
              df[dup_row, blank_data]  <- values[match_ind]        # Set article values
            }
            else { 
              df[k, ]  <- values[col_ind]     # Set article values
              df[k, 1] <- k                   # Set article number
              k <- k + 1
            }
          }
          else {
            df[k, ]  <- values[col_ind]     # Set article values
            df[k, 1] <- k                   # Set article number
            k <- k + 1
          }
        }
      }
    }
    else {
      df <- tryCatch(
                    read.csv(in_file$datapath, sep = ",", quote = '"', stringsAsFactors = FALSE, 
                             colClasses = "character", header = TRUE, row.names = 1),
                    error = function(e)
                    return(data.frame(ERROR = paste("<b style='color:red'>",
                                                    toupper(conditionMessage(e)), "</b>",sep="")))
      )
    }
    
    if (input$usa_auth_b){
      matched_usa <- union(grep("United States", df[, 5]), grep("USA", df[, 5]))
      df <- df[matched_usa, ]
    }
    return(unique(df))
  }
  
  #******************************************************************************
  # 
  #******************************************************************************
  load.authors <- function(df){
    author_data <- data.frame()
    # Iterate for each row of dataframe and extract authors, co-author email, and affiliations  
    for (i in 1:nrow(df)){
      #----Gather Author Info----#
#       names <- sapply(strsplit(V(g)$name, " "), "[",1)
      authors_split <- gsub(";\\S", "", df[i, 4]) # Remove semicolons not followed by a space
      authors_split <- unlist(strsplit(authors_split, ";", fixed = TRUE)) # Split the string into each author
      authors_split <- authors_split[which(!is.na(authors_split))] # Remove all NA's
      authors_split <- authors_split[which(gsub(" {2,}", "", authors_split) != "")] # Remove all blank authors
      if (length(authors_split) > 0){
        affl_ind <- regexec("[(][0-9]+[)]$", authors_split) # Find the authors affiliation number
        # If authors affiliation number not found, -1 is returned. Replace with NA and extract number
        auth_affl <- substring(authors_split, as.numeric(replace(affl_ind, affl_ind == -1, NA)) + 1, 
                               nchar(authors_split) - 1)
        
#         if (length(auth_affl) == 0L)
#           auth_affl <- rep(NA, length(authors_split))
        
        auth_affl[which(auth_affl == 0)] <- NA
        # Remove the number and brackets after each author name
        authors_split <- gsub("\\s[(][0-9]+[)]$", "", authors_split)
        # Iterate for each author
        for (j in 1:length(authors_split)){
          if (length(authors_split[j]) > 0){
            # For authors, split the last and first names
            split_author <- unlist(strsplit(authors_split[j], ", ", fixed = TRUE))
            lname <- split_author[1]
            lname_inits <- lname
            if (length(split_author) > 1){
              fname_str <- split_author[2]
              sub_fname <- gsub(" {2,}", " ", gsub("\\.", " ", fname_str))
              split_fname <- unlist(strsplit(sub_fname," ", fixed = TRUE))
              # Extract initials from first names
              initials <- paste(substring(split_fname, 1, 1), collapse = ".")
              initials <- paste0(initials, ".")
              # Combine last name with initials
              lname_inits <- gsub(" ", "", paste(lname, initials, sep = ","), fixed = TRUE)
            }
            art_refs <- ifelse(is.na(df[i, 19]), 0, df[i, 19])
            # Add author as a row to author_data dataframe
            # author_data dataframe (Article Number, Author's full name, Author's lastname with initials, Author's email address)
            row_data <- data.frame(i, authors_split[j], lname_inits, auth_affl[j], NA, art_refs, art_refs, 1, stringsAsFactors = FALSE)
            author_data <- rbind(author_data, row_data) 
          }
        }
      }
      
      #----Gather Author Affiliation Info----#
      affiliations <- gsub(";\\S", "", df[i, 5])
      affiliations <- gsub("&amp;", "&", df[i, 5], fixed = TRUE)
      split_affl <- unlist(strsplit(affiliations, ";", fixed = TRUE))
      split_affl <- gsub("[(][0-9]+[)]\\s", "", split_affl)

      
      outbound_ind <- ifelse(author_data[, 1] == i, ifelse(author_data[, 4] > length(split_affl), TRUE, FALSE), FALSE)
      outbound_ind[is.na(outbound_ind)] <- FALSE
      author_data[outbound_ind, 4] <- NA
      for (j in 1:length(split_affl)) {
        affl_index <- ifelse(author_data[, 1] == i, ifelse(author_data[, 4] == j, TRUE, FALSE), FALSE)
        affl_index[is.na(affl_index)] <- FALSE
        author_data[affl_index, 4] <- split_affl[j]
      }
      
      #----Gather Corresponding Author Info----#
      split_cr_author <- unlist(strsplit(df[i, 6], "(", fixed = TRUE)) # Split Co-Author string. String format: Name (email)
      # Find matching author using full name in author_data
      cr_author <- tolower(gsub(" {2,}", "", split_cr_author[1]))
      match_row <- grep(cr_author, tolower(author_data[, 2]), fixed = TRUE)
      # If not found, find matching author using lastname and initial in author_data
      if (length(match_row) == 0)
        match_row <- grep(cr_author, tolower(author_data[, 3]), fixed = TRUE)
      # If not found, find first author of associated article
      if (length(match_row) == 0)
        match_row <- which(df[i, 1] == author_data[, 1])[1]
      
      # If email address exists, save email address to author_data
      if (length(split_cr_author) > 1){
        email <- substring(split_cr_author[2], 1, nchar(split_cr_author[2]) - 1) # Remove bracket at end of email address
        author_data[match_row, 5] <- email
      }
    }
#     author_data <- unique(author_data) # Remove duplicate entries
    
    # Iterate for each author entry to combine authors by option select by user
    for (i in 1:nrow(author_data)){
      data_table <- author_data
      data_table[i, 2:5] <- NA
      matched_fulln <- grep(author_data[i, 2], data_table[, 2], fixed = TRUE)
      matched_initn <- grep(author_data[i, 3], data_table[, 3], fixed = TRUE)
      matched_affl <- grep(author_data[i, 4], data_table[, 4], fixed = TRUE)
#       matched_affl <- agrep(author_data[i, 4], data_table[, 4], fixed = TRUE, max.distance = 0.5)
      matched_email <- grep(author_data[i, 5], data_table[, 5], fixed = TRUE)
      matched_fulln <- matched_fulln[!is.na(matched_fulln)] 
      matched_initn <- matched_initn[!is.na(matched_initn)] 
      matched_affl <- matched_affl[!is.na(matched_affl)] 
      matched_email <- matched_email[!is.na(matched_email)] 
      # Select which approach to combine authors
      switch(input$sel_group,
             all = { matched <- union(intersect(matched_initn, matched_affl), intersect(matched_initn, matched_email)) 
                     matched <- union(matched, matched_fulln) },
             full_both = { matched <- union(intersect(matched_fulln, matched_affl), intersect(matched_fulln, matched_email)) },
             initn_both = { matched <- union(intersect(matched_initn, matched_affl), intersect(matched_initn, matched_email)) },
             initn_affl = { matched <- intersect(matched_initn, matched_affl) },
             initn_email = { matched <- intersect(matched_initn, matched_email) },
             full_name = { matched <- matched_fulln },
             init_name = { matched <- matched_initn },
             none = { matched <- integer(0) }
      )
      if (length(matched) > 0){
        articles <- unique(c(unlist(strsplit(as.character(author_data[i, 1]), split = "; ", fixed = TRUE)), 
                             unlist(strsplit(as.character(author_data[matched, 1]), split = "; ", fixed = TRUE))))
        author_data[i, 1] <- paste(articles, collapse = "; ")
        # Determine which authors matched the selected grouping option (sel_group) above without the same affiliation
        diff_affl <- intersect(matched, (1:nrow(author_data))[-matched_affl])
        diff_affl <- intersect(diff_affl, which(!is.na(author_data[, 4])))
        author_data[i, 4] <- paste(unique(c(author_data[i, 4], author_data[diff_affl, 4])), collapse = "; ")
        diff_email <- intersect(matched, (1:nrow(author_data))[-matched_email])
        diff_email <- intersect(diff_email, which(!is.na(author_data[, 5])))
        author_data[i, 5] <- paste(unique(c(author_data[i, 5], author_data[diff_email, 5])), collapse = "; ")
#         diff_ref <- intersect(matched, which(!is.na(author_data[, 6])))
        author_data[i, 6] <- paste(c(author_data[i, 6], author_data[matched, 6]), collapse = "; ")
        author_data[i, 7] <- sum(as.numeric(unlist(strsplit(author_data[i, 6], split="; ", fixed = TRUE))), na.rm = TRUE)
        author_data[i, 8] <- length(articles)
        author_data <- author_data[-matched, ]
      }
    }
#     author_data[which(author_data[, 4] == "NA"), 4] <- NA
    names(author_data) <- c("Articles", "Author_full", "Author_Initials", "Author affiliation", "Email", "References", "Total References", "Publications")
    if (input$usa_auth_b){
      matched_usa <- union(grep("United States", author_data[, 4]), grep("USA", author_data[, 4]))
      author_data <- author_data[matched_usa, ]
      author_data <- author_data[order(author_data[, 2]), ]
    }
    
    return(author_data)
  }
    
  #******************************************************************************
  # 
  #******************************************************************************
  load.matrix <- function(mat_x, mat_y, mat_weight, weight_sep = "; "){
    # Two nodes are called adjacent if they are connected by an edge.
    # Two edges are called incident, if they share a node.
    mat_xy   <- array(0, c(length(mat_x),length(mat_y)), dimnames = list(mat_x, mat_y)) #incident matrix
    mat_2x   <- array(0, c(length(mat_x),length(mat_x)), dimnames = list(mat_x, mat_x)) #adjacency matrix
    
    # Create Author/Article Matrix (Incident matrix)
    for (i in 1:nrow(mat_xy)){
      split_ind <- as.numeric(unlist(strsplit(as.character(mat_weight[i]), split = "; ", fixed = TRUE)))
      mat_xy[i, split_ind] <- 1
    }
    mat_2x <- mat_xy %*% t(mat_xy)
    return(mat_2x)
  }  
  
  #******************************************************************************
  # 
  #******************************************************************************
  output$sna_plot <- renderPlot({
    del_count <<- input$btn_delete
    if (nrow(mat_graph) > 1 && input$btn_upload != 0){
      if (input$sna_label_b)
        V(sna_plot)$label <- V(sna_plot)$name
      else
        V(sna_plot)$name <- rep("", vcount(sna_plot))
      sna_plot <- delete.vertices(sna_plot, V(sna_plot)[degree(sna_plot) < input$num_degree])
      set.seed(3952)
      plot(sna_plot, layout = get(input$graph_layout))
    }
  })
  
  #******************************************************************************
  # 
  #******************************************************************************
  output$result_plot <- renderPlot({
    del_count <<- input$btn_delete
    if (input$btn_upload != 0 && nrow(author_data) != 0){
      x_label <- ifelse(input$sel_result == "Local_Cluster", input$sel_result, 
                        paste(input$sel_result,"Centrality")) 
      hist(author_data[,input$sel_result], col = "lightblue", xlab = x_label, 
           main = "Histogram of Centrality Measurement")
    }
  })
  
  #******************************************************************************
  # 
  #******************************************************************************
  output$dt_alldata <- renderChart2({
    # If the display data button has not been pressed, return null
    data_display <- data.frame(Data = "")
    del_count <<- input$btn_delete
    if (input$btn_upload == 0 || nrow(dataset) == 0)
      return(dTable(data_display, bPaginate = FALSE, bFilter = FALSE, bInfo =  FALSE,
                    aoColumnDefs = list(list(bSortable = FALSE, aTargets = list("_all")))))
    if (colnames(dataset)[1] == "ERROR" || colnames(dataset)[1] == "MESSAGE")
      return(dTable(dataset, sPaginationType = 'two_button', bPaginate = FALSE, bFilter = FALSE, bInfo =  FALSE))
    data_table <- dTable(dataset[c(1,3,8:11)], sPaginationType = 'full_numbers', bProcessing = TRUE)
    data_table$set(dom = 'dataset_table')
    return(data_table)
  }) # End dt_alldata
  
  #******************************************************************************
  # 
  #******************************************************************************
  output$dt_authors <- renderChart2({
    # If the display data button has not been pressed, return null
    data_display <- data.frame(Data = "")
    del_count <<- input$btn_delete
    if (input$btn_upload == 0 || nrow(author_data) == 0)
      return(dTable(data_display, bPaginate = FALSE, bFilter = FALSE, bInfo =  FALSE,
                    aoColumnDefs = list(list(bSortable = FALSE, aTargets = list("_all")))))
    
    data_display <- cbind(Row = seq(1,nrow(author_data)), author_data[-c(3:5)])
    data_table <- dTable(data_display, sPaginationType = 'full_numbers', bProcessing = TRUE)
    data_table$set(dom = 'authors_table')
    return(data_table)
  }) # End dt_authors
  
  #******************************************************************************
  # 
  #******************************************************************************
  output$dt_gstats <- renderChart2({
    # If the display data button has not been pressed, return null
    data_display <- data.frame(SNA.Summary = "")
    del_count <<- input$btn_delete
    if (input$btn_upload == 0 || nrow(sna_stats) == 0)
      return(dTable(data_display, bPaginate = FALSE, bFilter = FALSE, bInfo =  FALSE,
                    aoColumnDefs = list(list(bSortable = FALSE, aTargets = list("_all")))))
    
    data_table <- dTable(sna_stats, bPaginate = FALSE, bFilter = FALSE, bInfo =  FALSE,
                         aoColumnDefs = list(list(bSortable = FALSE, aTargets = list("_all"))))
    data_table$set(dom = 'stats_table')
    return(data_table)
  }) # End dt_authors
    
  #******************************************************************************
  # 
  #******************************************************************************
#   output$dt_sna <- renderChart2({
#     # If the display data button has not been pressed, return null
#     data_display <- data.frame(Data = "")
#     if (input$btn_upload == 0 || nrow(sna_result) == 0)
#       return(dTable(data_display, sPaginationType = 'two_button', bPaginate = FALSE, bFilter = FALSE, bInfo =  FALSE))
#     
#     data_table <- dTable(sna_result, sPaginationType = 'full_numbers', bProcessing = TRUE)
#     data_table$set(dom = 'sna_table')
#     return(data_table)
#   }) # End dt_sna
    
  #******************************************************************************
  # 
  #******************************************************************************
  output$sel_dupicate <- renderUI({
    selectInput('sel_dup', 'Remove Duplicates By (Optional):', col_names[-1], multiple = TRUE)  
  })
  
  #******************************************************************************
  # 
  #******************************************************************************
  output$delete_article <- renderUI({
    if (input$btn_upload != 0 && nrow(dataset) != 0)
      if (names(dataset)[1] != "ERROR" && names(dataset)[1] != "MESSAGE"){
        list(textInput('num_delete', 'Article Number(s) (seperate by comma):'), br(),
             actionButton('btn_delete', 'Delete Articles')) 
      }
  })
  
  #******************************************************************************
  # 
  #******************************************************************************
  output$btn_datadown <- renderUI({
    if (input$btn_upload != 0 && nrow(dataset) != 0)
      if (colnames(dataset)[1] != "ERROR" && colnames(dataset)[1] != "MESSAGE"){
         list(downloadLink('btn_all', 'Download All Results (xls)'), br(),
              downloadLink('btn_table', 'Download Dataset (csv)'), br(),
              downloadLink('btn_matrix', 'Download Matrix (csv)'), br(),
              downloadLink('btn_authors', 'Download Authors (csv)'), br(),
              downloadLink('btn_sna', 'Download SNA Summary (csv)'), br(),
              downloadLink('btn_keywords', 'Download Keywords (csv)'))    
      }
  })
  
  output$btn_all <- downloadHandler(
    filename = function() {
      paste('data_all-', Sys.Date(), '.xls', sep='')
    },
    content = function(file) {
      result_list <- list(Data = dataset, Authors = author_data,
                          Keywords = as.data.frame(keywords), SNA_Summary = sna_stats)
      print(lapply(result_list, is.data.frame))
#       write.csv(dataset, file)
      WriteXLS('result_list', file, row.names = FALSE, AdjWidth = TRUE, 
               BoldHeaderRow = TRUE, verbose = TRUE)
      #       write.csv(unlist(lr_result), file)
    },
    contentType = "application/vnd.ms-excel"
  )
  
  #******************************************************************************
  # 
  #******************************************************************************
  output$btn_table <- downloadHandler(
    filename = function() {
      paste('dataset-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(dataset, file)
    }
  )
  
  #******************************************************************************
  # 
  #******************************************************************************
  output$btn_authors <- downloadHandler(
    filename = function() {
      paste('authors-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(author_data, file)
    }
  )
  
  #******************************************************************************
  # 
  #******************************************************************************
  output$btn_matrix <- downloadHandler(
    filename = function() {
      paste('matrix-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(mat_graph, file)
    }
  )
  
  #******************************************************************************
  # 
  #******************************************************************************
  output$btn_sna <- downloadHandler(
    filename = function() {
      paste('sna_stats-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(sna_stats, file)
    }
  )
  
  #******************************************************************************
  # 
  #******************************************************************************
  output$btn_keywords <- downloadHandler(
    filename = function() {
      paste('keywords-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(keywords, file)
    }
  )
  
})