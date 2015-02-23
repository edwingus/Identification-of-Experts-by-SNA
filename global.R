library(shiny)

# ------------------- LOAD LIBRARIES ------------------------#
packages <- c("tm", "igraph", "stringr", "shiny", "devtools")
if (length(setdiff(packages, installed.packages())) > 0)
  install.packages(setdiff(packages, installed.packages()))

if (length(setdiff("shinydashboard", installed.packages())) > 0)
  devtools::install_github("rstudio/shinydashboard")

# devtools required for shinydashboard
library(devtools)
# shinydashboard package required for the layout
library(shinydashboard)
# tm package required for creating term-document matrix
library(tm)
# igraph package required for creating sna plot
library(igraph)
# stringr package used for trimming text
library(stringr)
# shiny package required for creating the website
library(shiny)


source("db.settings.R", local=TRUE)$value

# ------------------ SET GLOBAL VARS ------------------------#
# Data can currently only be read from web of science (wos) 
# or Compendex (com) database
db.l <- list('Web of Science' = 'wos', Compendex = 'com', Patent = 'pat')

# The node for the network can either be an author (actor) or a term
node.type.l <- c(Actor = 'actor')
                 # , "term", "affl", "category")

# term can be extracted from either the title, the abstract, 
# or author keywords
term.type.l <- c("title", "abstract", "keyword")

# character seperating node data
sep.l <- c("", " ", ",", ";", "\t", "-")

# weighting function to use for term-document matrix
term.weight.l <- c('nnn', 'ntc')

display.cols.l <- c('Publications', 'Degree', 'Betweenness', 'Closeness', 'EigenVector', 
                    'Local.Cluster')

# ------------------ GLOBAL FUNCTIONS ------------------------#
g_initialize <- function() {
  # Uploaded data
  g_data <<- data.frame()
  
  # Matrix and Actor data
  g_mat <<- list()
  
  # Edge List
  g_edge.list <<- data.frame()
  
  # Summary of Results
  g_res.summary <<- list(node=data.frame(),
                        net=data.frame())
  
  g_selected <<- reactiveValues(row = data.frame())
  
  g_queue <<- reactiveValues(sum.data = data.frame(),
                            all.data = data.frame())
  
  # The source of the data (database)
  g_db <<- NA
}

g_initialize()

# Load Data from the Files Selected
load.data <- function(file.loc, db) {
  if (length(file.loc) == 0)
    stop("No files selected. Make sure to select a file and press open.")
  # file.loc <- readline(cat("Enter the location of your Web of Science or Compendex csv file:\n"))
  # Determine number of Web of Science Files
  # wos.files <- which(sapply(file.loc, function(x) substr(x, nchar(x)-2, nchar(x)) == "txt"))
  # Determine number of Compendex Files
  # com.files <- which(sapply(file.loc, function(x) substr(x, nchar(x)-2, nchar(x)) == "csv"))
  # Only allow the selection of flies from one database, not both
  # if (length(wos.files) > 0 && length(com.files) > 0)
  #  stop("Select either Web of Science or Compendex Files, not both")
  
  # if (length(wos.files) > 0)
    # for (i in wos.files) {
  data <- switch(db,
      'wos' = {
        # data <- data.frame()
        tryCatch(data.tmp <- read.table(file.loc, sep="\t", quote="", header=T, encoding = "UTF-8", na.string = NA, 
                                        comment.char = "", fill = TRUE, blank.lines.skip = TRUE, row.names=NULL, 
                                        stringsAsFactors = FALSE),
                error = function(e) stop("Error opening/reading txt file. Ensure Web Of Science file downloaded according to description: ", e))
        c.names <- colnames(data.tmp)
        c.au <- match("AU", c.names)
        if (!is.na(c.au)) {
          colnames(data.tmp) <- c("PT", colnames(data.tmp)[-1:-(c.au-1)])
        } else
          stop("Author Column not part of Data. Ensure correct and complete data downloaded from WOS")
        # rbind(data, data.tmp)
        data.tmp
      },
  
  # if (length(com.files) > 0)
    # for (i in com.files) {
      # Read Web of Science (wos) or Compendex(com) file. If error occurs reading
      # the file, return error
      'com' = {
        tryCatch(data.tmp <- read.csv(file.loc, stringsAsFactors = FALSE),
                error = function(e) stop("Error opening/reading csv file. Ensure Compendex file downloaded according to description: ", e))
        # rbind(data, data.tmp) 
        data.tmp
      },
      "pat" = {
        tryCatch(data.tmp <- read.csv(file.loc, stringsAsFactors = FALSE),
                 error = function(e) stop("Error opening/reading csv file. Ensure Compendex file downloaded according to description: ", e))
        # rbind(data, data.tmp) 
        data.tmp
      }
  )
  data <- unique(data)
  data <- data.frame(ID = seq(1,nrow(data)), data)
  return(data)
}

# Extract Relevant Node Data
extract.nodes <- function(data, db = "wos", node.type = "actor", 
                          term.type = "title", stem = FALSE) {
  # Determine which column from the data to select, specify the 
  # character seperating the nodes, and specify a regular expression
  # for removing unwanted characters
#            if (node.type == "actor")
#              node.type <- "actor.wos"

  node.settings <- db.settings(db, node.type)
#            if (node.type == "actor")
#              node.type <- "actor.com"
  # Extract actors or terms from specified columns of data
  nodes <- switch(node.type,
                  term = {
                    # Create corpus using specified data and clean up terms
                    nodes.tmp <- Corpus(VectorSource(data[, node.settings$col]))
                    nodes.tmp <- tm_map(nodes.tmp, tolower)
                    nodes.tmp <- tm_map(nodes.tmp, removePunctuation)
                    nodes.tmp <- tm_map(nodes.tmp, removeNumbers)
                    nodes.tmp <- tm_map(nodes.tmp, removeWords, stopwords('english'))
                    removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
                    nodes.tmp <- tm_map(nodes.tmp, removeURL)
                    nodes.tmp <- tm_map(nodes.tmp, stripWhitespace)
#                     if (stem) {
#                       nodes.tmp.s <- tm_map(nodes.tmp, stemDocument)
#                       nodes.tmp <- tm_map(nodes.tmp.s, stemCompletion, dictionary=nodes.tmp)
#                     }
                    
                    tm_map(nodes.tmp, PlainTextDocument)
                  },
                  actor = {
                    # Split the nodes by the specified character
                    actors.upper <- toupper(data[, node.settings$col[1]])
                    actors.tmp <- strsplit(actors.upper, split = node.settings$sep[1])
                    nodes.tmp <- list()
                    list.ind <- 1
                    
                    switch(db, 
                      'com' = {
                        # Determine each actors affiliation from number in brackets next to name
                        actor.affl <- lapply(actors.tmp, function(x) 
                        as.numeric(sapply(strsplit(gsub(node.settings$affl, '\\1', x), split = ","), function(y) y[[1]])) + 1)
                        # Split the affiliations by the numbers
                        affl.tmp <- strsplit(data[, node.settings$col[2]], split = node.settings$sep[2])
                      
                        for (i in 1:length(actor.affl)) {
                        actors.tmp.i <- gsub(node.settings$rem, "", str_trim(actors.tmp[[i]]))
                        # Change first names to initials
                        actors.tmp.i <-sapply(strsplit(actors.tmp.i, split = ","), 
                                              function(x) {
                                                l.name <- str_trim(x[1])
                                                if (length(x) > 1) {
                                                  f.name <- strsplit(str_trim(x[2]), split = "\\s+")
                                                  f.name <- paste(sapply(f.name, function(y) gsub(node.settings$f.name, "\\1",y)), collapse = ".")
                                                  return(paste(l.name, f.name, sep = ","))
                                                }
                                                if (length(x) == 0 || tolower(l.name) == "anon")
                                                  return(NA)                                        
                                                return(l.name)
                                              })       
                        affl.tmp.i <- gsub(node.settings$rem, "", str_trim(affl.tmp[[i]][actor.affl[[i]]]))
                        if (length(affl.tmp.i) == 0)
                          affl.tmp.i <- NA
                        
                        if (length(actors.tmp.i) == 0)
                          actors.tmp.i <- NA
                        if (!is.na(actors.tmp.i[1])) {
                          nodes.tmp[[list.ind]] <- list(Actor = actors.tmp.i, Affiliation = affl.tmp.i, 
                                                        Doc.info = data[i, node.settings$col[3:length(node.settings$col)]])
                          list.ind <- list.ind + 1
                        }          
                        }
                      },
                      'wos' = {
                        for (i in 1:length(actors.tmp)) {
                          actors.tmp.i <- str_trim(actors.tmp[[i]])                    
                          if (length(actors.tmp.i) == 0)
                            actors.tmp.i <- NA
                          if (!is.na(actors.tmp.i[1])) {
                            nodes.tmp[[list.ind]] <- list(Actor = actors.tmp.i, 
                                                          Doc.info = data[i, node.settings$col[2:length(node.settings$col)]])
                            list.ind <- list.ind + 1
                          }
                        }
                      },
                      'pat' = {
                        # Determine each actors affiliation from info in brackets next to name
                        affl.tmp <- lapply(actors.tmp, function(x) gsub(node.settings$affl, '\\1', x))
                        
                        for (i in 1:length(actors.tmp)) {
                          actors.tmp.i <- gsub(node.settings$rem, "", str_trim(actors.tmp[[i]]))
                          # Change first names to initials
                          actors.tmp.i <-sapply(strsplit(actors.tmp.i, split = ","), 
                                                function(x) {
                                                  l.name <- str_trim(x[1])
                                                  if (length(x) > 1) {
                                                    f.name <- strsplit(str_trim(x[2]), split = "\\s+")
                                                    f.name <- paste(sapply(f.name, function(y) gsub(node.settings$f.name, "\\1", y)), collapse = ".")
                                                    return(paste(l.name, f.name, sep = ","))
                                                  }
                                                  if (length(x) == 0 || tolower(l.name) == "anon")
                                                    return(NA)                                        
                                                  return(l.name)
                                                })       
                          affl.tmp.i <- gsub(node.settings$rem, "", str_trim(affl.tmp[[i]]))
                          if (length(affl.tmp.i) == 0)
                            affl.tmp.i <- NA
                          
                          if (length(actors.tmp.i) == 0)
                            actors.tmp.i <- NA
                          if (!is.na(actors.tmp.i[1])) {
                            nodes.tmp[[list.ind]] <- list(Actor = actors.tmp.i, Affiliation = affl.tmp.i, 
                                                          Doc.info = data[i, node.settings$col[2:length(node.settings$col)]])
                            list.ind <- list.ind + 1
                          }          
                        }
                      })    
                    nodes.tmp
                  },
                  category = {
                    # nodes.tmp <- gsub(node.reg, "", data[, node.settings$col]) 
                    # Split the nodes by the specified character
                    nodes.tmp <- strsplit(nodes.tmp, split = node.settings$sep[3])
                    # Remove additional characters using the specified regular expression
                    lapply(nodes.tmp, function(x) gsub(node.settings$rem, "", str_trim(unlist(x))))
                  },
                  {
                    # nodes.tmp <- gsub(node.reg, "", data[, node.settings$col]) 
                    # Split the nodes by the specified character
                    nodes.tmp <- strsplit(nodes.tmp, split = node.settings$sep[4])
                    # Remove additional characters using the specified regular expression
                    lapply(nodes.tmp, function(x) gsub(node.settings$rem, "", str_trim(unlist(x))))
                  })
  return(nodes) 
}

# Create the Node-Document matrix
create.matrix <- function(nodes, node.type = "actor", db = "wos", term.minlength = 1, 
                          term.weight = "ntc", match.max = 0.10, 
                          clean.str = "dpto|dept|department"){
  mat <- switch(node.type,
                term = 
                  { 
                  suppressWarnings(
                              TermDocumentMatrix(nodes, 
                                          control=list(wordLengths = c(term.minlength, Inf),
                                                      weighting = function(x) 
                                                      weightSMART(x, spec=term.weight))))
                  },
                  {
                  if (node.type == "actor") {
                    nodes.all <- nodes
                    nodes <- lapply(nodes, function(x) x[[1]])
                    }
                    # Determine number of nodes per document
                    nodes.per.doc <- sapply(nodes, function(x) length(x))
                    # Create list of document numbers for each node 
                    nodes.doc.no <- rep(1:length(nodes.per.doc), times = nodes.per.doc)
                    nodes.unlist <- unlist(nodes)
                    # Determine number of nodes
                    node.count <- length(nodes.unlist)
                    # Clean up string by removing specified keywords and trimming
                    nodes.unlist <- sapply(strsplit(nodes.unlist, ","), 
                                        function(x) {
                                            match <- grep(clean.str, x, ignore.case = TRUE)
                                            if (length(match) > 0)
                                              paste(str_trim(x[-match]), collapse=",")
                                            else
                                              paste(str_trim(x), collapse=",")
                                        })
                    # Determine string lengths for each node, determine the max length for each adist comparison
                    mat.strlen <- t(matrix(str_length(nodes.unlist), nrow = node.count, ncol = node.count))
                    mat.strlen <- matrix(pmax(diag(mat.strlen), mat.strlen), nrow = node.count)
                    # Compare nodes by adist function. If nodes are close to one another, they are a match.
                    # The closeness of matches is set by match.max. If match.max is set to 0.1, then less
                    # than 10% of the strings must not match to be seen as the same.
                    match.b <- adist(nodes.unlist, nodes.unlist, ignore.case = TRUE)/mat.strlen < match.max
                    # Determine which nodes match
                    node.match <- apply(match.b, 1, function(x) which(x))
                    # Find unique matches. Since match of multiple nodes will have the same matching indexes,
                    # only include one.
                    node.match.unq <- node.match[!duplicated(node.match)]
                    # Determine document numbers for matching nodes
                    node.match.doc <- lapply(node.match.unq, function(x) nodes.doc.no[x])
  
                    # Create nodes/document matrix
                    mat.tmp <- matrix(0, nrow = length(node.match.doc), ncol = length(nodes), 
                                  dimnames = list(nodes.unlist[!duplicated(node.match)], 1:length(nodes)))
                    for (i in 1:nrow(mat.tmp))
                      mat.tmp[i, node.match.doc[[i]]] <- 1
  
                    mat.tmp <- as.matrix(mat.tmp)
                    mat.adj <- mat.tmp %*% t(mat.tmp)
  
                    # Create dataframe for actors containing all the relevant information
                    if (node.type == "actor") {
                      actors <- switch(db,
                                    'com' = {
                                            ind <- 3
                                            affl.unlist <- unlist(lapply(nodes.all, function(x) 
                                                                                        x[[2]]))[!duplicated(node.match)]
                                            c.author.unlist <- unlist(lapply(node.match.doc, function(x) 
                                                                                                paste(sapply(nodes.all[x], function(y) 
                                                                                                                              y[[ind]][1]), collapse = "|")))
                                            data.frame(C.Author = c.author.unlist, Affiliation = affl.unlist)
                                    },
                                    'wos' = {
                                            ind <- 2
                                            tc.unlist <- unlist(lapply(node.match.doc, function(x) 
                                                                                          sum(as.numeric(sapply(nodes.all[x], function(y) 
                                                                                                                                y[[ind]][1])))))
                                            email.unlist <- unlist(lapply(node.match.doc, function(x) 
                                                                                            paste(unique(sapply(nodes.all[x], function(y) 
                                                                                                                          y[[ind]][5])), collapse = "|")))
                                            affl.unlist <- unlist(lapply(node.match.doc, function(x) 
                                                                                            paste(unique(sapply(nodes.all[x], function(y) 
                                                                                                                          y[[ind]][6])), collapse = "|")))
                                            data.frame(Total.Citations = tc.unlist, Affiliation = affl.unlist, Email = email.unlist)
                                    },
                                    'pat' = {
                                            ind <- 3
                                            city.unlist <- unlist(lapply(nodes.all, function(x) 
                                                                                        x[[2]]))[!duplicated(node.match)]
                                            affl.unlist <- unlist(lapply(node.match.doc, function(x) 
                                                                                            paste(unique(sapply(nodes.all[x], function(y) 
                                                                                                                              y[[ind]][1])), collapse = "|")))
                                            data.frame(Affiliation = affl.unlist, City = city.unlist)
                                      })
                      author <- nodes.unlist[!duplicated(node.match)]
                      no.of.docs <- sapply(node.match.unq, function(x) length(x))
                      doi.unlist <- unlist(lapply(node.match.doc, function(x) 
                                                                      paste(sapply(nodes.all[x], function(y) 
                                                                                                    y[[ind]][2]), collapse = "|")))
                      id.unlist <- unlist(lapply(node.match.doc, function(x) 
                                                                      paste(sapply(nodes.all[x], function(y) 
                                                                                                    y[[ind]][3]), collapse = "|")))
                      title.unlist <- unlist(lapply(node.match.doc, function(x) 
                                                                      paste(sapply(nodes.all[x], function(y) 
                                                                                                    y[[ind]][4]), collapse = "|")))
                      actors <- data.frame(Author = author, Publications = no.of.docs, 
                                          DOI = doi.unlist, Article.ID = id.unlist, Title = title.unlist, actors)
                      list(mat = mat.adj, actors = actors)
                    }
                    else
                      mat.adj
                  })
  return(mat)
}

create.network <- function(mat, weighted = TRUE, mode = "undirected"){
  graph <- graph.adjacency(mat, weighted = weighted, mode = mode)
  graph <- simplify(graph)
  V(graph)$label <- V(graph)$name
  V(graph)$degree <- degree(graph)
  return(graph)
} 

# Extract summary of results
summary.results <- function(net) {
  # Results for individual nodes
  node <- data.frame(Degree = degree(net), 
                     Betweenness = as.numeric(formatC(betweenness(net), digits = 3, format = "f")), 
                     Closeness = as.numeric(formatC(closeness(net), digits = 3)),
                     EigenVector = as.numeric(formatC(evcent(net)$vector, digits = 3)),
                     Local.Cluster = as.numeric(formatC(transitivity(net, type = "local"), digits = 3)))
  # Results for network
  net <- data.frame(Nodes = length(V(net)), Edges = length(E(net)), 
                    Density = graph.density(net), 
                    Global_Cluster = transitivity(net, type="global"), 
                    Edge_Connectivity = edge.connectivity(net), 
                    Adhesion = graph.adhesion(net))
  return(list(node = node, net = net))
}

# Print summary of results
print.results <- function(net.summary, node.summary, print.by = "Betweenness", print.count = 20) {
  
  print("#----------------------NETWORK SUMMARY------------------------#")
  print(net.summary)
  # Write network summary results to csv file
  tryCatch({ write.csv(net.summary, file = "net.summary.csv")
             print(paste("Files saved to", getwd())) },
           error = function(e) {
             warning(paste("Could not write network summary data to csv. Ensure write access",
                           "is allowed for", getwd(), "and that the file net.summary.csv is closed. Type",
                           "write.csv(net.summary, file = 'net.summary.csv') to save results."), call. = FALSE)
           }) 
  
  # Show results for individual nodes
  print("#----------------------NODE SUMMARY---------------------------#")
  # Write individual network results to csv file
  print(head(node.summary[order(node.summary[, print.by], decreasing = TRUE), ], min(print.count, nrow(node.summary))))
  # Write individual network results to csv file
  tryCatch({ write.csv(node.summary, file = "node.summary.csv")
             print(paste("Files saved to", getwd())) },
           error = function(e) {
             warning(paste("Could not write network summary data to csv. Ensure write access",
                           "is allowed for", getwd(), "and that the file node.summary.csv is closed. Type",
                           "write.csv(node.summary, file = 'node.summary.csv') to save results."), call. = FALSE)
           }) 
}

# Plot results
plot.results <- function(node.summary, plot.by = "Betweenness", plot.count = 20, plot.edges = TRUE, layout = layout.kamada.kawai) {
  # Select nodes with the highest 'plot.by' selection (Degree or Betweenness). Select first 'plot.count' nodes
  high.nodes <- order(node.summary[, plot.by], decreasing = TRUE)[1:min(plot.count, nrow(node.summary))]
  net.s <- delete.vertices(net, V(net)[!(1:nrow(node.summary) %in% high.nodes)])
  # Set size of vertices
  if (plot.by == "InDegree")
    v.size <- degree(net.s, mode="in")
  else
    v.size <- betweenness(net.s)
  if (max(v.size) == 0)
    v.size = 1
  else
    v.size <- (v.size/max(v.size)) * 20
  
  # Set size of edges
  e.size <- (E(net.s)$weight/max(E(net.s)$weight)) * 4
  
  if (!plot.edges)
    net.s <- delete.edges(net.s, E(net.s)[e.size <= 1])
  V(net.s)$size <- v.size
  V(net.s)$label.cex <- 1
  V(net.s)$label.color <- "black"
  V(net.s)$color <- "#D2691E"
  E(net.s)$width <- e.size
  E(net.s)$color <- "steelblue"
  tkplot(net.s, canvas.width=800, canvas.height=800, layout = layout)
}