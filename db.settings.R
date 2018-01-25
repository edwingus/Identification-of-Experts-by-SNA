db.settings <- function(db, node.type, term.type = NA) {
  sep.l <- c("", " ", ",", ";", "\t", "-")
  
  settings <- switch(db,
                  'wos' = {  
                    # Columns to extract from the data for each node type selected
                    col <- switch(node.type,
                                      actor = c("AU", "TC", "DI", "ID", "TI", "EM", "C1"),
                                      term = switch(term.type, title = "TI", abstract = "AB", keyword = "DE", NA),
                                      affl = "C1",
                                      category = "WC")
                    # Characters seperating nodes in the data (;)
                    sep <- sep.l[4]
                    # Characters to be extracted from the data
                    ext <- NA
                    #Characters to be removed from the data
                    rem <- ifelse(node.type == "affl", "\\s*\\[(.*?)\\]\\s*", "\\.+\\s*")
                    # Extract initials from first names
                    f.name <- NA
                    
                    list(col = col, sep = sep, rem = rem, ext = ext, f.name = f.name)
                    },
                  'com' = {
                    # Columns to extract from the data for each node type selected
                    col <- switch(node.type,
                                       actor = c("Author", "Author.affiliation", "CODEN", "DOI", "ID", "Title"),
                                       term = switch(term.type, title = "Title", abstract = "Abstract", 
                                                     keyword = c("Controlled.Subject.terms", "Uncontrolled terms")),
                                       affl = "Author.affiliation",
                                       category = "Classification.code",
                                       journal = "Source")
                    # Characters seperating nodes in the data
                    sep <- switch(node.type,
                                       actor = c(sep.l[4], "\\s*\\([0-9]+\\)\\s*"), 
                                       affl = "\\s*\\([0-9]+\\)\\s*",
                                       category = "\\s*-\\s*",
                                       sep.l[4])
                    # Characters to be extracted from the data
                    ext <- switch(node.type,
                                       actor = "\\s*\\([0-9]+\\)\\s*", 
                                       "")
                    #Characters to be removed from the data
                    rem <- switch(node.type,
                                       affl = "^\\([0-9]+\\)\\s*", 
                                       "\\s*\\(.+\\)\\s*")
                    # Required to extract affiliation number from author name
                    affl <- switch(node.type,
                                  actor = ".*\\((.*)\\).*", 
                                  NA)
                    # Extract initials from first names
                    f.name <- ifelse(node.type == "actor", "^(.?).*", NA)
                    
                    list(col = col, sep = sep, rem = rem,  ext = ext, affl = affl, f.name = f.name)
                  },
                  'pat' = {  
                    # Columns to extract from the data for each node type selected
                    col <- switch(node.type,
                                  actor = c("Inventor.Name", "Assignee", "Document.Number", "ID", "Title"),
                                  term = switch(term.type, title = "Title", abstract = "Abstract", NA),
                                  affl = "Assignee",
                                  category = "Primary.Class")
                    # Characters seperating nodes in the data (;)
                    sep <- sep.l[4]
                    # Characters to be extracted from the data
                    ext <- switch(node.type,
                                  actor = "\\s*\\(.+\\)\\s*", 
                                  "")
                    #Characters to be removed from the data
                    rem <- switch(node.type,
                                  actor = "\\s*\\(.+\\)\\s*")
                    
                    # Required to extract affiliation number from author name
                    affl <- switch(node.type,
                                   actor = ".*\\((.*)\\).*", 
                                   "")
                    
                    # Extract initials from first names
                    f.name <- ifelse(node.type == "actor", "^(.?).*", NA)
                    
                    list(col = col, sep = sep, rem = rem, ext = ext, f.name = f.name, affl = affl)
                  }
  )
  return(settings)
}
