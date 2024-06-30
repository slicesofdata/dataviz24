view_html <- function(object, rows = F, show = 100,...) { 
  
  if (!require(DT)) {
    
    stop("DT library not installed")
    
  } else {
    
    if (tibble::is_tibble(object)) {
      object = as.data.frame(object)
      #message("converted tibble to dataframe for viewing")
    } 
    
    if (is.null(dim(object)) & class(object) == "list") {
      message("Object is a list. Viewer displays last list element. Consider passing each element to view().")
      
      lapply(object, function(x) {
        DT::datatable(x, rownames = rows, options = list(pageLength = show))
      }) 
    } else {
      DT::datatable(object, rownames = rows, options = list(pageLength = show)) 
    }
    
  }
}
