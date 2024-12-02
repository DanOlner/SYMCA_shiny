#functions
#For checking on call order
callcounter = 0

#Text output, including a counter each time called and a line break
inc <- function( ... ){
  
  callcounter <<- callcounter + 1
  cat(callcounter,": ", ..., "\n")
  
}

#Abbreviated vat with added line break
ct <- function(...) cat(...,"\n")




#Return either all-positive-values fisher palette bins
#Or one that diverges if values are on each side of zero
returnpalette <- function(x, togglestate, n){
  
  inc('Inside returnpalette function - number of values passed in: ', length(x))

  palette <- NULL

  if(togglestate){

    # ct('trueping!')
    
    #Make sure there are enough values for the requested bin number. Reduce if not.
    while (n > 0) {
      
      # Try to create class intervals
      try_result <- try({
        fisher_breaks <- classInt::classIntervals(x, n = n, style = "fisher")$brks %>% round %>% unique
      }, silent = TRUE)
      
      # Check if the result was successful
      if (!inherits(try_result, "try-error")) {
        break  # Return successful result
      }
      
      # Reduce the number of classes and try again
      n <- n - 1
      
    }
    
    # fisher_breaks <- classInt::classIntervals(x, n = n, style = "fisher")$brks %>% round %>% unique
    
    # ct("Current slider vals: ",isolate(input$employee_count_range))
    
    palette <- colorBin(palette = "RdYlBu", bins = fisher_breaks, domain = x)

  } else {

    # ct('falseping!')

    fisher_breaks_pos <- classInt::classIntervals(x[x > 0], n = ifelse(n %% 2 == 0, n/2, (n-1)/2), style = "fisher")$brks %>% round %>% unique
    # fisher_breaks_pos <- classInt::classIntervals(x[x > 0], n = n/2, style = "fisher")$brks %>% round %>% unique
    fisher_breaks_neg <- classInt::classIntervals(x[x <= 0], n = n/2, style = "fisher")$brks %>% round()
    
    fisher_breaks <- c(fisher_breaks_neg,fisher_breaks_pos)

    palette <- colorBin(palette = "RdYlBu", bins = fisher_breaks, domain = x)

  }

  ct("Inside returnpalette function - Fisher breaks made (including rounding): ", fisher_breaks)
  
  return(palette)

}