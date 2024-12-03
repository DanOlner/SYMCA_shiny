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



# returnpalette <- function(x, togglestate, n){
# 
#   while (n > 0) {
# 
#   palette <- case_when(
# 
#    togglestate
# 
#    )
# 
# }


#Used in returnpalette function -->
#Aim for 7 bins. If that causes issues in classIntervals, drop until it breaks, then set to a single value
gracefully_drop_palettebins <- function(x, n, palette = "RdYlBu"){
  
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
  
  #If it hasn't been found here, x had a single value (and classIntervals can't work with a single val) - set palette accordingly
  if(exists("fisher_breaks")){
    
    return(
      list(
        palette = colorBin(palette = "RdYlBu", bins = fisher_breaks, domain = x),
        values = fisher_breaks
       )
      )
    
  } else {
    
    #Single value...
    return(
      list(
        palette = colorFactor(palette = palette, domain = x),
        values = x
        )
      )
    
  }
  
}



#Return either all-positive-values fisher palette bins
#Or one that diverges if values are on each side of zero
returnpalette <- function(x, togglestate, n){
  
  inc('Inside returnpalette function - number of values passed in: ', length(x))

  # palette <- NULL

  #togglestate TRUE is to display employee counts, FALSE for % change (possibly diverging)
  if(togglestate){

    # ct('trueping!')
    palette <- gracefully_drop_palettebins(x,n)$palette
   
  } else {#else if togglestate

    # ct('falseping!')
    
    #May have single values for each of those, or no values at all.
    # fisher_breaks_pos <- classInt::classIntervals(x[x > 0], n = ifelse(n %% 2 == 0, n/2, (n-1)/2), style = "fisher")$brks %>% round %>% unique
    # 
    # # fisher_breaks_pos <- classInt::classIntervals(x[x > 0], n = n/2, style = "fisher")$brks %>% round %>% unique
    # fisher_breaks_neg <- classInt::classIntervals(x[x <= 0], n = n/2, style = "fisher")$brks %>% round()
    
    
     
    # fisher_breaks <- c(fisher_breaks_neg,fisher_breaks_pos)

    palette_neg_values <- gracefully_drop_palettebins(x[x < 0],n/2 %>% round)$values
    palette_pos_values <- gracefully_drop_palettebins(x[x >= 0],n/2 %>% round)$values
    
    palette <- colorBin(palette = "RdYlBu", bins = c(palette_neg_values,palette_pos_values) %>% unique, domain = x)
    

  }

  if(exists("fisher_breaks")){
    ct("Inside returnpalette function - Fisher breaks made (including rounding): ", fisher_breaks)
  } else {
    ct("Inside returnpalette function - NO Fisher breaks made.")
  }
  
  return(palette)

}