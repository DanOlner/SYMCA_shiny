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








#Used in returnpalette function -->
#Aim for 7 bins. If that causes issues in classIntervals, drop until it breaks, then set to a single value
gracefully_drop_palettebins <- function(x, n, palette = "RdYlBu"){
  
  #Make sure there are enough values for the requested bin number. Reduce if not.
  while (n > 0) {
    
    # Try to create class intervals
    try_result <- try({
      fisher_breaks <- classInt::classIntervals(x, n = n, style = "fisher")$brks %>% round %>% unique
      # fisher_breaks <- classInt::classIntervals(x, n = n, style = "fisher")$brks %>% unique
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
    
    # inc("Inside graceful palette bin dropping. Fisher values found: ", fisher_breaks)
    
    return(
      list(
        palette = colorBin(palette = "RdYlBu", bins = fisher_breaks, domain = x),
        values = fisher_breaks
       )
      )
    
  } else {
    
    # inc("Inside graceful palette bin dropping. No fisher values found. Value of x: ", x)
    
    #Single value...
    return(
      list(
        palette = colorFactor(palette = palette, domain = round(x)),
        values = round(x)
        )
      )
    
  }
  
}


#For diverging palettes, return the appropriate kind of palette if the length of the bin values is one
#replicating the behaviour in the function above that returns colorFactor rather than colorBin if only one value
return_diverging_palette <- function(bins,x){

  if(length(bins) > 1){
    
    return(colorBin(palette = "RdYlBu", bins = bins, domain = x))
      
  } else {
    
    return(colorFactor(palette = "RdYlBu", domain = x))
    
  }

}


#Return either all-positive-values fisher palette bins
#Or one that diverges if values are on each side of zero
returnpalette <- function(x, togglestate, n){
  
  # inc('Inside returnpalette function - number of values passed in: ', length(x))

  # palette <- NULL

  #togglestate TRUE is to display employee counts, FALSE for % change (possibly diverging)
  if(togglestate){

    #Can palette directly if just doing one set of values
    palette <- gracefully_drop_palettebins(x,n)$palette
   
  } else {#else if togglestate

    #Get the values first THEN combine into the palette so diverges each side of zero correctly
    palette_neg_values <- gracefully_drop_palettebins(x[x < 0],n/2 %>% round)$values
    palette_pos_values <- gracefully_drop_palettebins(x[x >= 0],n/2 %>% round)$values
    
    ct('% change: finding values for positive and negative palette. Values returned:')
    ct('Neg: ', palette_neg_values)
    ct('Pos: ', palette_pos_values)
    
    palette <- return_diverging_palette(c(palette_neg_values,palette_pos_values) %>% unique, x)
    

  }

  
  return(palette)

}