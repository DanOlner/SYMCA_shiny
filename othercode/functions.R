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