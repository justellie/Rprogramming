pollutantmean<- function()
{
  
}





main <- function() {

  if(interactive()) 
  {
    print("In interactive mode")
  } 
  else 
  {
    print("Not in interactive mode")
  }
  n1<-readline(prompt="Entrada: ")
  n1<-as.integer(n1)
  print(n1)
}


main()
