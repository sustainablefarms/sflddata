# function to return current date and time as a string
return_current_time <- function(){
  current_time <- list(
    hour = lubridate::hour(Sys.time()),
    minute = lubridate::minute(Sys.time())
  )
  current_time <- lapply(current_time, function(a){
    if(nchar(a) < 2){
      a <- paste0("0", a)
    }else{
      a <- as.character(a)
    }
    return(a)
  })
  result <- paste0(
    Sys.Date(),
    "_",
    paste(current_time$hour, current_time$minute, sep = "-")
  )
  return(result)
}