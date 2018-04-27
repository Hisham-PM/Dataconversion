#' @title JSON file to csv format
#'
#' @description Conversion of JSON data to csv format.
#'
#' @param input_file the path of a JSON file to read the file from; Only one  file must be supplied
#' 
#' @examples test1 <- '{"a":true, "b":false, "c":null}'
#' @examples input_file <- tempfile(fileext = ".json")
#' @examples writeLines(test1, input_file)
#' @examples JSON2csv(input_file = input_file)
#' 
#' @import rjson
#' 
#' @return NULL
#' 
#' @import utils
#'
#' @export JSON2csv
#' 
#' 
JSON2csv<-function(input_file)
{
  # eg:  input_file<-("/home/uvionics/Desktop/data_conversion/data.json")
  
  # library(rjson) # install.packages("rjson")
  
  # finds the  length of charecters in the source code path.
  # eg: > nchar(input_file) 
  #     [1] 48
  l<-nchar(input_file)
  
  # Make a sub string of path of the 'input_file' from 1 to (length-4)
  # getting the source code path without the last 4 charecter.
  # get source path directory without the format of source file
  # eg: > substr(input_file,1, (l-3))
  #    [1]  "/home/uvionics/Desktop/data_conversion/data."
  output<- substr(input_file,1,(l-4))
  
  #--Reads a JSON file
  JSON_format<-fromJSON(file=input_file)
  
  #Find out the column number of JSON file
  n <- length(JSON_format)
  
  # Find out "NA"s and replace with  blank space(""). 
  for(i in 1:n){
    a<-which(JSON_format[[i]] == "NA")
    if(length(a) > 0){
      JSON_format[[i]][a]<-""
    }
  }
  
  #--Converts to csv format
  csv_format <- do.call("cbind", JSON_format)
  
  #--write the Converted csv file in the original source code directory itself.
  #  eg  : [1]  "/home/uvionics/Desktop/data_conversion/data.csv
  write.csv(csv_format,file=paste(output,"csv",sep = ''), row.names = FALSE)
  
}





#' @title csv file to JSON format
#'
#' @description Conversion of csv data to JSON format.
#'
#' @param input_file the path of a csv file to read the file from; Only one  file must be supplied
#'
#' @examples test1 <- as.character(sample(1:100,10))
#' @examples input_file <- tempfile(fileext = ".csv")
#' @examples writeLines(test1, input_file)
#' @examples csv2JSON(input_file = input_file)
#' 
#' @import rjson
#' 
#' @return NULL
#' 
#' @import utils
#'
#' @export csv2JSON
#' 
csv2JSON <- function(input_file){
  # eg: input_file<-("/home/uvionics/Desktop/data_conversion/data.csv")
  
  # library(rjson)  install.packages("rjson")
  
  # finds the  length of charecters in the source code path.
  # eg: > nchar(input_file)
  #     [1] 47
  l<-nchar(input_file)
  
  # Make a sub string of path of the 'input_file' from 1 to (length-3)
  # getting the source code path without the last 3 charecter.
  # get source path directory without the format of source file
  # eg: > substr(input_file,1, (l-3))
  #    [1]  "/home/uvionics/Desktop/data_conversion/data."
  output<- substr(input_file,1, (l-3))
  
  #--Reads a csv file
  csv_format<-read.csv(file=input_file,header=T,stringsAsFactors = F)
  
  #--Converts to JSON format
  JSON_format<-toJSON(csv_format)
  
  # JSON_out<-JSON_format
  
  #--write the Converted JSON file in the original source code directory itself.
  #  eg  : [1]  "/home/uvionics/Desktop/data_conversion/data.json
  write(JSON_format,paste(output,"json",sep = ''))
  
  # return(JSON_out)
}