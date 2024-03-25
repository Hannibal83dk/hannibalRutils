


#' Replace all white spaces in a character vector with underscores
#'
#' @param vector A character vector containing text where spaces needs to be replaced
#'
#' @return Returns the provided vector with the white spaces replaced with underscores
#' @export
#'
#' @examples
#' vec <- c("Word1 word2", "Word3 word4")
#'
#' SpaceReplace(vec)
#'
#'
SpaceReplace <- function(vector){

  return(gsub(" ", "_", vector))

}


#' Extract code to hard code creation of a given vector
#'
#' @param vector A vector for which is wanted to be hard coded.
#'
#' @return Returns a text that represent a code snippet to hard code creation of the given vecor
#' @export
#'
#' @examples
#'
#' vec <- c("Word1 word2", "Word3 word4")
#'
#' extractVectorCode(vec)
#'
extractVectorCode <- function(vector){

  codetext <- "c("

  for(i in 1:(length(vector)-1)){
    codetext <- paste0(codetext, " '", vector[i], "',")
  }
  codetext <- paste0(codetext, " '", vector[i+1], "')")

  return(codetext)
}



#' converts numeric CPR numbers to characters and adds a leading 0 to danish CPR number of only 7 digits.
#'
#' @param vector a vector containing numeric cpr numbers
#'
#' @return A vector containing danish cpr number in character form adding a possible leading 0.
#' @export
#'
#' @examples
#'
#' vec <- c(9076342, 3102314, 24032789)
#'
#' convertToCPR(vec)
#'
convertToCPR <- function(vector){

  charvec <- NA

  for (i in 1:length(vector)) {

    vector <- as.character(vector)

    if(nchar(vector[i]) == 9){
      charvec[i] <- paste0("0",vector[i])
    } else {charvec[i] <- vector[i]}

  }

  return(charvec)
}


#' Factorize a column in a dataframe
#'
#' @param x A vector to convert to vector, normally a column in a dataframe
#'
#' @return Returns nothing bu prints the code used for factorizing the specific column
#' @export
#'
#' @examples
#'
#' col1 <- c("Area1", "Area2", "Area3")
#'
#' col2 <- c(10, 20, 30)
#'
#' df <- data.frame(Area = col1, Size = col2)
#'
#'
#' factorize(df$Area)
#'
#'
factorize <- function(x){


  print(paste0(deparse(substitute(x)), "<- as.factor(", deparse(substitute(x)),")"))
  return(eval.parent(
    parse(
      text = paste0(deparse(substitute(x)), "<- as.factor(", deparse(substitute(x)),")")
    ), n = 1
  ))

  print('factorization finished')

}

#' Convert Cpr to date
#'
#' @param x A character string containing a cpr number
#'
#' @return Returns the birthday of a person given a cpr number
#' @export
#'
#' @examples
#'
#' cpr <- 0203592008
#'
#' convert.cprTodate(cpr)
#'
convert.cprTodate <- function(x){

  ## Find date of birth from cpr
  temp <- paste(substr(x, 1,4), "19", substr(x, 5,6), sep = "")
  ## convert to dateTime
  temp <- as.Date(temp, '%d%m%Y')

  return(temp)
}



#' Removes columns from data frames
#'
#' @param data A data frame containing the column to remove
#' @param colnames The name of the columnn that is to be removed
#'
#' @return Nothing, however runs code to remove volumns from a data frame
#' @export
#'
#' @examples
#'
#' col1 <- c("Area1", "Area2", "Area3")
#'
#' col2 <- c(10, 20, 30)
#'
#' df <- data.frame(Area = col1, Size = col2)
#'
#'
#' dropcols(df, "Area")
#'
#'
#'
#'
dropcols <- function(data, colnames){

  funcCall <- paste0(deparse(substitute(data)), " <- ",deparse(substitute(data)), "[!names(",deparse(substitute(data)),") %in% ", deparse(substitute(colnames)),"]")

  print(funcCall)

  #data <- data[names(data) != colname]

  eval(
    parse(
      text = funcCall
    ), envir = .GlobalEnv
  )


}
git_default_branch_rediscover()


#' Create a section comment for easy division and overview of scripts
#'
#' @param text The text to be displayed, must be at least two characters shorter than width
#' @param width How wide should the comment be
#'
#' @return A text containing the comment
#' @export
#'
#' @examples
#'
#' SectionComment("Section 1")
#'
#'
SectionComment <- function(text = "Example text", width = 80){

  width <- width-2

  txt <- c()
  txt[1] <- paste0("#", paste0(replicate(width, ":"), collapse = ""), "#\n")

  txt[2] <- paste0("#", paste0(replicate(width, " "), collapse = ""), "#\n")


  txt[3] <- paste0("#",
                   paste0(replicate((width-nchar(text))/2, " "), collapse = ""),
                   text,                                                               # <- Text goes here
                   paste0(replicate((width-nchar(text))/2, " "), collapse = ""),
                   "#\n")


  txt[4] <- paste0("#", paste0(replicate(width, " "), collapse = ""), "#\n")

  txt[5] <- paste0("#", paste0(replicate(width, ":"), collapse = ""), "#\n")


  cat((txt), sep = "")

}



######################################################
##						    ##
##  Find sample/water mix based on dilution factor  ##
##						    ##
######################################################

# decimalnumcount<-function(x){stopifnot(class(x)=="character")
#   x<-gsub("(.*)(\\.)|([0]*$)","",x)
#   nchar(x)
# }
#
# findmix <- function(dilutionfactor){
#
#   multi = 1.00
#   while(1)
#   {
#     x <- dilutionfactor * multi
#     dec <- decimalnumcount(as.character(x))
#     sample = x/dilutionfactor
#     water = x-sample
#
#     cat(paste("\n multi: ", multi,"x: ", x, "decimals :", dec, "\n\n"))
#
#     if(dec <=2 && sample + water > 2){
#
#       break
#     }
#
#     else{multi = round(multi + 0.01, 2)}
#
#   }
#
#   cat(paste("You need ", sample, " µl sample and ", water, " µl water"))
#
#   # return(c(sample,water))
#
# }
########################  end findmix  ###########################

## winDialog(type = c("ok", "okcancel", "yesno", "yesnocancel"), message)

# winDialog(type = "okcancel", "message")
