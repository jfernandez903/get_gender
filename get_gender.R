# author: Jesus Fernandez
# git hub: https://github.com/jfernandez903/get_gender.git
BehindTheName <- function(name){
  require(gender)
  
  g <- matrix(data = NA, nrow = length(unique(name)), ncol = 2)
  colnames(g) <- c("Name", "Gender")
  
  g[,1] <- unique(name)
  
  n <- gender(g[,1], method = "ipums") 
  
  for(i in 1:length(n$gender)){
    g[which(g[,1] == n$name[i]),2] <- n$gender[i]
  }
  
  helper <- function(g){
    baseurl <- 'http://www.behindthename.com/name/'
    if(is.na(g[2])){                                                 # Start of first if statement
      myurl <- paste(baseurl,g[1], sep = '')
      r <- httr::GET(myurl)
      if (r$status_code < 250){
        x <- readLines(myurl)
        z <- identical(grep("GENDER:",x),integer(0))
        if (!z){
          g[2] <- gsub('^.*<span [^>]*>([^<]*)</span>.*$','\\1',grep('GENDER:',x,value=TRUE))
        }
      }
      
      if(is.na(g[2])){                                               # Start of second if statement 
        myurl <- paste(baseurl,g[1], '-1',sep = '')
        r <- httr::GET(myurl)
        if (r$status_code < 250){
          x <- readLines(myurl)
          z <- identical(grep("GENDER:",x),integer(0))
          if (!z){
            g[2] <- gsub('^.*<span [^>]*>([^<]*)</span>.*$','\\1',grep('GENDER:',x,value=TRUE))
          }
        }
        
        if(is.na(g[2])){                                               # Start of third if statement
          myurl <- paste(baseurl,g[1],'/submitted',sep = '')
          r <- httr::GET(myurl)
          if (r$status_code < 250){
            x <- readLines(myurl)
            z <- identical(grep("GENDER:",x),integer(0))
            if (!z){
              c <- gsub('^.*><span [^>]*>([^<]*)</span>.*$','\\1',grep('GENDER:',x,value=TRUE))
              g[2] <- c[1]
            }
          }
          
          if(is.na(g[2])){
            baseurl1 <- 'http://www.gpeters.com/names/baby-names.php?name='
            myurl <- paste(baseurl1,g[1], sep = '')
            r <- httr::GET(myurl)
            if (r$status_code < 250){
              x <- readLines(myurl, warn = FALSE)
              z <- identical(grep("It's a",x),integer(0))
              if (!z){
                g[2] <- substring(x[149],13,16)
              }
            }
          }
        }                                                               # end of third if statemnt 
      }                                                               #end of second if statement
    }#end of first if statement 
    g
  }
  g <- t(unlist(apply(g,1,helper)))
  for(i in 1:nrow(g)){
    if(is.na(g[i,2])){
      next
    } else if(g[i,2] == "female" || g[i,2] == "Feminine" || g[i,2] == "girl"){
      g[i,2] <- "female"
    } else {
      g[i,2] <- "male"
    }
  }
  
  d <- rep(NA,length(name))
  
  for(i in 1:nrow(g)){
    d[which(name == g[i,1])] <- g[i,2]
  }
  d
}