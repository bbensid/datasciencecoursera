complete <- function(directory, id = 1:332) {
        dt <- data.frame(id=NULL,nobs=NULL)
        for (i in id){    
          w <- read.csv(paste(directory,"/",dir(directory)[i],sep=""))
          obs <- sum(complete.cases(w))
          dt <- rbind(dt,data.frame(id=i,nobs=obs))
        }
        dt
}