pollutantmean <- function(directory, pollutant, id = 1:332) {
        v <- c()
        for (i in id){    
                w <- read.csv(paste(directory,"/",dir(directory)[i],sep=""))[,pollutant]
                v <- append(v,w)
              } 
        mean(v, na.rm=TRUE)
}