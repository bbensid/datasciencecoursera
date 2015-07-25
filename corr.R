corr <- function(directory, threshold = 0) {
        v <- numeric(0)
        for (i in 1:length(dir(directory))){    
                w <- read.csv(paste(directory,"/",dir(directory)[i],sep=""))
                if (sum(complete.cases(w))< threshold) 
                          {next}
                    else {
                      nitrate <- w$nitrate[complete.cases(w)]
                      sulfate <- w$sulfate[complete.cases(w)]
                      v <- append(v,cor(nitrate, sulfate))
                    }}
        v
}