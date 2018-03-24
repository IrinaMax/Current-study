# set up dir for all work appears there
setwd('/Users/38923/Desktop/R/For_Amit') 
h <- read.csv("/Users/38923/Desktop/R/For_Amit/cp3_70c_postdr.csv")

nrow(h)/100  # devide for how many bis you want

n <- 963    # how many rows you have in every bin
nr <- nrow(h)
bins <- split(h, rep(1:ceiling(nr/n), each=n, length.out=nr))

# my function to write all bins as csv in the same folder
for(i in names(bins) ){
  write.csv(bins[[i]], paste0('bin',i,".csv"))
}

# you can also use mapply library(dplyr) just
mapply(write.csv, bins, paste0("h_part", names(bins),".csv"))   # working fine
