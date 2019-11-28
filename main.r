setwd("E:\R")

source("MMSDSampling.r")

popFile<-"sampling.csv"

pop <- read.csv(popFile)

#sink("genInit.txt")
initialSample<-MMSDSampling(pop,150,20000)

write.table(initialSample, file ="initSample.csv", sep = ",", col.names = NA,qmethod = "double")

#sink()