library(lubridate)
library(tidyverse)

in_fol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_input/5d_6o_keyDays2/"

nscen = 5
per_len = 120 #this is max period length
vdr_inputs = array(dim = c(4,18*per_len,nscen)) #pp, period, o
last_row = 1
pps = c(2,4,6,8)
for(pp in 1:length(pps)){
  print(paste("pp",pps[pp]))
    vdrfiles = list.files(path = in_fol, pattern = paste0("drResponseScenarios_vdr_o5_0.",pps[pp],"pp_1day_p"))
  for(i in 1:length(vdrfiles)){ 
    print(paste("i",i))
    vdr = as.matrix(read_csv(paste0(in_fol,vdrfiles[i])))
    vdr_inputs[pp,last_row:(last_row+nrow(vdr)-1),] = vdr
    last_row = last_row + nrow(vdr) + 1
  }
  last_row = 1
}

# there are 90 rows of NA at the bottom of vdr_inputs
# there are 2160 rows total
nhours = 2160-90
vdr_inputs = vdr_inputs[,1:nhours,]

# take sums across second dimension
dimnames(vdr_inputs) = list(paste0("pp0.",pps),c(),paste0("o",1:nscen))

vdrsum = apply(vdr_inputs,c(1,3),sum,na.rm=T)/nhours
# dimnames(vdrsum) = list(paste0("pp0.",pps),paste0("o",1:5))
print(1-vdrsum)
print(rowSums(vdrsum)/nscen)
# vdravg = apply(vdr)

# examine SD of distributions
vdrsd = apply(vdr_inputs,1,sd,na.rm=T)
print(vdrsd)
barplot(height = vdrsd)

vdr2d = array_reshape(vdr_inputs,dim = c())

# plot
barplot(height =max(rowSums(vdrsum)/nscen)-rowSums(vdrsum)/nscen)

vdrmean = apply(vdr_inputs,1,mean,na.rm=T)
barplot(height = vdrmean-1)


