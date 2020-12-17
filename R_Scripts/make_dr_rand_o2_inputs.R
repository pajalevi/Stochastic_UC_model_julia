# vdr_in = convert(Array,CSV.read(string(subsel_data_fol,"drResponseScenarios_vdr_",inputs[1,:DRrand_ID],"_",periodsave,".csv")))
# vdr_p = convert(Array,CSV.read(string(subsel_data_fol,"drResponseScenarios_vdr_p_",inputs[1,:DRrand_ID],"_",periodsave,".csv")))


periodfileloc = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_input/5d_6o_keyDays2/"
names = list.files(path = periodfileloc, pattern = "^periods")

meanresponse = 1
nomega = 2
randID = "o2_100mean_5050_"

for(i in 1:length(names)){
  print(names[i])
  periods = read.csv(paste0(periodfileloc, names[i]), header = FALSE)
  print(nrow(periods))
  
  vdr = matrix(nrow = nrow(periods), ncol = nomega, dimnames = list(c(), paste0("x",1:nomega)))
  if(nomega ==2){
    responses = c(1,1)
  } else if(nomega ==1) {
    responses = c(meanresponse)
  }
  for(j in 1:nomega){
    vdr[,j] = responses[j]
  }
  vdrp = matrix(nrow = 1, ncol = nomega, dimnames = list(c(), paste0("x",1:nomega)) )
  if(nomega ==2){
    vdrp[1,] = c(0.5,0.5)#c(meanresponse, 1-meanresponse)
  } else if(nomega ==1) {
    vdrp[1,] = c(1)
  }
  
 
  
  n = strsplit(names[i],"\\.|_")[[1]]
  write.csv(vdr, file = paste0(periodfileloc, "drResponseScenarios_vdr_",randID,"p",n[2],"_",n[3],"_",n[4],".csv"), 
            row.names = FALSE)
  write.csv(vdrp, file = paste0(periodfileloc, "drResponseScenarios_vdr_p_",randID, "p",n[2],"_",n[3],"_",n[4],".csv"), 
            row.names = FALSE)
}
