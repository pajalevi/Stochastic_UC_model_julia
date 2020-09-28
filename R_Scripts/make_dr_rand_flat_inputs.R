# vdr_in = convert(Array,CSV.read(string(subsel_data_fol,"drResponseScenarios_vdr_",inputs[1,:DRrand_ID],"_",periodsave,".csv")))
# vdr_p = convert(Array,CSV.read(string(subsel_data_fol,"drResponseScenarios_vdr_p_",inputs[1,:DRrand_ID],"_",periodsave,".csv")))


periodfileloc = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_input/5d_6o_keyDays2/"
names = list.files(path = periodfileloc, pattern = "^periods")

minresponse = 0.2
maxresponse = 1
nomega = 5
randID = "o5_80pp_flat_"

for(i in 1:length(names)){
  print(names[i])
  periods = read.csv(paste0(periodfileloc, names[i]))
  print(nrow(periods))
  
  vdr = matrix(nrow = nrow(periods), ncol = nomega, dimnames = list(c(), c("x1","x2","x3","x4","x5")))
  responses = seq(minresponse, maxresponse, length.out = nomega)
  for(j in 1:nomega){
    vdr[,j] = responses[j]
  }
  vdrp = matrix(nrow = 1, ncol = nomega, dimnames = list(c(), c("x1","x2","x3","x4","x5")) )
  vdrp[,] = 1/nomega
  
  write.csv(vdr, file = paste0(periodfileloc, "drResponseScenarios_vdr_",randID, names[i]), row.names = FALSE)
  write.csv(vdrp, file = paste0(periodfileloc, "drResponseScenarios_vdr_p_",randID, names[i]), row.names = FALSE)
}
