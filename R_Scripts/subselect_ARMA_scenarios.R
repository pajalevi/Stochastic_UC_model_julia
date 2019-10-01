#subselect_ARMA_scenarios.R
# from the _prob and _vdem files already created
# from an ARMA simulation, subselect a given number
# of them and save them with the appropriate naming
# Use this if you've already used write_key_days.R
# to make the original ARMA scenarios and you want to
# run a model that uses fewer scenarios

# currently just saves the first n scenarios
library(stringr)
library(tidyverse)

subselect_ARMA = function(n_scenarios,folder, filepattern = "demandScenarios_prob_ARMA26.0",ID_length=5,extra_ID = ""){
  # filepattern is assumed to have the standard 3-part identifier 
  # starting at the end of the form "pXX_XXXX_XXXX.csv"
  # or at least be consistent across prob and vdem
  
  # make a list of all files
  file_names = list.files(path = folder, pattern = filepattern)
  file_ends = substring(file_names,nchar(filepattern)+2,100)
  filepattern_vdem = str_replace(filepattern,"prob","vdem")
 
  
  # iterate through each
  for(i in 1:length(file_ends)){
    # make and save new _prob file with appropriate adjustment
      # load prob file
    probs = read.csv(file = paste0(folder,filepattern,"_",file_ends[i]))
    scenario_sample = sample(1:nrow(probs),size=n_scenarios)
    print(scenario_sample)
    
    totprob = sum(probs$V1[1:n_scenarios])
    newprob = round(probs$V1[scenario_sample]/totprob,15)
    if(abs(sum(newprob) - 1) > 1e-10){stop("newprob is",newprob,"something has gone terribly wrong")}
    
    outfile = data.frame(V1 = newprob)
    write_csv(outfile, paste0(folder,filepattern,extra_ID,"_o",n_scenarios,"_",substr(file_ends[i],ID_length,100)))
    
    # make and save new _vdem file with subselection
    # print(paste0(folder,filepattern_vdem,"_", file_ends[i]))
    vdem = read.csv(file = paste0(folder,filepattern_vdem,"_", file_ends[i]))
    vdem_subsel = vdem[,scenario_sample]#vdem[,1:n_scenarios]
    write_csv(vdem_subsel, paste0(folder,filepattern_vdem,extra_ID,"_o",n_scenarios,"_",substr(file_ends[i],ID_length,100)))
  }
}

# folder = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_input/5d_6o_periods/"
# subselect_ARMA(5,folder)

folder = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_input/5d_6o_testDays/"
subselect_ARMA(5,folder, ID_length = 5,extra_ID = "_v4")
