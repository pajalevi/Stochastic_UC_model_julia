# mergeTimeseriesData.R
# reads in timeseries data from multi-period runs
# and concatenates it into one file

# loadTimeseriesData reads in the designated type of timeseries data
# and merges multiple periods into one dataframe

# loadproduction is its precursor, which load slow and fast production data
# concatenates them, and merges multiple periods

# loadTimeseriesData()
# consolidates a given type of timeseries data from multiple runs of ercot_stoch.jl model
# and saves it as a single file
# Feb 2018
# Patricia Levi

library(tidyverse)
library(data.table)

### KLUDGE ###
# this is to select just the periods I wanted this one time
# allowedperiods = read.csv(paste0("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/base_2019-02/","periods_overlap_base_advNot2.csv"))
########

loadTimeseriesData <- function(output_fol, dataType, overlap, dataStage, input_fol, nscen,
                               probabilities = T,dist_ID,endtrim#,
                               # validP = allowedperiods
                               ){
 # dist_ID="winter_ercot"
   # outputfol is the output data folder
  # overlap is the number of hours of overlap between periods
  # dataTypes is the name of the file before the periodID
  #     e.g. "fast_production
  # dataStage indicates the format of the data, as data saved from the first stage
  #     has a different format than data from the second stage which has multiple realizations
  # input_fol is needed if dataStage=2
  # nscen (number of scenarios) also only needed if dataStage=2
 # print(paste("allowed files are", validP$x))

  all_files = list.files(path=output_fol, pattern = dataType)
  nfiles = length(all_files)
  print(paste("there are ",nfiles," files"))
  if(nfiles == 0) { stop("no files found matching dataType ", dataType, " in folder ", output_fol)}
  # ID which files are overlapping
  allperiodinfo = str_split(all_files,"_|\\.|-|p",simplify=T)
  lenInfo = ncol(allperiodinfo)
  
  # sort by the first period
  reorder = order(as.numeric(allperiodinfo[,lenInfo - 2]))
  allperiodinfo = allperiodinfo[reorder,]
  all_files = all_files[reorder]

  prev_overlap = 1+as.numeric(allperiodinfo[,lenInfo - 1])[1:(nfiles-1)] - as.numeric(allperiodinfo[,lenInfo - 2])[2:nfiles]
    # this is 1 + last period[i] - first period[i+1]
  print("Period overlaps are:")
  print(prev_overlap)
  # if i-th  prev_overlap <0 , then (i+1)th period overlaps with previous
  # this should obviate 'overlap'
  
  
  ## check if any of the prev_overlap entries that are <0 are not the correct overlap. if so stop.
  # overlapsel = which(prev_overlap < 0)
  # if(sum(prev_overlap[overlapsel]*-1 != overlap) > 0){
  #   print(paste("Period number: ",allperiodinfo[overlapsel,lenInfo - 3],
  #         " overlaps with previous period by: ",prev_overlap[overlapsel]*-1), collapse = ", ")
  #   stop("Period overlaps are not consistent with prescribed overlap of ",overlap)
  # }
  
  
  for(i in 1:nfiles){ 
    print(paste("i is",i,"file name is",all_files[i]))
    # load period info for this file
    # xx = all_files[i]
    periodinfo = allperiodinfo[i,]
    firstperiod = as.numeric(periodinfo[lenInfo - 2])
    lastperiod = as.numeric(periodinfo[lenInfo - 1])
    periodnum = as.numeric(periodinfo[lenInfo - 3])
    
    # if(periodnum %in% validP$x){
    #   print(paste("period number ", periodnum, " has overlap"))
 
      # load file
      output = read_csv(file = paste0(output_fol,all_files[i]))
      
      # add period num
      output$nperiod = periodnum
      
      if(dataStage == 2){
        # make t represent the hour of year, not hour of period
        output$t = output$t + firstperiod -1
        
        # gather scenario info into column
        output = output %>%
          gather(key = "scenario",value = "value", starts_with("o"))
        
        # load probability associated with each scenario
        # and associate correct label
        if(probabilities){
          probs = read_csv(file = paste0(input_fol,"demandScenarios_prob_",dist_ID,"_p",
                                         periodnum,"_",firstperiod,"_",lastperiod,
                                         ".csv"))
          probs$scenario = paste0("o",1:nscen)
          names(probs) = c("prob","scenario")
          
          # format as datatables for faster merge. (~26x faster!!) 
          # see https://stackoverflow.com/questions/37551433/speed-up-merging-many-dataframes-in-r
          # merge probability with output
          outputdt = as.data.table(output)
          probdt = as.data.table(probs)
          output = merge(outputdt, probdt, all.x = T, by="scenario")
        }
        
      } else if (dataStage == 1){ 
        # add a t column
        output$t = firstperiod:lastperiod
      } else{ stop("dataStage ", dataStage," is invalid. must be 1 or 2")}
      
      # trim output
      if(i ==1){ ## is this the first period? 
        
        print(paste("period",periodnum, "encompassing",firstperiod,"-",lastperiod,"is the first period"))
        if(prev_overlap[i] > 0){ #does it overlap with second period
          # trim beginning with endtrim
          # trim end with overlap
          tSel = which(output$t >= firstperiod + endtrim & 
                         output$t <= lastperiod - overlap/2)
          print(paste("period number",periodnum,"overlaps with next period"))
        } else {
          ## trim both ends with endtrim
          tSel = which(output$t >= firstperiod + endtrim & 
                         output$t <= lastperiod - endtrim)
          print(paste("period number",periodnum,"does not overlap with next period"))
        }

      } else{ # not the first period
        if(prev_overlap[i-1] > 0){ # does it overlap with previous period?
          print(paste("period number",periodnum,"overlaps with previous period"))
          if(periodnum==nfiles){ # is it the last period?
            #TODO: use periodnum info
            # trim beginning with overlap
            # trim end with endtrim
            tSel = which(output$t >= firstperiod + overlap/2 & 
                           output$t <= lastperiod - endtrim)
            print(paste("period number",periodnum,"is the last period"))
          } else if(prev_overlap[i] > 0){ # does it overlap with next period?
            # trim both ends with overlap
            tSel = which(output$t >= firstperiod + overlap/2 & 
                           output$t <= lastperiod - overlap/2)
            print(paste("period number",periodnum,"overlaps with next period"))
          } else { # does not overlap with next period
            # trim beginning with overlap
            # trim end with endtrim
            tSel = which(output$t >= firstperiod + overlap/2 & 
                           output$t <= lastperiod - endtrim)
            print(paste("period number",periodnum,"does not overlap with next period"))
          }
        } else { # does not overlap with previous period
          print(paste("period number",periodnum,"does not overlap with previous period"))
          if(i==nfiles){ # is it the last period?
            # trim beginning with endtrim
            # trim end with endtrim
            tSel = which(output$t >= firstperiod + endtrim & 
                           output$t <= lastperiod - endtrim)
            print(paste("period number",periodnum,"is the last period"))
          } else if(prev_overlap[i] > 0){ # does it overlap with next period?
            # trim beginning with endtrim
            # trim end with overlap
             tSel = which(output$t >= firstperiod + endtrim & 
                           output$t <= lastperiod - overlap/2)
             print(paste("period number",periodnum,"overlaps with next period"))
          } else { # does not overlap with next period
            # trim both ends with endtrim
            tSel = which(output$t >= firstperiod + endtrim & 
                           output$t <= lastperiod - endtrim)
            print(paste("period number",periodnum,"does not overlap with next period"))
          }
        }
      }
      output = output[tSel,] 
      
      # combine with previous
      if(i==1){ output_all = output 
      } else { 
        # output_all = rbind(output_all, output) 
        output_all = bind_rows(output_all, output) # should deal with diff numbers of columns, match by colname
      }
    }
  return(output_all)
}

loadproduction <- function(instance_in_fol, output_fol, overlap) {
  # instance_in_fol is the periods folder (in inputs)
  # output_fol is the output data folder
  # overlap is the number of hours of overlap between periods
  
  all_periods = list.files(path = instance_in_fol)
  for(i in 1:length(all_periods)){
    # load period info
    xx = all_periods[i]
    periodinfo = strsplit(xx,"_")[[1]]
    firstperiod = as.numeric(periodinfo[3])
    lastperiod = as.numeric(strsplit(periodinfo[4],"\\.")[[1]][1])
    periodID = paste0(periodinfo[2],"_",periodinfo[3],"_",periodinfo[4])
    
    # load and combine model output
    slow_prod = read_csv(file = paste0(output_fol,"slow_production_p",periodinfo[2],"_",periodinfo[3],"-",periodinfo[4]))
    slow_prod$speed = "slow"
    fast_prod = read_csv(file = paste0(output_fol,"fast_production_p",periodinfo[2],"_",periodinfo[3],"-",periodinfo[4]))
    fast_prod$speed = "fast"
    prod = rbind(slow_prod, fast_prod)
    
    # make t represent the hour of year, not hour of perood
    prod$t = prod$t + firstperiod -1
    
    # trim prod
    if(as.numeric(periodinfo[2]) ==1){
      # trim end
      tSel = which(prod$t <= lastperiod - overlap/2)
    } else{
      # trim both sides
      tSel = which(prod$t <= lastperiod - overlap/2 & prod$t >= firstperiod + overlap/2)
    }
    prod = prod[tSel,] 
    
    # combine with previous
    if(i==1){ prod_all = prod 
    } else { 
      prod_all = rbind(prod_all, prod)
    }
    
    # error check
    t_jumps = diff(unique(prod_all$t))
    if(sum(t_jumps >1 & t_jumps < (lastperiod - firstperiod - overlap))){
      warning("might have messed up trimming periods. Diffs in unique(t) are \n", t_jumps)
    }
  }
  return(prod_all)
}

# test3prod= loadproduction(instance_in_fol, output_fol,12)



  # loadstuff <- function(instance_in_fol, output_fol) {
  #   # instance_in_fol is the periods folder (in inputs)
  #   # output_fol is the output data folder
  #   all_periods = list.files(path = instance_in_fol)
  #   
  #   #CREATE DUMMY HOLDERS?
  #   
  #   for(i in 1:length(all_periods)){
  #     xx = all_periods[i]
  #     periodinfo = strsplit(xx,"_")[[1]]
  #     periodID = paste0(periodinfo[2],"_",periodinfo[3],"_",periodinfo[4])
  #     # makedemandgraphs(default_in_fol, instance_in_fol, output_fol, periodID,stochID,outputID)
  #   
  #   
  #   
  #     slow_commit = read_csv(file = paste0(output_fol,"slow_commitment_p",periodinfo[1],"_",periodinfo[2],"-",periodinfo[3]))
  #     
  #     timeperiods = read_csv(file = paste0(instance_in_fol,"periods_",periodID), col_names = "hour")
  #     
  #     # slow_commit = read_csv(file = paste0(output_fol,"slow_commitment.csv"))
  #     slow_commit = read_csv(file = paste0(output_fol,"slow_commitment_p",periodinfo[1],"_",periodinfo[2],"-",periodinfo[3]))
  #     slow_commit$t = timeperiods$hour
  #     
  #     slow_prod = read_csv(file = paste0(output_fol,"slow_production_p",periodinfo[1],"_",periodinfo[2],"-",periodinfo[3]))
  #     slow_prod$speed = "slow"
  #     fast_prod = read_csv(file = paste0(output_fol,"fast_production_p",periodinfo[1],"_",periodinfo[2],"-",periodinfo[3]))
  #     fast_prod$speed = "fast"
  #     prod = rbind(slow_prod, fast_prod)
  #     
  #     # load dr dispatch
  #     
  #     #MERGE FILES
  #     # removing a portion of beginning and end...
  #   }
  #   #SAVE FILES
  #   
  #   #MAKE GRAPHS
  #   
  #   #DETERMINE KEY VALUES
  #   # max first-stage committed capacity
  #   # max up and down ramp rates of rest of system (flexibility value)
  #   # co2 emissions
  #   
  # }


  # mergeOtherData()
# reads in non-timeseries data from multi-period runs
# and concatenates it into one file

# cost, cost breakdown
# TODO: save this by hour, or calculate from startup, production data


# loadRunInputs()
# given one input file for a multi-period run
# loads up the generator data and distribution data
