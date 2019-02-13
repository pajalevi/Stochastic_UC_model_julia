# submit_multi.jl
# identifies the runs that must be submitted
# by reading files in script
# writes sbatch script for them and submits
# Patricia Levi Feb 2019

# Currently, the 'run' commands dont seem to work very well
# a few options:
# 1) look into soln at bottom of https://discourse.julialang.org/t/using-as-a-wildcard-in-backtick-commands/6094/4
# 2) write a shell script that creates PERIODS and calls the SBATCH script (created statically)
# 3) make the SBATCH script statically and create PERIODS by hand
# NB that the script which creates PERIODS must be at the same or higher hierarchical
#   level as the sbatch script - cannot call a script which creates PERIODS and then
#   call sbatch - script would have to create periods and call sbatch
#   and if sbatch is dynamically named then the shell script has to be dynamically written
# currently 2 and 3 look the most likely -- will have to test this in real time

function submit_multi(nperiods, paramsID, drID, stochID, genID, multiTF)
    # WRITE SBATCH SCRIPT
    # saved in julia_ver/code
    open("$runID.sbatch","w") do f
        write(f,"#!/bin/bash\n \n")
        write(f,string("#SBATCH --job-name = ",runID," \n"))
        write(f,string("#SBATCH --output = ",runID,"""_%A_%a.out
                #SBATCH --error = """,runID,"_%A_%a.err \n"))
        write(f,"""#SBATCH --time=3:00:00
                #SBATCH -p normal
                #SBATCH -mem = 64G
                #SBATCH --mail-type = END,FAIL
                #SBATCH --mail_user=pjlevi@stanford.edu

                cd ~/dr_stoch_uc/julia_ver/code
                module load julia 0.6.4
                module load gurobi \n
                PERIODNAME = \${PERIODS[\$SLURM_ARRAY_TASK_ID]}
                DATE=`date '+%Y-%m-%d_%H-%M-%S'` \n""")
        write(f,"julia ercot_stoch.jl \$DATE $paramsID $multiTF \$PERIODNAME")
    end

    #export (in shell) an array of period names for sbatch
    #taken to be the names of period_*.csv file in input folder
    #assume input folder has runID name
    cdcmd = `cd ~/dr_stoch_uc/julia_ver/inputs/$runID`
    periodscmd = `export PERIODS = (\$(ls -1 period\*.csv))` #THIS IS WRONG. how to use *?
    cdbackcmd = `cd ~/dr_stoch_uc/julia_ver/code`
    run(cdcmd)
    run(periodscmd)
    run(cdbackcmd)

    #SUBMIT
    #submitargs = [runID] #maybe this isnt needed bc args are already in sbatch script
    submit_command = `sbatch $runID.sbatch` #$submitargs`
    run(submit_command)
end
