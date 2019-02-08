# submit_multi.jl
# identifies the runs that must be submitted
# by reading files in script
# writes sbatch script for them and submits
# Patricia Levi Feb 2019

function submit_multi(nperiods, runID, drID, stochID, genID)
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
                PERIODNAME = \${PERIODS[\$SLURM_ARRAY_TASK_ID]}""")
        write(f,"julia ercot_stoch.jl \$PERIODNAME $runID $drID $stochID $genID")
    end

    #export (in shell) an array of period names for sbatch
    #taken to be the names of period_*.csv file in input folder
    #assume input folder has runID name
    cdcmd = `cd ~/dr_stoch_uc/julia_ver/inputs/$runID`
    periodscmd = `export PERIODS = (ls -1 period\*.csv)` #THIS IS WRONG. how to use *?
    cdbackcmd = `cd ~/dr_stoch_uc/julia_ver/code`
    run(cdcmd)
    run(periodscmd)
    run(cdbackcmd)

    #SUBMIT
    #submitargs = [runID] #maybe this isnt needed bc args are already in sbatch script
    submit_command = `sbatch $runID.sbatch` #$submitargs`
    run(submit_command)
end
