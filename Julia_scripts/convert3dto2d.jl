#convert3dto2d
# function that flattens a 3d array to a 2d dataframe with colnames
# and adds an additional column to indicate which slice of the lost
# dimension each row came from, and another additional column to indicate
# which row of the lost dimension
# Patricia Levi July 2018

function convert3dto2d(array3d,ind_dim,x_dim,y_dim,colnames,indnames)
    # ---- inputs: array, dimension that gets the column indicator,
    # ----         dimension to be vertical, dimension to be horizonal
    # ----         set of column names
    # ---- checks that the lengths of testnames, DRNAMES are appropriate
    # ---- and returns a dataframe
    if length(indnames) != size(array3d,ind_dim)
        error("indnames must have same length as ind_dim")
    end
    if length(colnames) != size(array3d,x_dim)+2
        error("colnames must be one longer than x_dim")
    end

    array2d = Array{}
    for j in 1:size(array3d,ind_dim)
        newdf = array3d[j,:,:]
        if j>1
            # print("try to append to array2d \n")
            array2d = vcat(array2d,hcat(newdf,fill(indnames[j],size(array3d,2)),[1:size(array3d,2)...]))
        else
            array2d = hcat(newdf,fill(indnames[j],size(array3d,2)),[1:size(array3d,2)...])
            # print("array2d initialized\n")
        end
    end

    df2d = DataFrame(array2d)
    rename!(df2d,[Symbol("$input") for input in colnames])

    return df2d
end

# TESTING
# in this example, ind_dim = 1, y_dim = 2, x_dim = 3
# test = reshape([1:(12*5*2)...],(2,12,5))
# testnames = ["o1","o2","o3","o4","o5","DR_IND"]
# DRNAMES = ["dr1","dr2"]
# xx = convert3dto2d(test,1,3,2,testnames,DRNAMES)
# CSV.write(string(default_fol,"/test2.csv"),xx)

# desired output is a 12x6 dataframe with above colnames
# for j in 1:size(test)[1] #iterate over each generator
#     newdf = test[j,:,:]
#     # dftest[j] = reshape(test[j,:,:], size(test)[2]*size(test)[3])
#     if j>1
#         dftest = vcat(dftest,hcat(newdf,fill(DRNAMES[j],size(test,2))))
#     else
#         dftest = hcat(newdf,fill(DRNAMES[j],size(test,2)))
#     end
# end
# dfend = DataFrame(dftest)
# names!(dfend,[Symbol("$input") for input in testnames])
