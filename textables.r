#!/usr/bin/env Rscript
convertlistreg2matrix=function(listreg,roundnum=4){
    listreg2=vector("list",length(listreg))
    for(i in 1:length(listreg)){
        coefsum=summary(listreg[[i]])$coefficients
        coef=coefsum[,1]
        se=coefsum[,2]

        for(j in 1:length(se)){
            se[j]=round(as.numeric(se[j]),digits=roundnum)
        }

        #stars
        vec=coefsum[,4]
        vec2=rep("",length(vec))
        vec2=ifelse(vec<0.05,paste(vec2,'*',sep=""),vec2)
        vec2=ifelse(vec<0.01,paste(vec2,'*',sep=""),vec2)
        vec2=ifelse(vec<0.001,paste(vec2,'*',sep=""),vec2)

        for(j in 1:length(coef)){
            coef[j]=paste(round(as.numeric(coef[j]),digits=roundnum),vec2[j],sep="")
        }

        resultsmat=matrix(c(coef,se),,2)
        rownames(resultsmat)=rownames(coefsum)

        listreg2[[i]]=resultsmat
    }
    return(listreg2)
}


#Function where take rownames of two columns and convert into standard format


# listcoef=c('trendpageviews','shock1','shock2','shock30','shock60','shock90','shock120','shock150','shock180')
convertlistmatrixreg2tex=function(listregmatrix,keepvars){
    line=''
    for(coef in keepvars){
        line=paste(line,coef)
        #add values if exist
        for (i in 1:length(listregmatrix)){
            line=paste(line,'&') #do this regardless since may want empty space

            #line on coefficient
            if (coef %in% rownames(listregmatrix[[i]])){
                line=paste(line,listregmatrix[[i]][coef,1])
            }
        }
        line=paste(line,'\\\\ \n')


        #line on standard error
        #add values if exist
        for (i in 1:length(listregmatrix)){
            line=paste(line,'&')

            #line on coefficient
            if (coef %in% rownames(listregmatrix[[i]])){
                line=paste(line,listregmatrix[[i]][coef,2])
            }
        }
        line=paste(line,'\\\\ \n')
    }
    return(line)
}
texoutput=function(listreg,keepvars){
    convertlistmatrixreg2tex(convertlistreg2matrix(listreg),keepvars)
}
