#' Run Lingo Codes
#' 
#' Run Lingo Codes and return the results.
#' 
#' This function is used to execute Lingo codes by RunLingo.exe and get the
#' results by system function in R.
#' 
#' @param file the filename of SQL codes.
#' @param lingo the directory of executive Lingo file.
#' @param intern a logical (not NA) which indicates whether to capture the 
#' output of the command as an R character vector.
#' @param ... arguments to be passed to \code{\link{system}}.
#' @return The results of Lingo as character vector.
#' @export
#' 
runLingo=function(file,lingo="d:/LINGO10/RunLingo.exe",intern=TRUE,...){
    system(paste(lingo,file),intern=intern,...)
}

#' Write Lingo Data
#' 
#' Write Lingo data into a text file.
#' 
#' This function is used to convert the data list in R into a data file
#' which can be imported by Lingo.
#' 
#' @param datalist the data list for the Lingo data file.
#' @param ldt the Lingo data file.
#' @return A Lingo data file contains all data which can be imported by 
#' Lingo code.
#' @export
#' 
writeLingo=function(datalist,ldt="lingo.txt"){
    d1=lapply(datalist,t)
    d2=lapply(1:length(d1),function(x) paste(d1[[x]],collapse=","))
    d3=unlist(paste(d2,"~",sep=""))
    d4=gsub(" ","",d3)
    writeLines(d4,ldt)
}

#' Extract Lingo Results
#' 
#' Extract Lingo results into R list.
#' 
#' This function is used to translate Lingo results into a list which can be
#' easily used by R.
#' 
#' @param res the result of \code{\link{runLingo}}.
#' @return A list which includes "solution", "objective", "iterations", 
#' "values", "price".
#' @export
#' 
extractLingo=function(res){
    ans=list()
    # Solution
    ind=grep("solution",res)
    ans$solution=gsub("^\\s+","",res[ind[1]])    
    # Objective
    ind=grep("Objective",res)
    nvar=ifelse(length(ind),4,3)
    ans$objective=as.numeric(gsub("[^:]+:","",res[ind]))
    # Iterations
    ind=grep("iterations",res)
    ans$iterations=as.numeric(gsub("[^:]+:","",res[ind]))
    # Variable & Reduced Cost
    blank=which(res==" ")
    ind=grep("Variable",res)+1
    end=min(blank[blank>ind])-1
    res1=gsub("^\\s+","",res[ind:end])
    # avoid variable without index
    idx=which(!grepl("\\(",res1))
    res1[idx]=gsub("\\s{2,}","\\( 0\\) ",res1[idx])
    res1=gsub("\\s{2,}"," ",res1)
    res1=gsub("[()]","",res1)
    res1=gsub(", ",",",res1)
    ans$values=t(as.data.frame(strsplit(res1," ")))
    rownames(ans$values)=NULL
    colnames(ans$values)=c("variable","index","value",
                           "reduced.cost")[1:nvar]
    ans$values=as.data.frame(ans$values)
    ans$values[,1:2]=apply(ans$values[,1:2,drop=F],2,as.character)
    ans$values[,3:nvar]=apply(apply(ans$values[,3:nvar,drop=F],2,
                                    as.character),2,as.numeric)
    # Slack or Surplus & Dual Price
    ind=grep("Slack or Surplus",res)+1
    if(length(ind)){
        end=min(blank[blank>ind])-1
        res1=gsub("^\\s+","",res[ind:end])
        res1=gsub("\\s{2,}",",",res1)
        res1=gsub(" ","",res1)
        ans$price=t(as.data.frame(strsplit(res1,",")))
        ans$price=data.frame(ans$price)
        rownames(ans$price)=NULL
        colnames(ans$price)=c("row","slack.surplus",
                              "dual.price")[1:(nvar-1)]
        for(i in 2:dim(ans$price)[2])
            ans$price[,i]=as.numeric(ans$price[,i])
    }else{
        ans$price=numeric(0)
    }    
    # Return
    ans
}

#' Seperate the Extracted Values
#' 
#' Seperate the extracted values of Lingo results by each dimmention.
#' 
#' This function is used to seperate values of Lingo results into a list which 
#' can be easily used by R.
#' 
#' @param eres.values values of Lingo results from \code{\link{extractLingo}}.
#' @return A list which includes "dim1", "dim2", ...
#' @export
#' 
dimsep=function(eres.values){
    dims=unlist(lapply(gregexpr(",",eres.values$index),function(x) 
        ifelse(x==-1,0,length(x))))+1
    ndim=sort(unique(dims))
    ans=vector("list",length(ndim))
    names(ans)=paste("dim",ndim,sep="")
    if(any(dims==1)) ans[[1]]=eres.values[dims==1,]
    for(i in ndim[ndim>1]){
        ind=t(as.data.frame(strsplit(as.character(
            eres.values$index[dims==i]),",")))
        rownames(ind)=NULL
        colnames(ind)=paste("index",1:i,sep="")
        ans[[i]]=cbind(eres.values[dims==i,-2],ind)
        idim=dim(ans[[i]])[2]
        ans[[i]]=ans[[i]][,c(1,(idim-1):idim,2:(idim-2))]
    }
    # Return
    ans
}