
elem_of_files <- function(folder='.',ptrn='*',col_n=NULL,row_n=NULL){
	sims_files <- list.files(path=folder,pattern=ptrn,recursive=FALSE,full.names=TRUE)
	l <- c()
	for(file in sims_files){
		dtable <- read.table(file)
		if(is.null(col_n))
			col_n <- length(dtable)
		if(is.null(row_n))
			row_n <- length(dtable[,1])
		l <- c(l,dtable[row_n,col_n])
	}
	return(l)
}
