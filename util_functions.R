
kappa_colnames <- function(kappa){
	conn <- file(kappa,open="r")
	splited <- strsplit(readLines(conn,1),"'|\"|#|\t")[[1]]
	cnames <- grep(".",splited,value=T)
	close(conn)
	return(cnames)
}

		
