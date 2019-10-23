#source("util_functions.R")

sum_folder <- function(folder=".",ptrn="*",dtable=NULL,cnames=NULL){
	piska_files <- list.files(path=folder,recursive=FALSE,full.names=TRUE,pattern=ptrn)
	
	if(is.null(dtable)){
		dtable <- read.table(piska_files[1],col.names=cnames)
	}
	coln <- length(dtable[1,])
	dtable[2:coln] <- dtable[2:coln] * 0
	for( cmp in piska_files ){ 
		try (dtable2 <- read.table(cmp)[2:coln] , print(cmp) )
		if(!is.null(cnames))
			col.names(dtable2) <- cnames
		for(elem in dtable2[,2])
			if(is.na(elem))
				print(cmp)
		m <- min(length(dtable[,1]),length(dtable2[,1]))
		dtable[1:m,2:coln] <- dtable[1:m,2:coln] + dtable2[1:m,]
	}
	return(dtable)
}

concatenate_folder <- function(folder=".",ptrn="*",dtable=NULL,cnames=NULL,skip=1,columns=NULL){
	piska_files <- list.files(path=folder,recursive=FALSE,full.names=TRUE,pattern=ptrn)
	if(is.null(dtable)){
		dtable <- read.table(piska_files[1])
	}
	coln <- length(dtable)
	if(is.null(columns))
		columns <- 1:coln
	else
		coln <- length(columns)
	dtable <- dtable[columns]
	#dtable[2:coln] <- dtable[2:coln] * 0
	for(col in 1+skip:coln)
		dtable[,1+skip] <- NULL
	if(is.null(cnames))
		cnames <- kappa_colnames(piska_files[1])[columns]
	names(dtable)[1:skip] <- cnames[1:skip]
	for( cmp in piska_files ){ 
		try (dtable2 <- read.table(cmp)[columns][(skip+1):coln] , print(cmp) )
		fname <- strsplit(cmp,"/|[.]")[[1]]
		fname <- fname[length(fname)-1]
		names(dtable2) <- paste(fname,cnames[(skip+1):coln],sep=":")
		for(elem in dtable2[,1])
			if(is.na(elem))
				print(cmp)
		m <- min(length(dtable[,1]),length(dtable2[,1]))
		coln_sum <- length(dtable)
		#print(dtable[1:4,])
		dtable[,colnames(dtable2)] <- dtable2[1:m,]
	}
	return(dtable)
}

average_folders <- function(folder=".",ptrn="*",concatenate=T,cnames=NULL){
	sims_dirs <- list.dirs(path=folder,recursive=FALSE,full.names=FALSE)
	dtable <- read.table(
				list.files(sims_dirs[1],recursive=FALSE,full.names=TRUE,pattern=ptrn)[1]
	)
	if(!is.null(cnames))
		colnames(dtable) <-cnames
	#else
	#	colnames(dtable) <- kappa_colnames()
	#dtable[2:coln] <- dtable[2:coln] * 0
	total_table <- NULL
	if(concatenate)
		func <- concatenate_folder
	else
		func <- sum_folder
	count <- 0
	for(sim_dir in sims_dirs){
		try({
		if(is.null(total_table)){
			total_table <- func(sim_dir,ptrn,dtable,cnames)
			coln <- length(total_table)
		}
		else
			total_table[2:coln] <- total_table[2:coln] + func(sim_dir,ptrn,dtable,cnames)[2:coln]
		count <- count + 1})
	}
	total_table[2:coln] <- total_table[2:coln]/count
	return(total_table)
}

max_rown_table <- function(sims_files){
	max_row <- 0
	for(sim_file in sims_files){
		dtable <- read.table(sim_file)
		rown <- length(dtable[,1])
		if(rown > max_row){
			max_row <- rown
			max_table <- dtable
		}
	}
	return(max_table)
}

fill_rows <- function(dtable,rows){
	rown <- length(dtable[,1])
	for(r in rown:rows)
		dtable[r,] <- dtable[rown,]
	return(dtable)
}

average_files <- function(folder=".",ptrn="*",cnames=NULL){
	sims_files <- list.files(path=folder,pattern=ptrn,recursive=FALSE,full.names=TRUE)
	dtable <- max_rown_table(sims_files)
	if(!is.null(cnames))
		colnames(dtable) <-cnames
	else
		colnames(dtable) <- kappa_colnames(sims_files[1])
	coln <- length(dtable)
	rown <- length(dtable[,1])
	dtable[2:coln] <- dtable[2:coln] * 0
	total_table <- dtable
	for(sim_file in sims_files){
		dtable_buff <- fill_rows(read.table(sim_file),rown)
		total_table[2:coln] <- total_table[2:coln] + dtable_buff[2:coln]
	}
	total_table[2:coln] <- total_table[2:coln]/length(sims_files)
	return(total_table)
}

avg_sdev_files <- function(folder="*",ptrn="*",cnames=NULL){
	avg_table <- average_files(folder,ptrn,cnames)
	sims_files <- list.files(path=folder,pattern=ptrn,recursive=FALSE,full.names=TRUE)
	dtable <- max_rown_table(sims_files)
	if(!is.null(cnames))
		colnames(dtable) <-cnames
	else
		colnames(dtable) <- kappa_colnames(sims_files[1])
	coln <- length(dtable)
	rown <- length(dtable[,1])
	dtable[2:coln] <- dtable[2:coln] * 0
	sdev_table <- dtable
	for(sim_file in sims_files){
		dtable_buff <- fill_rows(read.table(sim_file),rown)
		sdev_table[2:coln] <- sdev_table[2:coln] + (avg_table[2:coln] - dtable_buff[2:coln])**2
	}
	sdev_table[2:coln] <- (sdev_table[2:coln]/length(sims_files))**0.5
	return(list(avg_table,sdev_table))
}


sum_columns <- function(cols,coln=1){
	if(length(cols) > coln)
		for(i in 1:(length(cols)/coln-1))
			cols[1:coln] <- cols[1:coln]+cols[(1+coln*i):(coln*(i+1))]
	return(cols[1:coln])
}















