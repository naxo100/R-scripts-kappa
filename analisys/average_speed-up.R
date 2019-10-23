
average_time <- function(time_file,ignore=-as.real('inf')){
	conn <- file(time_file,open="r")
	elapsed <- grep("elapsed",readLines(conn),value=T)
	times <- as.real(matrix(unlist(strsplit(elapsed,"\t")),,2,T)[,2])
	close(conn)
	return(mean(times[times > ignore]))
}


average_sim_time <- function(files){
	t <- 0
	for(f in files()){
		tabla <- read.table(f)
		t <- t + tabla[length(tabla[,1]),1]
	}
	return(t/length(files))	
}

average_events <- function(files){
	E <- 0
	for(f in files()){
		conn <- file(f,open="r")
		events <- grep("events",readLines(conn),value=T)
		e <- as.real(matrix(unlist(strsplit(events," ")),,2,T)[,2])
		E <- E + e
		close(conn)
	}
	return(e / length(files))
}

average_activities <- function(outputs){
	acts <- 0
	for(output in outputs){
		conn <- file(output,open="r")
		lines <- readLines(conn)
		act0 <- grep("0 Activity's",lines,value=T)
		act0 <- as.real(matrix(unlist(strsplit(act0,":")),,2,T)[,2])
		act1 <- grep("1 Activity's",lines,value=T)
		act1 <- as.real(matrix(unlist(strsplit(act1,":")),,2,T)[,2])
		acts <- acts+act1/act0
		close(conn)
	}
	return(acts/length(outputs))
}





