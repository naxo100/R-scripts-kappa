
try(source("util_functions.R"))

plot_folder <- function(folder=".",ptrn="*.out",labs=c("Title","X","Y"),cnames=NULL,loc_legend=1,xlim=NULL){
	files <- list.files(path=folder,recursive=FALSE,full.names=TRUE,pattern=ptrn)
	if(is.null(cnames))
		cnames <- kappa_colnames(files[1])
	coln <- length(cnames)
	size <- length(files)
	size_h <- ceiling(sqrt(size))
	size_w <- ceiling(size/size_h)
	if(size == 2)
		par(mfrow=c(1,2))
	else
		par(mfrow=c(size_w,size_h))
	par(mar=c(1,1.5,1,0.5),oma=c(5,5,3,2))
	counter <- 0
	tablas <- list()
	max_Y <- 0
	for(file in files){
		fname <- strsplit(file,"/")[[1]]
		fname <- fname[length(fname)]
		buff <- strsplit(fname,"[.]")[[1]]
		name <- buff[1]
		
		tabla <- read.table(file,col.names=cnames)
		tabla <- tabla#[1:700,]
		
		max_Y <- max(max_Y,max(tabla[2:length(tabla)],na.rm = TRUE))
		tablas[[length(tablas)+1]] <- list(name,tabla)
	}
	#return(tablas)
	for(tabla in tablas){
		name <- tabla[[1]]
		t1 <- tabla[[2]]
		
		if (size > 1){
			if (counter %% size_h == 0 ) ploty <- "s" else ploty <- "n"
			if (counter >= size-size_h) plotx <- "s" else plotx <- "n"
		}
		else {
			ploty <- "s"
			plotx <- "s"
		}
		
	
		if(!is.null(xlim)){
			last <- t1[length(t1[,1]),]
			last[,1] <- xlim[2]
			t1[length(t1[,1])+1,] <- last
		}
		
		plot(t1[,2]~t1[,1],ann=FALSE,type="n",ylim=c(0,max_Y),xlim=xlim,xaxt=plotx,yaxt=ploty)
		for( column in 2:length(t1) ) {
			lines(t1[,column]~t1[,1],lwd=1,col=column)
		}
		#if(g == "tray-1.dat")
	
		title(paste(name,sep=""),outer=FALSE)
		if( counter+1 == loc_legend)
			legend("topright",cex=1,pch=1,legend=colnames(t1)[2:length(t1)],col=2:length(t1))
		counter <- counter + 1
	}
	title(labs[1],xlab=labs[2],ylab=labs[3],outer=TRUE,cex.main=2,cex.lab=2.5)
	#return(t1)
}


