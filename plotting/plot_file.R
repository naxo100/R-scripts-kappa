#textwidth = 400px  /2= 233

try(source("util_functions.R"))

plot_file <- function(fname,size=1,labs=NULL,pdf=NULL,max_Y=NULL, draw_axis=c('s','s'),leg_pos=c('topright',0.01,0.03),loc_legend=-1,cnames=NULL,xlims=NULL){
	cnames <- kappa_colnames(files[1])
	t1 = read.table(fname)
	if(!is.null(pdf)){
		w <- as.double(pdf[2])
		h <- as.double(pdf[3])
		cairo_pdf(paste(pdf[1],".pdf",sep=""), width=w,height=h)
	}
	side <- ceiling(sqrt(size))
	if(size == 2)
		par(mfrow=c(1,2))
	else
		par(mfrow=c(side,side))
	if(loc_legend== -1)
		loc_legend = side-1
	lineas <- length(strsplit(fname,'\n')[[1]])
	if(is.null(labs)){
		name = strsplit(fname,"[./]")[[1]]
		name = name[[length(name)-1]]
		labs <- c(name,cnames[1],"Population")
	}
	par(mar=c(1,1.5,4,0.5),oma=c(5,5,3,2))
	coln <- length(t1[1,])
	if(is.null(max_Y))
		max_Y <- max(t1[,2:coln],na.rm=T)#max(as.real(t1[,2:coln]),na.rm=T)
	#print(max_value)
	coln = (length(cnames)-1)/size
	counter = 0
	for(n in 0:(size-1)){
		
		#if (size > 1){
		#	if (counter %% side == 0 ) ploty <- "s" else ploty <- "n"
		#	if (counter >= size-side) plotx <- "s" else plotx <- "n"
		#}
		#else {
			ploty <- "s"
			plotx <- "s"
		#}
		
	
		if(!is.null(xlims)){
			last <- t1[length(t1[,1]),]
			last[,1] <- xlims[2]
			t1[length(t1[,1])+1,] <- last
		}
		else
			xlims = c(min(t1[1]),max(t1[1]))
		
		plot(t1[,2]~t1[,1],ann=FALSE,type="n",ylim=c(0,max_Y),xlim=xlims,xaxt=plotx,yaxt=ploty)
		for( column in 0:(coln-1) ) {
			lines(t1[,column+2+n*coln]~t1[,1],lwd=1,col=2+column)
		}
		#if(g == "tray-1.dat")
		leg_labels = matrix(unlist(strsplit(cnames[(2+n*coln):(1+n*coln+coln)],split=":")),nrow=2)
		title(leg_labels[1,1],outer=FALSE)
		if( counter == loc_legend)
			legend("topright",cex=1,pch=1,legend=leg_labels,col=2:(coln+1))
		counter <- counter + 1
	}
	title(labs[1],xlab=labs[2],ylab=labs[3],outer=TRUE,cex.main=2,cex.lab=2.0)
	
	if(!is.null(pdf) && !('n' %in% draw_axis))
		dev.off()
	return(max_Y)
}

