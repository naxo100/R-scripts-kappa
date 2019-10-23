#textwidth = 400px  /2= 233

plot_table <- function(t1,name="table-plot",xlab=NULL,ylab="Q",pdf=NULL,columns=NULL,max_value=NULL,draw_axis=c('s','s'),leg_pos=c('topright',0.01,0.03)){
	if(!is.null(pdf)){
		w <- as.double(pdf[2])
		h <- as.double(pdf[3])
		cairo_pdf(paste(pdf[1],".pdf",sep=""), width=w,height=h)
	}
	else{
		w <- 4
		h <- 4
	}
	lineas <- length(strsplit(name,'\n')[[1]])
	if(is.null(xlab))
		xlab <- colnames(t1)[1]
	op <- par(mai=c(0.65,0.66,0.60,0.1))
	coln <- length(t1[1,])
	if(is.null(max_value))
		max_value <- max(t1[,2:coln],na.rm=T)#max(as.real(t1[,2:coln]),na.rm=T)
	#print(max_value)
	if(is.numeric(t1[1,1])){
		plot(t1[,2]~t1[,1],ann=FALSE,type="n",ylim=c(0,max_value*1.05),xaxt=draw_axis[1],yaxt=draw_axis[2])
		for( column in 2:coln ){
			if(is.null(columns))
				lines(t1[,column]~t1[,1],lwd=1,col=column,type='l')
			else
				lines(t1[,column]~t1[,1],lwd=1,col=columns[[1]][column-1],type='l',lty=columns[[2]][column-1],xaxt=draw_axis[1],yaxt=draw_axis[2])
		}
	}
	else{
		plot(t1[,2],ann=FALSE,type="n",xaxt='n',yaxt=draw_axis[2],ylim=c(min(as.real(t1[,2:coln]),na.rm=T)*0.9,max_value*1.1))
		for( column in 2:coln ){
			if(is.null(columns))
				lines(t1[,column],lwd=1,col=column,type='l',xaxt=draw_axis[1],yaxt=draw_axis[2])
			else
				lines(t1[,column],lwd=1,col=columns[[1]][column-1],type='l',lty=columns[[2]][column-1],xaxt=draw_axis[1],yaxt=draw_axis[2])
		}
		axis(1,at=1:(length(t1[,1])), labels=t1[,1])
	}
	#title(name, line=NA, outer=FALSE, cex.axis=1, cex.main=0.92, cex.lab=1, family="Verdana",font.main=1)
	#mtext(name, side=3, line=1-0.8*(lineas-1),cex=1.3)
	#mtext(xlab, side=1, line=2.1,cex=1.15)
	#mtext(ylab, side=2, line=2.1,cex=1.15)
	#legend("topright",cex=1,pch=1,legend=colnames(t1)[2:length(t1)],col=2:length(t1))
	title(main=name,line=1)
	title(xlab=xlab,ylab=ylab,line=2.3)
	if(is.null(columns))
		legend(leg_pos[1],pch=1,legend=colnames(t1)[2:coln],col=2:coln,ncol=1,cex=0.8,inset=as.double(leg_pos[-1]))
	else
		legend(leg_pos[1],pch=NULL,legend=unique(colnames(t1)[2:coln]),col=columns[[1]],lty=columns[[2]],ncol=1,cex=0.8,inset=as.double(leg_pos[2:3]))
	par(op)
	if(!is.null(pdf) && !('n' %in% draw_axis))
		dev.off()
	return(max_value)
}

