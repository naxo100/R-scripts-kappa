

toSize <- function(x,size){
	r <- (0:(size-1))*(length(x)-1)/(size-1)+1
	for(i in 1:size){
		val <- r[i]
		val_r <- val %% 1
		val_i <- val - val_r
		if( val_r %% 1 != 0 )
			r[i] <- x[val_i]*(1-val_r) + x[val_i+1]*val_r
		else
			r[i] <- x[val_i]
	}
	return(r)
}


all_correlations <- function(sim_ka_file,sims_cka_files){
	source("MINE.R")
	source("average_data.R")
	sim_ka <- read.table(sim_ka_file)
	correlations <- vector(mode="list",length=length(sims_cka_files))
	i <- 1
	for(sim_cka_file in sims_cka_files){
		sim_cka <- read.table(sim_cka_file)
		coln_ka <- length(sim_ka)
		coln_cka <- length(sim_cka)
		rown_ka <- length(sim_ka[,1])
		rown_cka <- length(sim_cka[,1])
		sim_ka <- fill_rows(sim_ka,rown_cka)
		diff <- coln_ka - coln_cka
		if(coln_ka != coln_cka)
			print(paste("Number of columns dont match (",diff,")",sep=""))
		corr_mat <- intercalar(sim_ka[(diff+2):coln_ka],sim_cka[2:coln_cka])
		#print(corr_mat[,1:5])
		name <- grep(".",strsplit(sim_cka_file,"/|e-|[.]dat")[[1]],value=T)
		name <- name[length(name)]
		mic_file <- paste("../results/mic/",grep("-",strsplit(sim_ka_file,"/|e-|[.]dat|KaSim")[[1]],value=T)[1],name,sep="")
		rMINE(corr_mat,mic_file,"adjacent.pairs",max.num.boxes.exponent=0.6,num.clumps.factor=20,notify.wait=30000)
		mic_file <- paste(mic_file,"adjacentpairs,cv=0.0,B=n^0.6,Results.csv",sep=",")
		
		mic <- colMeans(read.csv(mic_file)[3])
		
		kendall <- c()
		spearman <- c()
		pearson <- c()
		for(k in  0:(length(corr_mat[,1])/2-1)){
			kendall <- c(kendall,cor(corr_mat[2*k+1,],corr_mat[2*k+2,],method = "kendall"))
			spearman <- c(spearman,cor(corr_mat[2*k+1,],corr_mat[2*k+2,],method = "spearman"))
			pearson <- c(pearson,cor(corr_mat[2*k+1,],corr_mat[2*k+2,],method = "pearson"))
		}
		correlations[[i]] <- c(kendall = mean(kendall), spearman = mean(spearman), pearson = mean(pearson), MIC=mic)
		names(correlations)[i] <- name
		names(correlations[[i]]) <- c("kendall","spearman","pearson","mic")
		i <- i+1
	}

	return( correlations )
}


intercalar <- function(tabla1,tabla2){
	coln <- min(length(tabla1),length(tabla2))
	rown <- min(length(tabla1[,1]),length(tabla2[,1]))
	mat <- matrix(nrow=2*coln,ncol=rown)
	mat[] <- 0
	tabla1 <- tabla1[sort(names(tabla1))]
	tabla2 <- tabla2[sort(names(tabla2))]
	for(i in 0:(coln-1)){
		print(paste(names(tabla1[i+1]),"VS",names(tabla2[i+1])))
		mat[2*i+1,] <- tabla1[1:rown,i+1]
		mat[2*i+2,] <- tabla2[1:rown,i+1]
	}
	return(mat)
}
