
QUEUE1="geminis"
QUEUE2="geminis"

cd ..
cd validity 

cd pred-prey-PISKa
./pred-prey_validity.sh 9 1 1-1000 1.0 $QUEUE1
./pred-prey_validity.sh 9 1 1-1000 0.2 $QUEUE1
./pred-prey_validity.sh 9 1 1-1000 0.05 $QUEUE1
./pred-prey_validity.sh 9 1 1-1000 0.01 $QUEUE1
cd ..

cd clock-PISKa
./clock_validity.sh 2 1 1-1000 0.5 $QUEUE1
./clock_validity.sh 2 1 1-1000 0.1 $QUEUE1
./clock_validity.sh 2 1 1-1000 0.02 $QUEUE1
./clock_validity.sh 2 1 1-1000 0.005 $QUEUE1
cd ..

cd ..


cd speed-up

cd clock-KaSim
./clock_speed-up.sh 1-1000 $QUEUE1
cd ..

cd pred-prey9-KaSim
./pred-prey9_speed-up.sh 1-1000 $QUEUE1
cd ..

cd clock-PISKa
./clock_speed-up.sh 2 1 1-400 0.5 $QUEUE1
./clock_speed-up.sh 2 1 1-400 0.1 $QUEUE1
./clock_speed-up.sh 2 1 1-400 0.02 $QUEUE1
./clock_speed-up.sh 2 1 1-400 0.005 $QUEUE1

./clock_speed-up.sh 2 2 1-400 0.5 $QUEUE2
./clock_speed-up.sh 2 2 1-400 0.1 $QUEUE2
./clock_speed-up.sh 2 2 1-400 0.02 $QUEUE2
./clock_speed-up.sh 2 2 1-400 0.005 $QUEUE2
cd ..

cd pred-prey-PISKa
./pred-prey_speed-up.sh 9 1 1-400 1.0 $QUEUE1
./pred-prey_speed-up.sh 9 1 1-400 0.2 $QUEUE1
./pred-prey_speed-up.sh 9 1 1-400 0.05 $QUEUE1
./pred-prey_speed-up.sh 9 1 1-400 0.01 $QUEUE1
cd ..

cd ..
cd scripts

#data mining
R --no-save <<< "
	source('average_data.R')
	KaSim_exps <- c('pred-prey9-KaSim',
			'clock-KaSim',
			'ABC-KaSim')
	PISKa_exps <- c('pred-prey-PISKa',
			'clock-PISKa',
			'ABC-PISKa',
			'clock_diffusion-PISKa')
	
	KaSim_data <- paste('../validity',KaSim_exps,'raw-data',sep='/')
	for (exp in KaSim_data){
		avrg_table <- average_files(exp,'*[0-9].out')
		name <- grep('.',strsplit(exp,'[/]')[[1]],value=T)
		name <- name[length(name)-1]
		write.table(avrg_table,
			file=paste(exp,'/average-',name,'.dat',sep= ''))
	}
	
	PISKa_data <- paste('../validity',PISKa_exps,'raw-data',sep='/')
	for (exp in PISKa_data){
		folders <- list.dirs(exp,recursive=FALSE,full.names=TRUE)
		for(folder in folders){
			avrg_table <- average_folders(folder,'*.out')
			name <- strsplit(folder,'/')[[1]]
			name <- name[length(name)]
			write.table(avrg_table,
				file=paste(folder,'/../average-',name,'.dat',sep= ''))
		}
	}
	
	source('correlations_func.R')
	correlations <- vector(mode='list',length=length(KaSim_exps))
	for(i in 1:length(KaSim_exps)){
		KaSim_avg <- list.files(KaSim_data[i],recursive=FALSE,full.names=TRUE,pattern='average-*')[1]
		PISKa_avgs <- list.files(PISKa_data[i],recursive=FALSE,full.names=TRUE,pattern='average-*')
		corr <- 0
		try(corr <- all_correlations(KaSim_avg,PISKa_avgs),silent=FALSE)
		correlations[i] <- list(corr)
		name <- strsplit(KaSim_avg,'/')[[1]]
		names(correlations)[i] <- name[length(name)]
	}
	
	#special clock models
	source('average_data.R')
	clock_avgs <- list.files(PISKa_data[4],recursive=FALSE,full.names=TRUE,pattern='^average-.*')
	for(clock_file in clock_avgs){
		clock_data <- read.table(clock_file)
		coln <- length(clock_data)
		nucleus_cells <- grep('.1..1..1.|nucleus',names(clock_data))
		cytosol_cells <- grep('.1..1..1.|nucleus',names(clock_data),invert=TRUE)[-1]
		clock_data[2:9] <- sum_columns(clock_data[cytosol_cells],8) 
		clock_data[10:17] <- sum_columns(clock_data[nucleus_cells],8) 
		names(clock_data)[10:17] <- names(clock_data[nucleus_cells[1:8]])
		clock_data[18:coln] <- list(NULL)
		file_splt <- strsplit(clock_file,'/')[[1]]
		splt_len <- length(file_splt)
		name <- file_splt[splt_len]
		write.table(clock_data,	
			file=paste(c(file_splt[1:(splt_len-1)],paste('sum',name,sep='-')),collapse='/'))
	}
	
	KaSim_avg <- list.files(KaSim_data[2],recursive=FALSE,full.names=TRUE,pattern='average-*')[1]
	PISKa_avgs <- list.files(PISKa_data[4],recursive=FALSE,full.names=TRUE,pattern='sum-average-*')
	corr <- 0
	try(corr <- all_correlations(KaSim_avg,PISKa_avgs),silent=FALSE)
	correlations[length(correlations)+1] <- list(corr)
	name <- strsplit(KaSim_avg,'/')[[1]]
	names(correlations)[length(correlations)] <- name[length(name)]

	#SPEED-UPS
	source('average_speed-up.R')
	speedups <- vector(mode='list',length=length(KaSim_exps))
	KaSim_t <- paste('../speed-up',KaSim_exps,'raw-data','times.out',sep='/')
	for(i in 1:length(KaSim_t)){
		tryCatch({KaSim_avg <- average_time(KaSim_t[i],5.0)},warning=function(e){next})
		PISKa_outs <- paste('../speed-up',PISKa_exps[i],'raw-data',sep='/')
		PISKa_t <- list.files(PISKa_outs,recursive=TRUE,full.names=TRUE,pattern='times.out')
		PISKa_avg <- list(c('KaSim'=KaSim_avg))
		for(time_file in PISKa_t){
			time <- average_time(time_file,8.0)
			ts <- c('time'=time,'speed-up'=KaSim_avg/time)
			PISKa_avg[length(PISKa_avg)+1] <- list(ts)
			name <- strsplit(time_file,'/')[[1]]
			name <- name[length(name)-1]
			names(PISKa_avg)[length(PISKa_avg)] <- c(name)
		}
		speedups[i] <- list(PISKa_avg)
		name <- strsplit(KaSim_exps[i],'/')[[1]]
		names(speedups)[i] <- name[length(name)]
	}
	
	#SPECIAL CLOCK MODELS
	tryCatch({KaSim_avg <- average_time(KaSim_t[2],5.0)},warning=function(e){next})
	PISKa_t <- paste('../validity',PISKa_exps[4],'raw-data',sep='/')
	PISKa_out <- list.files(PISKa_t,recursive=TRUE,full.names=TRUE,pattern='times.out')
	PISKa_avg <- list(c('KaSim'=KaSim_avg))
	for(time_file in PISKa_out){
		time <- average_time(time_file,10.0)
		name <- strsplit(time_file,'/')[[1]]
		name <- name[length(name)-1]
		ts <- c('time'=time,'speed-up'=KaSim_avg/time)
		PISKa_avg[length(PISKa_avg)+1] <- list(ts)
		names(PISKa_avg)[length(PISKa_avg)] <- c(name)
	}
	speedups[4] <- list(PISKa_avg)
	name <- strsplit(PISKa_exps[4],'/')[[1]]
	names(speedups)[4] <- name[length(name)]
	
	
	#ACTIVITIES
	outs <- list.files('../speed-up/clock-activity/raw-data/n2-N1-h0.02',recursive=T,full.names=T,pattern='output')
	acts <- average_activities(outs)
	print('average activity ratio: ')
	mean(acts)
	
	
	#PLOTS
	source('plot_table.R')
	l <- 1:(72/0.02)*0.02
	mat <- matrix(c(l,acts),ncol=2)
	colnames(mat) <- c('Horas','Cuociente\nCitosol/Núcleo')
	plot_table(as.table(mat),xlab='Horas',ylab='Cuociente reactividad',name='Tasas de reactividad\nentre compartimientos',pdf=c('../plots/reactivity',6,4.3))
	
	cnames <- c('time','mRNA(PER1)','mRNA(PER2)',
		'mRNA(CRY1)','mRNA(CRY2)','PER1','PER2' ,
		'CRY1','CRY2')
	
	tab <- read.table('../validity/clock-KaSim/raw-data/average-clock-KaSim.dat')
	sum_tab <- tab[1]
	sum_tab[2:9] <- sum_columns(tab[2:17],8)
	names(sum_tab) <- cnames
	plot_table(sum_tab,name='Ciclo Circadiano\nSimulación con KaSim',xlab='Horas',ylab='Concentración',pdf=c('../plots/clock-KaSim',7,3.8))
	
	for(h in c(0.02,0.1,0.5)){
		tab <- read.table(paste('../validity/clock-PISKa/raw-data/average-n2-N1-h',h,'.dat',sep=''))
		sum_tab <- tab[1]
		sum_tab[2:9] <- sum_columns(tab[2:17],8)
		names(sum_tab) <- cnames
		plot_table(sum_tab,name=paste('Ciclo Circadiano\nSimulación con PISKa (h=',h,')',sep=''),xlab='Horas',ylab='Concentración',pdf=c(paste('../plots/clock-h',h,sep=''),7,3.8))
	}
	
	ns <- c('n3','n5','n9','n17','n18','n27','n28','n29')
	ups <- c('n2',ns)
	for(h in c('h0.02')){
		for(d in c('diff0.01','diff0')){
			sims <- paste(ns,'N1',h,d,sep='-')
			ups <- c(ups,speedups$'clock-KaSim'[[paste('n2-N1-',h,sep='')]][[2]])
			for(sim in speedups$'clock_diffusion-PISKa'[sims])
				if(is.null(sim))
					ups <- c(ups,NA)
				else
					ups <- c(ups,sim[[2]])
		}
	}
	ups_tab <- matrix(ups,nrow=length(ns)+1)
	colnames(ups_tab) <- c('CPUs','h=0.02, d=0.01','h=0.02, d=0')
	plot_table(as.table(ups_tab),name='Speed-Up de PISKa en base al bio-tiempo\nModelo ciclo circadiano',ylab='Speed-Up',pdf=c('../plots/speedup-biotime',5,3.8))
	
	
	ns <- c('n4','n9','n16','n25','n36','n64','n121')
	ups <- ns
	for(h in c('h0.2')){
		for(N in c('N1','N2')){
			sims <- paste(ns,N,h,sep='-')
			#ups <- c(ups)
			for(sim in speedups$'pred-prey9-KaSim'[sims])
				if(is.null(sim))
					ups <- c(ups,NA)
				else
					ups <- c(ups,sim[[2]])
		}
	}
	ups_tab <- matrix(ups,nrow=length(ns))
	colnames(ups_tab) <- c('CPUs','N1-h0.2','N2-h0.2')
	plot_table(as.table(ups_tab),name='Speed-Up de PISKa en base a eventos\nModelo predador-presa',ylab='Speed-Up',pdf=c('../plots/speedup-events',5,3.8))
	
	tab <- read.table('../validity/clock-KaSim/raw-data/average-clock-KaSim.dat')
	sum_tab <- tab[1]
	sum_tab[2:9] <- sum_columns(tab[2:17],8)
	names(sum_tab) <- cnames
	comparative <- sum_tab[c(1,7)]
	for(h in c(0.02,0.1,0.5)){
		tab <- read.table(paste('../validity/clock-PISKa/raw-data/average-n2-N1-h',h,'.dat',sep=''))
		sum_tab <- tab[1]
		sum_tab[2:9] <- sum_columns(tab[2:17],8)
		names(sum_tab) <- cnames
		comparative[length(comparative)+1] <- sum_tab[7]
	}
	names(comparative) <- c('time','KaSim Simulation','PISKa with h=0.02','PISKa with h=0.1','PISKa with h=0.5')
	plot_table(comparative,name='Circadian Clock\nPER2 concentrations with PISKa and KaSim',xlab='Hours',ylab='Concentration',pdf=c('../plots/per2_comparative',7,3.8))
	
"




