source('~/experiments/scripts/plot_folder.R')
source('~/experiments/scripts/util_functions.R')
source('~/experiments/scripts/plot_table.R')
source('~/experiments/scripts/average_data.R')

#average_files('../experiments/SEIRD-gamma(T_E)/raw-data/')
avg_sdev_data <- avg_sdev_files('../experiments/SEIRD-gamma(T_E)/raw-data/',ptrn="city*")
avg_cases_data <- avg_sdev_data[[1]][1:3][-2]
sdev_cases_data <- avg_sdev_data[[2]][1:3][-2]
avg_sdev_cases_data <- avg_cases_data
avg_sdev_cases_data[3] <- avg_sdev_cases_data[2]
avg_sdev_cases_data[2] <- exp(avg_sdev_cases_data[1] / 21)
avg_sdev_cases_data[4] <- avg_cases_data[2]+sdev_cases_data[2]
avg_sdev_cases_data[5] <- avg_cases_data[2]-sdev_cases_data[2]
names(avg_sdev_cases_data)[c(4,5,2,3)] <- c('std dev','std dev','Exponential growth','Cases')

#plot_table(avg_sdev_cases_data,"Cases of Ebola along time","Days","Cases",NULL,list(c(2,2,2),c(1,3,3)))
plot_table(avg_sdev_cases_data,"Cases of Ebola along time","Days","Cases",c("SEIRD-cases",7,7),list(c(3,2,2,2),c(1,1,3,3)),max_value=2500)
#axis(1,c(0,1,2),labels=c(expression(10^0),expression(10^1),expression(10^2)),line=0.805)
#t <- seq(0,150,150/500)
#lines(t,exp(t/21),col=3)
#axis(2,line=0.805)
#dev.off()



final_pop <- elem_of_files(c("../experiments/SEIRD-gamma(T_E)/raw-data","../experiments/SEIRD-gamma(T_E)/raw-data/no-reactions"),ptrn="city*",col_n=3)


hist_data <- hist(log(final_pop)/150,breaks=seq(0,0.07,0.005),plot=FALSE)
hist_data$counts <- hist_data$counts/sum(hist_data$counts)
pdf("SEIRD-growth-hist.pdf",7,7)
op <- par(mai=c(0.65,0.66,0.60,0.1))
plot(hist_data,xlim=c(0,0.06),ann=FALSE,type='n')
abline(v=1/21,col=2,lty=4)
text(1.08/21,650,expression(frac(1,lambda)==21),col=2)
title(main="Epidemic Growth Rate",line=1)
title(xlab=expression("1"/lambda),ylab="Probability",line=2.3)
par(op)
dev.off()


sims_dirs <- list.dirs(path='../experiments/SEIRD_CL-gamma(T_E)/raw-data',recursive=FALSE,full.names=FALSE)
dtable <- read.table(
			list.files(sims_dirs[1],recursive=FALSE,full.names=TRUE,pattern="*.out")[1]
)
total_CL_table <- NULL
count <- 0
cases_CL_list <- c()
for(sim_dir in sims_dirs){
	try({
	conc_table <- concatenate_folder(sim_dir,"*.out",dtable,c('Days',''),1,c(1,2))
	if(sum(conc_table[501,-1]) > 50){
		if(is.null(total_CL_table)){
			total_CL_table <- conc_table[c(1,8,4,11,3,6)]
			coln <- length(total_CL_table)
		}
		else
			total_CL_table[2:coln] <- total_CL_table[2:coln] + conc_table[c(1,8,4,11,3,6)][2:coln]
		count <- count + 1
	}
	cases_CL_list <- c(cases_CL_list,sum(conc_table[501,-1]))
	})
}
total_CL_table[2:coln] <- total_CL_table[2:coln]/count

#plot_table(total_CL_table)
log_table <- total_CL_table
log_table[2:6] <- log10(log_table[2:6])
max_noinfo <- plot_table(log_table[1:350,],"Cases of Ebola per city","Days, t","Cases",c('SEIRD_CL-cases',4,4),draw_axis=c('s','n'),leg_pos=c('topleft',0.02,0.03))
axis(2,c(0,1,2),labels=c(expression(10^0),expression(10^1),expression(10^2)),line=0.805)
dev.off()

pdf("SEIRD_CL-growth-hist.pdf",7,7)
op <- par(mai=c(0.65,0.66,0.60,0.1))
hist_data <- hist(log(cases_CL_list)/150,breaks=seq(0,0.07,0.005),plot=FALSE)
hist_data$counts <- hist_data$counts/sum(hist_data$counts)
plot(hist_data,xlim=c(0,0.06),ylim=c(0.0,0.3),ann=FALSE,type='n')
abline(v=1/21,col=2,lty=4)
text(0.9/21,0.2,expression(frac(1,lambda)==21),col=2)
title(main="Epidemic Growth of total cases \n without information",line=1)
title(xlab=expression("1"/lambda),ylab="Probability",line=2.3)
par(op)
dev.off()



sims_dirs <- list.dirs(path='../experiments/SEIRD_CL-fast_info-gamma(T_E)/raw-data',recursive=FALSE,full.names=FALSE)
dtable <- read.table(
			list.files(sims_dirs[1],recursive=FALSE,full.names=TRUE,pattern="*.out")[1]
)
total_CL_info_table <- NULL
count <- 0
cases_CL_info_list <- c()
for(sim_dir in sims_dirs){
	try({
	conc_table <- concatenate_folder(sim_dir,"*.out",dtable,c('Days',''),1,c(1,2))
	if(sum(conc_table[501,-1]) > 50){
		if(is.null(total_CL_info_table)){
			total_CL_info_table <- conc_table[c(1,8,4,11,3,6)]
			coln <- length(total_CL_info_table)
		}
		else
			total_CL_info_table[2:coln] <- total_CL_info_table[2:coln] + conc_table[c(1,8,4,11,3,6)][2:coln]
		count <- count + 1
	}
	cases_CL_info_list <- c(cases_CL_info_list,2*sum(conc_table[501,-1]))
	})
}
total_CL_info_table[2:coln] <- 2*total_CL_info_table[2:coln]/count

#plot_table(total_CL_info_table)
log_table <- total_CL_info_table
log_table[2:6] <- log10(log_table[2:6])
plot_table(log_table[1:350,],"Cases of Ebola per city","Days, t","Cases",c('SEIRD_CL-info-cases',4,4),NULL,max_noinfo,draw_axis=c('s','n'),leg_pos=c('topleft',0.02,0.03))
axis(2,c(0,1,2),labels=c(expression(10^0),expression(10^1),expression(10^2)),line=0.805)
dev.off()

pdf("SEIRD_CL-info-growth-hist.pdf",7,7)
op <- par(mai=c(0.65,0.66,0.60,0.1))
hist_data <- hist(log(cases_CL_info_list)/150,breaks=seq(0,0.06,0.005),plot=FALSE)
hist_data$counts <- hist_data$counts/sum(hist_data$counts)
plot(hist_data,xlim=c(0,0.06),ann=FALSE,type='n')
abline(v=1/21,col=2,lty=4)
text(0.94/21,0.2,expression(frac(1,lambda)==21),col=2)
title(main="Epidemic growth of total cases \n with information",line=1)
title(xlab=expression("1"/lambda),ylab="Probability",line=2.3)
par(op)
dev.off()








