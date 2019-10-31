source("import_nvmain_trace.R")
source("aggregate_accesses.R")
source("sample.R")
require(tikzDevice)

do_plot <- function(filename, plotname, plotfile, lower_bnd, upper_bnd, color){
    print(paste("Plotting",plotname))
    data <- import_nvmain_trace(filename,lower_bnd,upper_bnd)
    print(paste("Length:",length(data)))
    data_agg <- aggregate_accesses(data,64,lower_bnd,upper_bnd)
    print(paste("Mean",mean(data_agg)))
    print(paste("Max",max(data_agg)))
    rm(data)
    gc()
    data_agg_sampled <- sample_data(data_agg,150)
    rm(data_agg)
    gc()
    
    pdf(paste(plotfile,".pdf",sep=""))
    plot(data_agg_sampled, type="l", lwd=2, col=color, xlab="main memory address (bytes)", ylab="write count", main=plotname)
    num_pages<-(upper_bnd-lower_bnd)/4096
    abline(v=(seq(0,num_pages)*27.3),col=8)
    dev.off()
    
    tikz(paste(plotfile,".tikz",sep=""), standAlone=FALSE)
    plot(data_agg_sampled, type="l", lwd=2, col=color, xlab="main memory address (bytes)", ylab="write count", main=plotname)
    num_pages<-(upper_bnd-lower_bnd)/4096
    abline(v=(seq(0,num_pages)*27.3),col=8)
    dev.off()
    
    rm(data_agg_sampled)
    gc()
}

do_plot_log <- function(filename, plotname, plotfile, lower_bnd, upper_bnd, color, y_lower_lim, y_upper_lim){
    print(paste("Plotting",plotname))
    data <- import_nvmain_trace(filename,lower_bnd,upper_bnd)
    print(paste("Length:",length(data)))
    data_agg <- aggregate_accesses(data,64,lower_bnd,upper_bnd)
    print(paste("Mean",mean(data_agg)))
    print(paste("Max",max(data_agg)))
    rm(data)
    gc()
    #data_agg_sampled <- sample_data(data_agg,150)
    data_agg_sampled <- sample_data(data_agg,32)
    print(paste("Mean",mean(data_agg_sampled)))
    print(paste("Max",max(data_agg_sampled)))
    rm(data_agg)
    gc()
    
    pdf(paste(plotfile,".pdf",sep=""))
    num_pages<-(upper_bnd-lower_bnd)/4096
    plot(data_agg_sampled, type="l", lwd=2, col=color, xlab="main memory address (bytes)", ylab="write count", main=plotname, log="y", ylim=c(10^y_lower_lim, 10^y_upper_lim), panel.first=c(abline(v=(seq(0,num_pages)*128),col=8), abline(h=10^(seq(y_lower_lim, y_upper_lim)), col=8, lty=2) ))
    axis(side=2, at=rep((10^(seq(y_lower_lim,y_upper_lim))), each=8) * rep(seq(2,9),(y_upper_lim-y_lower_lim+1)), labels=FALSE, tck=-0.01)
    axis(side=2, at=rep((10^(seq(y_lower_lim,y_upper_lim))), each=1) * rep(seq(1,1),(y_upper_lim-y_lower_lim+1)), labels=FALSE, tck=-0.02)
    dev.off()
    
    tikz(paste(plotfile,".tikz",sep=""), standAlone=FALSE)
    num_pages<-(upper_bnd-lower_bnd)/4096
    plot(data_agg_sampled, type="l", lwd=2, col=color, xlab="main memory address (bytes)", ylab="write count", main=plotname, log="y", ylim=c(10^y_lower_lim, 10^y_upper_lim), panel.first=c(abline(v=(seq(0,num_pages)*128),col=8), abline(h=10^(seq(y_lower_lim, y_upper_lim)), col=8, lty=2) ))
    axis(side=2, at=rep((10^(seq(y_lower_lim,y_upper_lim))), each=8) * rep(seq(2,9),(y_upper_lim-y_lower_lim+1)), labels=FALSE, tck=-0.01)
    axis(side=2, at=rep((10^(seq(y_lower_lim,y_upper_lim))), each=1) * rep(seq(1,1),(y_upper_lim-y_lower_lim+1)), labels=FALSE, tck=-0.02)
    dev.off()
    
    rm(data_agg_sampled)
    gc()
}
