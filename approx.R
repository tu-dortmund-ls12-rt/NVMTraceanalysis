require(tikzDevice)

plot_approx <- function(data, lower_address, upper_address, title, color, output_path){
    xlabstep=floor(length(data)/6)

    
    pdf(paste(output_path,".pdf",sep=""))
    plot(data, main=title, col=color, type="o", lwd=2, xaxt="n", ylab="approximated write-count", xlab="main memory address (bytes)")
    axis(1, at=seq(1,length(data),xlabstep), labels=seq(0, length(data)-1,xlabstep)*4096)
    abline(v=(seq(1,length(data))),col=8)
    dev.off()
    
    tikz(paste(output_path,".tikz",sep=""), standAlone=FALSE)
    plot(data, main=title, col=color, type="o", lwd=2, xaxt="n", ylab="approximated write-count", xlab="main memory address (bytes)")
    axis(1, at=seq(1,length(data),xlabstep), labels=seq(0, length(data)-1,xlabstep)*4096)
    abline(v=(seq(1,length(data))),col=8)
    dev.off()
}
