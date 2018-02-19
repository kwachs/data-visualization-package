VisualizeDistribution <- function(distrib){
  ### plot the cdf and pdf of the distribution "distrib" for different parameter values (pre-set? specified by user?). This function is more of a learning tool to give the user an idea of what the distributions look like when you tweak the different parameters.
  
  # list of available distributions
  kAvailableDistributions <- c("binom", "exp", "gamma", "geom", "hyper", "nbinom", "norm", "pois", "unif")
  
  
  # check if function input is correct
  if(missing(distrib) || mode(distrib) != "character" || length(distrib) != 1){ # distrib should be a character string of length 1
    stop("distrib should be a non-empty character string of length 1")
  }
  if(! distrib %in% kAvailableDistributions){ # distrib should be one of kAvailableDistributions
    stop("distrib should be one of: ", "binom ", "exp ", "gamma ", "geom ", "hyper ", "nbinom ", "norm ", "pois ", "unif ")
  }
  
  # now that we've verified the input is okay, let's do some plotting!
  
  # binomial distribution
  if (distrib == "binom"){
    # pmf.plot
    
    # define ranges of x and y axes for the pdf plots
    x <- seq(from = 0, to = 40, by = 1) # x-axis range
    y <- seq(from = 0, to = 0.25, by= .00625) # y-axis range
    
    # display both pdf and cdf plot in one window
    par(mfrow = c(1,2))
    
    # initialize blank plot area, no annotation (axis titles and overall titles)
    plot(x,y,type='n',ann=FALSE)
    
    title(main = "Binomial Distribution PMF", xlab = "X", ylab = "Density")
    
    # define the 4 pmfs to be plotted
    bd1=dbinom(seq(0,40,1),size=20,prob=.5)
    bd2=dbinom(seq(0,40,1),size=20,prob=.7)
    bd3=dbinom(seq(0,40,1),size=40,prob=.5)
    bd4=dbinom(seq(0,40,1),size=40,prob=.7)
    
    
    
    # plot the 4 pmfs
    lines(seq(0,40,1),bd1,col="Red",type="b",pch=1)
    lines(seq(0,40,1),bd2,col="Blue",type="b",pch=1)
    lines(seq(0,40,1),bd3,col="orange",type="b",pch=1)
    lines(seq(0,40,1),bd4,col="green",type="b",pch=1)
    
    
    legend("topright", 
           legend = c("size=20,p=.5", "size=20,p=.7","size=40,p=.5","size=40,p=.7"),
           col = c("Red", "Blue","Orange","Green"),
           lwd = rep(1, 4),
           bty = "n", 
           cex = 0.8) # bty = "n" means no box around legend; cex is font-size relative to current par("cex")
    
    # cdf.plot
    
    # define ranges of x and y axes for the pdf plots
    x <- seq(from = 0, to = 40, by = 1) # x-axis range
    y <- seq(from = 0, to = 1, by=.025) # y-axis range
    
    # in same plotting window as pdf.plot, initialize blank plot area with no annotation (axis titles and overall titles)
    plot(x,y,type="n",ann=FALSE)
    
    title(main = "Binomial Distribution CDF", xlab = "X", ylab = "Cumulative Density")
    
    # define the 4 cdfs to be plotted
    bp1=pbinom(seq(0,40,1),size=20,prob=.5)
    bp2=pbinom(seq(0,40,1),size=20,prob=.7)
    bp3=pbinom(seq(0,40,1),size=40,prob=.5)
    bp4=pbinom(seq(0,40,1),size=40,prob=.7)
    
    # plot the 4 cdfs 
    lines(seq(0,40,1),bp1,col="Red",type="s",pch=1)
    lines(seq(0,40,1),bp2,col="Blue",type="s",pch=1)
    lines(seq(0,40,1),bp3,col="orange",type="s",pch=1)
    lines(seq(0,40,1),bp4,col="green",type="s",pch=1)
    
    
    legend("bottomright", 
           legend = c("size=20,p=.5", "size=20,p=.7","size=40,p=.5","size=40,p=.7"),
           col = c("Red", "Blue","Orange","Green"),
           lwd = rep(2, 4), 
           #bty = "n", 
           cex = 0.8)
    
    par(mfrow = c(1,1)) # return par mfrow to default 1 plot per window
    return("Visualize Binomial Distribution Complete")
  }
  # exponential distribution
  if (distrib == "exp"){
    # pdf.plot
    
    # define ranges of x and y axes for the pdf plots
    x <- seq(from = 0, to = 5, by = 1) # x-axis range
    y <- seq(from = 0, to = 2, by=0.4) # y-axis range
    
    # display both pdf and cdf plot in one window
    par(mfrow = c(1,2))
    
    # initialize blank plot area, no annotation (axis titles and overall titles)
    plot(x,y,type='n',ann=FALSE)
    
    title(main = "Exponential Distribution PDF", xlab = "X", ylab = "Density")
    
    # define the 4 pdfs to be plotted
    ed1=dexp(seq(0,5,.001),0.5)
    ed2=dexp(seq(0,5,.001),1)
    ed5=dexp(seq(0,5,.001),1.5)
    edn2=dexp(seq(0,5,.001),2)
    
    # plot the 4 pdfs
    lines(seq(0,5,.001),ed1,col="Red",lwd=2)
    lines(seq(0,5,.001),ed2,col="Blue",lwd=2)
    lines(seq(0,5,.001),ed5,col="orange",lwd=2)
    lines(seq(0,5,.001),edn2,col="green",lwd=2)
    
    
    legend("topright", 
           legend = c("rate=.5", "rate=1","rate=1.5","rate=2"),
           col = c("Red", "Blue","Orange","Green"),
           lwd = rep(2, 4),
           bty = "n", 
           cex = 0.8) # bty = "n" means no box around legend; cex is font-size relative to current par("cex")
    
    # cdf.plot
    
    # define ranges of x and y axes for the pdf plots
    x <- seq(from = 0, to = 5, by = 1) # x-axis range
    y <- seq(from = 0, to = 1, by=0.2) # y-axis range
    
    # in same plotting window as pdf.plot, initialize blank plot area with no annotation (axis titles and overall titles)
    plot(x,y,type='n',ann=FALSE)
    
    title(main = "Exponential Distribution CDF", xlab = "X", ylab = "Cumulative Density")
    
    # define the 4 cdfs to be plotted
    ep1=pexp(seq(0,5,.001),0.5)
    ep2=pexp(seq(0,5,.001),1)
    ep5=pexp(seq(0,5,.001),1.5)
    epn2=pexp(seq(0,5,.001),2)
    
    
    # plot the 4 cdfs 
    lines(seq(0,5,.001),ep1,lwd=2,col="Red")
    lines(seq(0,5,.001),ep2,col="Blue",lwd=2)
    lines(seq(0,5,.001),ep5,col="orange",lwd=2)
    lines(seq(0,5,.001),epn2,col="green",lwd=2)
    
    
    legend("bottomright", 
           legend = c("rate=.5", "rate=1","rate=1.5","rate=2"),
           col = c("Red", "Blue","Orange","Green"),
           lwd = rep(1, 4), 
           bty = "n", 
           cex = 0.8)
    
    par(mfrow = c(1,1)) # return par mfrow to default 1 plot per window
    return("Visualize Exponential Distribution Complete")
    
  }
  # gamma distribution
  if (distrib == "gamma"){
    # pdf.plot
    
    # define ranges of x and y axes for the pdf plots
    x <- seq(from = 1, to = 20, by = 0.01) # x-axis range
    y <- seq(from = 0, to = 1, by=.0005) # y-axis range
    
    # display both pdf and cdf plot in one window
    par(mfrow = c(1,2))
    
    # initialize blank plot area, no annotation (axis titles and overall titles)
    plot(x=seq(0,20,.01),y=seq(0,.5,.00025),type="n",ann=FALSE)
    
    title(main = "Gamma Distribution PDF", xlab = "X", ylab = "Density")
    
    # define the 7 pdfs to be plotted
    gd1=dgamma(seq(0,20,.001),shape=1,scale=2)
    gd2=dgamma(seq(0,20,.001),shape=2,scale=2)
    gd3=dgamma(seq(0,20,.001),shape=3,scale=2)
    gd4=dgamma(seq(0,20,.001),shape=5,scale=1)
    gd5=dgamma(seq(0,20,.001),shape=9,scale=0.5)
    gd6=dgamma(seq(0,20,.001),shape=7.5,scale=1)
    gd7=dgamma(seq(0,20,.001),shape=0.5,scale=1)
    
    
    # plot the 7 pdfs
    lines(seq(0,20,.001),gd1,col="Red",lwd=2)
    lines(seq(0,20,.001),gd2,col="Orange",lwd=2)
    lines(seq(0,20,.001),gd3,col="Yellow",lwd=2)
    lines(seq(0,20,.001),gd4,col="Green",lwd=2)
    lines(seq(0,20,.001),gd5,col="Black",lwd=2)
    lines(seq(0,20,.001),gd6,col="Blue",lwd=2)
    lines(seq(0,20,.001),gd7,col="Purple",lwd=2)
    
    
    legend("topright", 
           legend = c("shape=1, scale=2", "shape=2, scale=2","shape=3, scale=2","shape=5, scale=1","shape=9, scale=0.5","shape=7.5, scale=1","shape=0.5, scale=1"),
           col = c("Red", "Orange","Yellow","Green","Black","Blue","Purple"),
           lwd = rep(1, 7),
           bty = "n", 
           cex = 0.8) # bty = "n" means no box around legend; cex is font-size relative to current par("cex")
    
    # cdf.plot
    
    # define ranges of x and y axes for the pdf plots
    x <- seq(from = 0, to = 20, by = 0.01) # x-axis range
    y <- seq(from = 0, to = 1, by=0.0005) # y-axis range
    
    # in same plotting window as pdf.plot, initialize blank plot area with no annotation (axis titles and overall titles)
    plot(x,y,type="n",ann=FALSE)
    
    title(main = "Gamma Distribution CDF", xlab = "X", ylab = "Cumulative Density")
    
    # define the 7 cdfs to be plotted
    gp1=pgamma(seq(0,20,.001),shape=1,scale=2)
    gp2=pgamma(seq(0,20,.001),shape=2,scale=2)
    gp3=pgamma(seq(0,20,.001),shape=3,scale=2)
    gp4=pgamma(seq(0,20,.001),shape=5,scale=1)
    gp5=pgamma(seq(0,20,.001),shape=9,scale=0.5)
    gp6=pgamma(seq(0,20,.001),shape=7.5,scale=1)
    gp7=pgamma(seq(0,20,.001),shape=0.5,scale=1)
    
    
    # plot the 7 cdfs 
    lines(seq(0,20,.001),gp1,col="Red",lwd=2)
    lines(seq(0,20,.001),gp2,col="Orange",lwd=2)
    lines(seq(0,20,.001),gp3,col="Yellow",lwd=2)
    lines(seq(0,20,.001),gp4,col="Green",lwd=2)
    lines(seq(0,20,.001),gp5,col="Black",lwd=2)
    lines(seq(0,20,.001),gp6,col="Blue",lwd=2)
    lines(seq(0,20,.001),gp7,col="Purple",lwd=2)
    
    
    legend("bottomright", 
           legend = c("shape=1, scale=2", "shape=2, scale=2","shape=3, scale=2","shape=5, scale=1","shape=9, scale=0.5","shape=7.5, scale=1","shape=0.5, scale=1"),
           col = c("Red", "Orange","Yellow","Green","Black","Blue","Purple"),
           lwd = rep(2, 4), 
           bty = "n", 
           cex = 0.6)
    
    par(mfrow = c(1,1)) # return par mfrow to default 1 plot per window
    return("Visualize Exponential Distribution Complete")
    
  }
  # geometric distribution
  if (distrib == "geom"){
    # pdf.plot
    
    # define ranges of x and y axes for the pdf plots
    x <- seq(from = 0, to = 10, by = 1) # x-axis range
    y <- seq(from = 0, to = 1, by=0.1) # y-axis range
    
    # display both pdf and cdf plot in one window
    par(mfrow = c(1,2))
    
    # initialize blank plot area, no annotation (axis titles and overall titles)
    plot(x,y,type="n",ann=FALSE)
    
    title(main = "Geometric Distribution PMF", xlab = "X", ylab = "Density")
    
    # define the 3 pmfs to be plotted
    ged1=dgeom(seq(0,10,1),prob=.2)
    ged2=dgeom(seq(0,10,1),prob=.5)
    ged3=dgeom(seq(0,10,1),prob=.8)
    
    
    # plot the 3 pmfs
    lines(seq(0,10,1),ged1,col="Red",type="b",pch=1)
    lines(seq(0,10,1),ged2,col="Blue",type="b",pch=1)
    lines(seq(0,10,1),ged3,col="Green",type="b",pch=1)
    
    legend("topright", 
           legend = c("p=.2", "p=.5","p=.8"),
           col = c("Red", "Blue","Green"),
           lwd = rep(1, 3),
           bty = "n", 
           cex = 0.8) # bty = "n" means no box around legend; cex is font-size relative to current par("cex")
    
    # cdf.plot
    
    # define ranges of x and y axes for the pdf plots
    x <- seq(from = 0, to = 10, by = 1) # x-axis range
    y <- seq(from = 0, to = 1, by=0.1) # y-axis range
    
    # in same plotting window as pdf.plot, initialize blank plot area with no annotation (axis titles and overall titles)
    plot(x,y,type="n",ann=FALSE)
    
    title(main = "Geometric Distribution CDF", xlab = "X", ylab = "Cumulative Density")
    
    # define the 3 cdfs to be plotted
    gep1=pgeom(seq(0,10,1),prob=.2)
    gep2=pgeom(seq(0,10,1),prob=.5)
    gep3=pgeom(seq(0,10,1),prob=.8)
    
    
    # plot the 3 cdfs 
    lines(seq(0,10,1),gep1,col="Red",type="s",pch=1)
    lines(seq(0,10,1),gep2,col="Blue",type="s",pch=1)
    lines(seq(0,10,1),gep3,col="Green",type="s",pch=1)
    
    
    legend("bottomright", 
           legend = c("p=.2", "p=.5","p=.8"),
           col = c("Red", "Blue","Green"),
           lwd = rep(1,3), 
           bty = "n", 
           cex = 0.8)
    
    par(mfrow = c(1,1)) # return par mfrow to default 1 plot per window
    return("Visualize Geometric Distribution Complete")
  }
  # hypergeometric distribution
  if (distrib == "hyper"){
    # pdf.plot
    
    # define ranges of x and y axes for the pdf plots
    x <- seq(from = 0, to = 60, by = 1) # x-axis range
    y <- seq(from = 0, to = 0.15, by=0.0025) # y-axis range
    
    # display both pdf and cdf plot in one window
    par(mfrow = c(1,2))
    
    # initialize blank plot area, no annotation (axis titles and overall titles)
    plot(x,y,type='n',ann=FALSE)
    
    title(main = "HyperGeometric Distribution PMF", xlab = "X", ylab = "Density")
    
    # define the 3 pmfs to be plotted
    hgd1=dhyper(seq(0,60,1),m=50,n=450,k=100)
    hgd2=dhyper(seq(0,60,1),m=60,n=440,k=200)
    hgd3=dhyper(seq(0,60,1),m=70,n=430,k=300)
    
    # plot the 3 pmfs
    lines(seq(0,60,1),hgd1,col="Red",type="b",pch=1)
    lines(seq(0,60,1),hgd2,col="Blue",type="b",pch=1)
    lines(seq(0,60,1),hgd3,col="Green",type="b",pch=1)
    
    
    legend("topright", 
           legend = c("N=500, r=50, n=100", "N=500, r=60, n=200","N=500, r=70, n=300"),
           col = c("Red", "Blue","Green"),
           lwd = rep(1, 3),
           bty = "n", 
           cex = 0.8) # bty = "n" means no box around legend; cex is font-size relative to current par("cex")
    
    # cdf.plot
    
    # define ranges of x and y axes for the pdf plots
    x <- seq(from = 0, to = 60, by = 1) # x-axis range
    y <- seq(from = 0, to = 1, by=(1/60)) # y-axis range
    
    # in same plotting window as pdf.plot, initialize blank plot area with no annotation (axis titles and overall titles)
    plot(x,y,type="n",ann=FALSE)
    
    title(main = "HyperGeometric Distribution CDF", xlab = "X", ylab = "Cumulative Density")
    
    # define the 3 cdfs to be plotted
    hgp1=phyper(seq(0,60,1),m=50,n=450,k=100)
    hgp2=phyper(seq(0,60,1),m=60,n=440,k=200)
    hgp3=phyper(seq(0,60,1),m=70,n=430,k=300)
    
    
    # plot the 3 cdfs 
    lines(seq(0,60,1),hgp1,col="Red",type="s",pch=1)
    lines(seq(0,60,1),hgp2,col="Blue",type="s",pch=1)
    lines(seq(0,60,1),hgp3,col="Green",type="s",pch=1)
    
    
    legend("bottomright", 
           legend = c("N=500, r=50, n=100", "N=500, r=60, n=200","N=500, r=70, n=300"),
           col = c("Red", "Blue","Green"),
           lwd = rep(1, 3),
           bty = "n", 
           cex = 0.8) # bty = "n" means no box around legend; cex is font-size relative to current par("cex")
    
    par(mfrow = c(1,1)) # return par mfrow to default 1 plot per window
    return("Visualize HyperGeometric Distribution Complete")
    
    
  }
  # negative binomial distribution
  if (distrib == "nbinom"){
    # pdf.plot
    
    # define ranges of x and y axes for the pdf plots
    x <- seq(from = 0, to = 60, by = 1) # x-axis range
    y <- seq(from = 0, to = 0.15, by=.0025) # y-axis range
    
    # display both pdf and cdf plot in one window
    par(mfrow = c(1,2))
    
    # initialize blank plot area, no annotation (axis titles and overall titles)
    plot(x,y,type='n',ann=FALSE)
    
    title(main = "Negative Binomial Distribution PMF", xlab = "X", ylab = "Density")
    
    # define the 8 pmfs to be plotted
    nbd1=dnbinom(seq(0,60,1),size=1,prob=.1)
    nbd2=dnbinom(seq(0,60,1),size=2,prob=.2)
    nbd3=dnbinom(seq(0,60,1),size=3,prob=.3)
    nbd4=dnbinom(seq(0,60,1),size=4,prob=.4)
    nbd5=dnbinom(seq(0,60,1),size=5,prob=.5)
    nbd6=dnbinom(seq(0,60,1),size=10,prob=.5)
    nbd7=dnbinom(seq(0,60,1),size=20,prob=.5)
    nbd8=dnbinom(seq(0,60,1),size=40,prob=.5)
    
    # plot the 8 pmfs
    lines(seq(0,60,1),nbd1,col="Red",type="b",pch=1)
    lines(seq(0,60,1),nbd2,col="Orange",type="b",pch=1)
    lines(seq(0,60,1),nbd3,col="Yellow",type="b",pch=1)
    lines(seq(0,60,1),nbd4,col="Green",type="b",pch=1)
    lines(seq(0,60,1),nbd5,col="Blue",type="b",pch=1)
    lines(seq(0,60,1),nbd6,col="Purple",type="b",pch=1)
    lines(seq(0,60,1),nbd7,col="Black",type="b",pch=1)
    lines(seq(0,60,1),nbd8,col="Brown",type="b",pch=1)
    
    
    legend("topright", 
           legend = c("size=1,prob=.1", "size=2,prob=.2","size=3,prob=.3", "size=4,prob=.4","size=5,prob=.5", "size=10,prob=.5","size=20,prob=.5", "size=40,prob=.5"),
           col = c("Red","Orange","Yellow","Green","Blue","Purple","Black","Brown"),
           lwd = rep(1,8),
           bty = "n", 
           cex = 0.8) # bty = "n" means no box around legend; cex is font-size relative to current par("cex")
    
    # cdf.plot
    
    # define ranges of x and y axes for the pdf plots
    x <- seq(from = 0, to = 60, by = 1) # x-axis range
    y <- seq(from = 0, to = 1,by=(1/60)) # y-axis range
    
    # in same plotting window as pdf.plot, initialize blank plot area with no annotation (axis titles and overall titles)
    plot(x,y,type="n",ann=FALSE)
    
    title(main = "Negative Binomial Distribution CDF", xlab = "X", ylab = "Cumulative Density")
    
    # define the 8 cdfs to be plotted
    nbp1=pnbinom(seq(0,60,1),size=1,prob=.1)
    nbp2=pnbinom(seq(0,60,1),size=2,prob=.2)
    nbp3=pnbinom(seq(0,60,1),size=3,prob=.3)
    nbp4=pnbinom(seq(0,60,1),size=4,prob=.4)
    nbp5=pnbinom(seq(0,60,1),size=5,prob=.5)
    nbp6=pnbinom(seq(0,60,1),size=10,prob=.5)
    nbp7=pnbinom(seq(0,60,1),size=20,prob=.5)
    nbp8=pnbinom(seq(0,60,1),size=40,prob=.5)
    
    
    # plot the 8 cdfs 
    lines(seq(0,60,1),nbp1,col="Red",type="s",pch=1)
    lines(seq(0,60,1),nbp2,col="Orange",type="s",pch=1)
    lines(seq(0,60,1),nbp3,col="Yellow",type="s",pch=1)
    lines(seq(0,60,1),nbp4,col="Green",type="s",pch=1)
    lines(seq(0,60,1),nbp5,col="Blue",type="s",pch=1)
    lines(seq(0,60,1),nbp6,col="Purple",type="s",pch=1)
    lines(seq(0,60,1),nbp7,col="Black",type="s",pch=1)
    lines(seq(0,60,1),nbp8,col="Brown",type="s",pch=1)
    
    
    legend("bottomright", 
           legend = c("size=1,prob=.1", "size=2,prob=.2","size=3,prob=.3", "size=4,prob=.4","size=5,prob=.5", "size=10,prob=.5","size=20,prob=.5", "size=40,prob=.5"),
           col = c("Red","Orange","Yellow","Green","Blue","Purple","Black","Brown"),
           lwd = rep(1,8),
           bty = "n", 
           cex = 0.8) # bty = "n" means no box around legend; cex is font-size relative to current par("cex")
    
    par(mfrow = c(1,1)) # return par mfrow to default 1 plot per window
    return("Visualize Negative Binomial Distribution Complete")
    
  } 
  # normal distribution
  if (distrib == "norm"){
    # pdf.plot
    
    # define ranges of x and y axes for the pdf plots
    x <- seq(from = -7, to = 7, by = 0.05) # x-axis range
    y <- seq(from = 0, to = 0.9, length.out = length(x)) # y-axis range
    
    # display both pdf and cdf plot in one window
    par(mfrow = c(1,2))
    
    # initialize blank plot area, no annotation (axis titles and overall titles)
    plot(x, y, col = "white", ann = FALSE)
    
    title(main = "Normal Distribution PDF", xlab = "X", ylab = "Density")
    
    # define the 4 pdfs to be plotted
    d1 <- dnorm(x, mean =  0, sd = sqrt(1))
    d2 <- dnorm(x, mean =  0, sd = sqrt(0.2))
    d5 <- dnorm(x, mean =  0, sd = sqrt(5))
    dn2 <- dnorm(x, mean = -2, sd = sqrt(0.5))
    
    # plot the 4 pdfs
    lines(x, d1, col = "red", lwd = 2) # lwd is line-width
    lines(x, d2, col = "blue", lwd = 2)
    lines(x, d5, col = "orange", lwd = 2)
    lines(x, dn2, col = "green", lwd = 2)
    
    legend("topleft", 
           legend = c("mean =  0, var = 0.2", "mean =  0, var = 1", "mean =  0, var = 5", "mean = -2, var = 0.5"),
           col = c("blue", "red", "orange", "green"),
           lwd = rep(2, 4),
           bty = "n", 
           cex = 0.6) # bty = "n" means no box around legend; cex is font-size relative to current par("cex")
    
    # cdf.plot
    
    # define ranges of x and y axes for the pdf plots
    x <- seq(from = -5, to = 5, by = 0.05) # x-axis range
    y <- seq(from = 0, to = 1, length.out = length(x)) # y-axis range
    
    # in same plotting window as pdf.plot, initialize blank plot area with no annotation (axis titles and overall titles)
    plot(x, y, col = "white", ann = FALSE)
    
    title(main = "Normal Distribution CDF", xlab = "X", ylab = "Cumulative Density")
    
    # define the 4 cdfs to be plotted
    p1 <- pnorm(x, mean = 0, sd = sqrt(1))
    p2 <- pnorm(x, mean = 0, sd = sqrt(0.2))
    p5 <- pnorm(x, mean = 0, sd = sqrt(5))
    pn2 <- pnorm(x, mean = -2, sd = sqrt(0.5))
    
    # plot the 4 cdfs 
    lines(x, p1, col = "red", lwd = 2)
    lines(x, p2, col = "blue", lwd = 2)
    lines(x, p5, col = "orange", lwd = 2)
    lines(x, pn2, col = "green", lwd = 2)
    
    legend("topleft", 
           legend = c("mean =  0, var = 0.2", "mean =  0, var = 1", "mean =  0, var = 5", "mean = -2, var = 0.5"),
           col = c("blue", "red", "orange", "green"),
           lwd = rep(2, 4), 
           bty = "n", 
           cex = 0.6)
    
    par(mfrow = c(1,1)) # return par mfrow to default 1 plot per window
    return("Visualize Normal Distribution Complete")
    
  }
  # Poisson distribution
  if (distrib == "pois"){
    # pdf.plot
    
    # define ranges of x and y axes for the pdf plots
    x <- seq(from = 0, to = 20, by = 1) # x-axis range
    y <- seq(from = 0, to = .4, by=.02) # y-axis range
    
    # display both pdf and cdf plot in one window
    par(mfrow = c(1,2))
    
    # initialize blank plot area, no annotation (axis titles and overall titles)
    plot(x,y,type='n',ann=FALSE)
    
    title(main = "Poisson Distribution PMF", xlab = "X", ylab = "Density")
    
    # define the 3 pmfs to be plotted
    pd1=dpois(seq(0,20,1),lambda=1)
    pd2=dpois(seq(0,20,1),lambda=4)
    pd3=dpois(seq(0,20,1),lambda=10)
    
    
    # plot the 3 pmfs
    lines(seq(0,20,1),pd1,col="Red",type="o",pch=16)
    lines(seq(0,20,1),pd2,col="Blue",type="o",pch=16)
    lines(seq(0,20,1),pd3,col="Green",type="o",pch=16)
    
    
    legend("topright", 
           legend = c("Lambda=1", "Lambda=4","Lambda=10"),
           col = c("Red", "Blue","Green"),
           lwd = rep(1,3),
           bty = "n", 
           cex = 0.8) # bty = "n" means no box around legend; cex is font-size relative to current par("cex")
    
    # cdf.plot
    
    # define ranges of x and y axes for the pdf plots
    x <- seq(from = 0, to = 20, by = 1) # x-axis range
    y <- seq(from = 0, to = 1, by=(1/20)) # y-axis range
    
    # in same plotting window as pdf.plot, initialize blank plot area with no annotation (axis titles and overall titles)
    plot(x,y,type="n",ann=FALSE)
    
    title(main = "Poisson Distribution CDF", xlab = "X", ylab = "Cumulative Density")
    
    # define the 3 cdfs to be plotted
    pp1=ppois(seq(0,20,1),lambda=1)
    pp2=ppois(seq(0,20,1),lambda=4)
    pp3=ppois(seq(0,20,1),lambda=10)
    
    
    # plot the 3 cdfs 
    lines(seq(0,20,1),pp1,col="Red",type="s",pch=16)
    lines(seq(0,20,1),pp2,col="Blue",type="s",pch=16)
    lines(seq(0,20,1),pp3,col="Green",type="s",pch=16)
    
    
    legend("bottomright", 
           legend = c("Lambda=1", "Lambda=4","Lambda=10"),
           col = c("Red", "Blue","Green"),
           lwd = rep(1,3),
           bty = "n", 
           cex = 0.8) # bty = "n" means no box around legend; cex is font-size relative to current par("cex")
    
    par(mfrow = c(1,1)) # return par mfrow to default 1 plot per window
    return("Visualize Poisson Distribution Complete")
  }
  # uniform distribution
  if (distrib == "unif"){
    # pdf.plot
    
    # define ranges of x and y axes for the pdf plots
    x <- seq(from = -3, to = 3, by = 0.3) # x-axis range
    y <- seq(from = 0, to = 2, by=0.1) # y-axis range
    
    # display both pdf and cdf plot in one window
    par(mfrow = c(1,2))
    
    # initialize blank plot area, no annotation (axis titles and overall titles)
    plot(x,y,col=0,ann=FALSE)
    
    title(main = "Uniform Distribution PDF", xlab = "X", ylab = "Density")
    
    # define the 4 pdfs to be plotted
    ud1=dunif(seq(-5,5,.001),0,1)
    ud2=dunif(seq(-5,5,.001),0,.5)
    ud5=dunif(seq(-5,5,.001),0,2)
    udn2=dunif(seq(-5,5,.001),-2,2)
    
    
    # plot the 4 pdfs
    lines(seq(-5,5,.001),ud1,lwd=2,col="Red")
    lines(seq(-5,5,.001),ud2,col="Blue",lwd=2)
    lines(seq(-5,5,.001),ud5,col="orange",lwd=2)
    lines(seq(-5,5,.001),udn2,col="green",lwd=2)
    
    
    legend("topright", 
           legend = c("min=0, max=.5", "min=0, max=1","min=0, max=2","min=-2, max=2"),
           col = c("Blue", "Red","Orange","Green"),
           lwd = rep(1, 4),
           bty = "n", 
           cex = 0.8) # bty = "n" means no box around legend; cex is font-size relative to current par("cex")
    
    # cdf.plot
    
    # define ranges of x and y axes for the pdf plots
    x <- seq(from = -3, to = 3, by = 0.3) # x-axis range
    y <- seq(from = 0, to = 1, .05) # y-axis range
    
    # in same plotting window as pdf.plot, initialize blank plot area with no annotation (axis titles and overall titles)
    plot(x,y,col=0,ann=FALSE)
    
    
    
    title(main = "Uniform Distribution CDF", xlab = "X", ylab = "Cumulative Density")
    
    # define the 4 cdfs to be plotted
    up1=punif(seq(-5,5,.001),0,1)
    up2=punif(seq(-5,5,.001),0,.5)
    up5=punif(seq(-5,5,.001),0,2)
    upn2=punif(seq(-5,5,.001),-2,2)
    
    
    # plot the 4 cdfs 
    lines(seq(-5,5,.001),up1,col="Red",lwd=2)
    lines(seq(-5,5,.001),up2,col="Blue",lwd=2)
    lines(seq(-5,5,.001),up5,col="orange",lwd=2)
    lines(seq(-5,5,.001),upn2,col="green",lwd=2)
    
    
    legend("topleft", 
           legend = c("min=0, max=.5", "min=0, max=1","min=0, max=2","min=-2, max=2"),
           col = c("Blue", "Red","Orange","Green"),
           lwd = rep(1, 4), 
           bty = "n", 
           cex = 0.8)
    
    par(mfrow = c(1,1)) # return par mfrow to default 1 plot per window
    return("Visualize Uniform Distribution Complete")
  }
  
  
  return("if you can see this, one of your if statements is missing a return statement...")
}