# load in dataset
load('communities.and.crime.rda',verbose=T) 

library(stats4)
# data analysis 
# print(head(communities.and.crime))


# image type is implied by the file name suffix, e.g. '.jpg'; other choices are '.pdf' and '.png'
pr2file <- function (filename)
{
    origdev <- dev.cur()
    parts <- strsplit(filename,".",fixed=TRUE)
    nparts <- length(parts[[1]])
    suff <- parts[[1]][nparts]
    if (suff == "pdf") {
        pdf(filename)
    }
    else if (suff == "png") {
        png(filename,bg='white')
    }
    else jpeg(filename)
    devnum <- dev.cur()
    dev.set(origdev)
    dev.copy(which = devnum)
    dev.set(devnum)
    dev.off()
    dev.set(origdev)
}

# NORMAL FAMILY
# use pctWWage
# plot and save histogram
hist(communities.and.crime$pctWWage, probability = TRUE)
#pr2file('normal//pctWWage_histogram.png')

# plot and save density plot 
plot(density(communities.and.crime$pctWWage))
#pr2file('normal//PctWWage_density_plot.png')

# find MLE 
n <- length(communities.and.crime$pctWWage)
llnorm <- function(mean, var) {
    loglik <- -n*(log(2*pi) + log(var))/2 - sum((communities.and.crime$pctWWage - mean)^2)/(2*var)
    return(-loglik)
}
mlenorm <- mle(minuslogl=llnorm,start=c(list(mean=1),list(var=1)))

# plot resulting density
plot(density(communities.and.crime$pctWWage))
curve(dnorm(x, mean=coef(mlenorm)[1], sd=sqrt(coef(mlenorm)[2])), add=TRUE, col='red')
#pr2file('normal//PctWWage_mle_plot.png')

# EXPONENTIAL FAMILY
# use PctLargHouseFam
# plot and save histogram
hist(communities.and.crime$PctLargHouseFam, probability = TRUE)
#pr2file('exponential//PctLargHouseFam_histogram.png')

# plot and save density plot 
plot(density(communities.and.crime$PctLargHouseFam))
#pr2file('exponential//PctLargHouseFam_density_plot.png')

# GAMMA FAMILY
# use PctNotHSGrad
# plot and save histogram
hist(communities.and.crime$PctNotHSGrad, probability = TRUE)
#pr2file('gamma//PctNotHSGrad_histogram.png')

# plot and save density plot 
plot(density(communities.and.crime$PctNotHSGrad))
#pr2file('gamma//PctNotHSGrad_density_plot.png')

# BETA FAMILY
# use PctNotSpeakEnglWell
# plot and save histogram
hist(communities.and.crime$PctNotSpeakEnglWell, probability = TRUE)
#pr2file('beta//PctNotSpeakEnglWell_histogram.png')

# plot and save density plot 
plot(density(communities.and.crime$PctNotSpeakEnglWell))
#pr2file('beta//PctNotSpeakEnglWell_density_plot.png')

# find MLE 
n <- length(communities.and.crime$PctNotSpeakEnglWell)
x <- sum(communities.and.crime$PctNotSpeakEnglWell)
llnorm <- function(alpha, beta) {
    loglik <- log((alpha+beta)/(alpha*beta)*x^(alpha-1)*(1-x)^(beta-1))
    return(loglik)
}
mlenorm <- mle(minuslogl=llnorm,start=c(list(alpha=1),list(beta=1)))
print(mlenorm)

# plot resulting density
plot(density(communities.and.crime$PctNotHSGrad))
curve(dgamma(x, shape=coef(mlenorm)[1], rate=sqrt(coef(mlenorm)[2])), add=TRUE, col='red')




