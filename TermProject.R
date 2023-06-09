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
x <- communities.and.crime$pctWWage
ll <- function(mean, var) {
    loglik <- -n*(log(2*pi) + log(var))/2 - sum((x - mean)^2)/(2*var)
    return(-loglik)
}
mle_result <- mle(minuslogl=ll,start=c(list(mean=1),list(var=1)))
mle_mean = coef(mle_result)[1]
mle_var = coef(mle_result)[2]

# plot resulting density
plot(density(x))
curve(dnorm(x, mean=mle_mean, sd=sqrt(mle_var)), add=TRUE, col='red')
pr2file('normal//PctWWage_mle_plot.png')

# find MM
mm_estimator <- function(x) {
  mu <- mean(x)
  sigma <- sqrt(mean(x^2) - mu^2)
  return(c(mu, sigma))
}
mm_estimates <- mm_estimator(x)
mm_mean = mm_estimates[1]
mm_sd = mm_estimates[2]

# plot resulting density
plot(density(x))
curve(dnorm(x, mean = mm_mean, sd = mm_sd), add = TRUE, col = 'blue')
pr2file('normal//PctWWage_mm_plot.png')

# EXPONENTIAL FAMILY
# use PctLargHouseFam
# plot and save histogram
hist(communities.and.crime$PctLargHouseFam, probability = TRUE)
#pr2file('exponential//PctLargHouseFam_histogram.png')

# plot and save density plot 
plot(density(communities.and.crime$PctLargHouseFam))
#pr2file('exponential//PctLargHouseFam_density_plot.png')

# find MLE 
n <- length(communities.and.crime$PctLargHouseFam)
x <- communities.and.crime$PctLargHouseFam
l1 <- function(lambda) {
    loglik <- n * log(lambda) - lambda * sum(x)
    return(-loglik)
}
mle_result <- mle(minuslogl=l1,start=c(list(lambda=1)))
lambda = coef(mle_result)[1]

# plot resulting density
plot(density(x))
curve(dexp(x, rate=lambda), add=TRUE, col='red')
pr2file('exponential//PctLargHouseFam_mle_plot.png')

# find MM
mm_estimator <- function(x) {
  lambda <- 1 / mean(x)
  return(lambda)
}
mm_estimate <- mm_estimator(x)

# plot resulting density
plot(density(x))
curve(dexp(x, rate = mm_estimate), add = TRUE, col = 'blue')
pr2file('exponential//PctLargHouseFam_mm_plot.png')

# GAMMA FAMILY
# use PctNotHSGrad
# plot and save histogram
hist(communities.and.crime$PctNotHSGrad, probability = TRUE)
#pr2file('gamma//PctNotHSGrad_histogram.png')

# plot and save density plot 
plot(density(communities.and.crime$PctNotHSGrad))
#pr2file('gamma//PctNotHSGrad_density_plot.png')

# find MLE
x <- communities.and.crime$PctNotHSGrad
x[which(x==0)] <- 0.001
n <- length(x)
ll <- function(k, theta) {
    loglik <- (k-1)*sum(log(x)) - sum(x/theta) - 
        n*k*log(theta) - n*log(gamma(k))
    return(-loglik)
}
z <- mle(minuslogl=ll,start=c(list(k=1),list(theta=1)))
plot(density(communities.and.crime$PctNotHSGrad))
curve(dgamma(x, shape=coef(z)[1], rate=1/coef(z)[2]), add=TRUE, col='red')
pr2file('gamma//PctNotHSGrad_mle_plot.png')

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
# x <- sum(communities.and.crime$PctNotSpeakEnglWell)
x <- communities.and.crime$PctNotSpeakEnglWell
#x[which(x==0)] <- 0.001
#x[which(x==1)] <- 0.999
ll <- function(alpha,beta) {
    loglik <- (alpha-1)*sum(log(x)) + (beta-1)*sum(log(1-x)) - 
        n*log(gamma(alpha)*gamma(beta)/gamma(alpha+beta))
    return(-loglik)
}
# ll <- function(alpha, beta) {
#     loglik <- log((alpha+beta)/(alpha*beta)*x^(alpha-1)*(1-x)^(beta-1))
#     return(loglik)
# }
z <- mle(minuslogl=ll,start=c(list(alpha=1),list(beta=1)))

# plot resulting density
plot(density(communities.and.crime$PctNotSpeakEnglWell))
curve(dbeta(x, shape1=coef(z)[1], shape2=coef(z)[2]), add=TRUE, col='red')
pr2file('beta//PctNotSpeakEnglWell_mle_plot.png')
