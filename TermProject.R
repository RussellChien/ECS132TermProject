library(stats4)
# Load dataset: Communities and Crime
load("communities.and.crime.rda", verbose = TRUE) 

# Image type is implied by the file name suffix, e.g. '.jpg'; other choices are '.pdf' and '.png'
pr2file <- function(filename) {
    origdev <- dev.cur()
    parts <- strsplit(filename, ".", fixed = TRUE)
    nparts <- length(parts[[1]])
    suff <- parts[[1]][nparts]
    if (suff == "pdf") {
        pdf(filename)
    } else if (suff == "png") {
        png(filename, bg = "white")
    } else jpeg(filename)
    devnum <- dev.cur()
    dev.set(origdev)
    dev.copy(which = devnum)
    dev.set(devnum)
    dev.off()
    dev.set(origdev)
}

######### NORMAL FAMILY ######### Variable: pctWWage
hist(communities.and.crime$pctWWage, probability = TRUE)
pr2file("normal//pctWWage_histogram.png")
plot(density(communities.and.crime$pctWWage))
pr2file("normal//pctWWage_density_plot.png")
x <- communities.and.crime$pctWWage
n <- length(x)

### NORMAL: MLE ###
ll <- function(mean, var) {
    loglik <- -n * (log(2 * pi) + log(var)) / 2 - sum((x - mean)^2) / (2 * var)
    -loglik
}

z <- mle(minuslogl = ll, start = c(list(mean = 1), list(var = 1)))
plot(density(communities.and.crime$pctWWage))
curve(dnorm(x, mean = coef(z)[1], sd = sqrt(coef(z)[2])),
            add = TRUE, col = "red")
pr2file("normal//pctWWage_mle_plot.png")

### NORMAL: MM ###
mm <- function(x) {
  mu <- mean(x)
  sigma <- sqrt(mean(x^2) - mu^2)
  return(c(mu, sigma))
}

e <- mm(x)
plot(density(communities.and.crime$pctWWage))
curve(dnorm(x, mean = e[1], sd = e[2]),
            add = TRUE, col = "blue")
pr2file("normal//pctWWage_mm_plot.png")



######### EXPONENTIAL FAMILY ######### Variable: PctLargHouseFam
hist(communities.and.crime$PctLargHouseFam, probability = TRUE)
# pr2file("exponential//PctLargHouseFam_histogram.png")
plot(density(communities.and.crime$PctLargHouseFam))
# pr2file("exponential//PctLargHouseFam_density_plot.png")
x <- communities.and.crime$PctLargHouseFam
n <- length(x)

### EXPONENTIAL: MLE ###
ll <- function(lambda) {
    loglik <- n * log(lambda) - lambda * sum(x)
    -loglik
}

z <- mle(minuslogl = ll, start = c(list(lambda = 1)))
plot(density(communities.and.crime$PctLargHouseFam))
curve(dexp(x, rate = coef(z)[1]),
           from = 0, add = TRUE, col = "red")
pr2file("exponential//PctLargHouseFam_mle_plot.png")

### EXPONENTIAL: MM ###
mm <- function(x) {
    lambda <- 1 / mean(x)
    lambda
}

e <- mm(x)
plot(density(communities.and.crime$PctLargHouseFam))
curve(dexp(x, rate = e),
           from = 0, add = TRUE, col = "blue")
pr2file("exponential//PctLargHouseFam_mm_plot.png")



######### GAMMA FAMILY ######### Variable: PctNotHsGrad
hist(communities.and.crime$PctNotHSGrad, probability = TRUE)
# pr2file("gamma//PctNotHSGrad_histogram.png")
plot(density(communities.and.crime$PctNotHSGrad))
# pr2file("gamma//PctNotHSGrad_density_plot.png")
x <- communities.and.crime$PctNotHSGrad
n <- length(x)

### GAMMA: MLE ###
x[which(x == 0)] <- 0.1
ll <- function(k, theta) {
    loglik <- (k - 1) * sum(log(x)) - sum(x / theta) -
               n * k * log(theta) - n * log(gamma(k))
    -loglik
}

z <- mle(minuslogl = ll, start = c(list(k = 1), list(theta = 1)))
plot(density(communities.and.crime$PctNotHSGrad))
curve(dgamma(x, shape = coef(z)[1], scale = coef(z)[2]),
             from = 0, add = TRUE, col = "red")
pr2file("gamma//PctNotHSGrad_mle_plot.png")

### GAMMA: MM ###
mm <- function(x) {
    mu <- mean(x)
    theta <- mean(x * log(x)) - mu * mean(log(x))
    k <- mu / theta
    return(c(k, theta))
}

e <- mm(x)
plot(density(communities.and.crime$PctNotHSGrad))
curve(dgamma(x, shape = e[1], scale = e[2]),
             from = 0, add = TRUE, col = "blue")
pr2file("gamma//PctNotHSGrad_mm_plot.png")



######### BETA FAMILY ######### Variable: PctNotSpeakEnglWell
hist(communities.and.crime$PctNotSpeakEnglWell, probability = TRUE)
# pr2file("beta//PctNotSpeakEnglWell_histogram.png")
plot(density(communities.and.crime$PctNotSpeakEnglWell))
# pr2file("beta//PctNotSpeakEnglWell_density_plot.png")
x <- communities.and.crime$PctNotSpeakEnglWell
n <- length(x)

### BETA: MLE ###
x[which(x == 0)] <- 0.0001  # As close as we can get before errors
x[which(x == 1)] <- 0.9999

# x[which(x == 0)] <- 0.1   # Closer match with data,
# x[which(x == 1)] <- 0.9   # but farther match from MM

ll <- function(alpha, beta) {
    loglik <- n * log(gamma(alpha + beta)) - n * log(gamma(alpha)) -
              n * log(gamma(beta)) + (alpha - 1) * sum(log(x)) +
              (beta - 1) * sum(log(1 - x))
    -loglik
}

z <- mle(minuslogl = ll, start = c(list(alpha = 1), list(beta = 1)))
plot(density(communities.and.crime$PctNotSpeakEnglWell))
curve(dbeta(x, shape1 = coef(z)[1], shape2 = coef(z)[2]),
            from = 0, to = 1, add = TRUE, col = "red")
pr2file("beta//PctNotSpeakEnglWell_mle_plot.png")

### BETA: MM ###
mm <- function(x) {
    mu <- mean(x)
    var <- var(x)
    alpha <- mu * (mu * (1 - mu) / var - 1)
    beta <- (1 - mu) * (mu * (1 - mu) / var - 1)
    return(c(alpha, beta))
}

e <- mm(x)
plot(density(communities.and.crime$PctNotSpeakEnglWell))
curve(dbeta(x, shape1 = e[1], shape2 = e[2]),
            from = 0, to = 1, add = TRUE, col = "blue")
pr2file("beta//PctNotSpeakEnglWell_mm_plot.png")

