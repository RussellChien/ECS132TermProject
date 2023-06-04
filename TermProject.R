# load in dataset
load('communities.and.crime.rda',verbose=T) 

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

# normal family
hist(communities.and.crime$pctWWage, probability = TRUE)
pr2file('pctWWage_histogram.png')
plot(density(communities.and.crime$pctWWage))
pr2file('PctWWage_density_plot.png')

# exponential family
hist(communities.and.crime$PctLargHouseFam, probability = TRUE)
pr2file('PctLargHouseFam_histogram.png')
plot(density(communities.and.crime$PctLargHouseFam))
pr2file('PctLargHouseFam_density_plot.png')

# gamma family
hist(communities.and.crime$PctNotHSGrad, probability = TRUE)
pr2file('PctNotHSGrad_histogram.png')
plot(density(communities.and.crime$PctNotHSGrad))
pr2file('PctNotHSGrad_density_plot.png')

# beta family
hist(communities.and.crime$PctNotSpeakEnglWell, probability = TRUE)
pr2file('PctNotSpeakEnglWell_histogram.png')
plot(density(communities.and.crime$PctNotSpeakEnglWell))
pr2file('PctNotSpeakEnglWell_density_plot.png')



