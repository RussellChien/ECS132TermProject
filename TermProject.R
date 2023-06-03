# load in dataset
load('communities.and.crime.rda',verbose=T) 

# data analysis 
print(head(communities.and.crime))
density.default(communities.and.crime$PctLargeHouseFam) 

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

# exponential family

# gamma family

# beta family
