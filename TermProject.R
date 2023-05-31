# load in 'law.school.admissions.rda'
load('law.school.admissions.rda',verbose=T) 

# data analysis 
print(head(law.school.admissions))
hist(law.school.admissions$fam_inc) 

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
# use lsat or fam_inc

# exponential family
# idk

# gamma family
# use lsat or fam_inc

# beta family
# use ugpa