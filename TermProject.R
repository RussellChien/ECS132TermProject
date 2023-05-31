# load in 'law.school.admissions.rda'
load('law.school.admissions.rda',verbose=T) 

print(head(law.school.admissions))
hist(law.school.admissions$fam_inc) 

# normal family
# use lsat or fam_inc

# exponential family
# idk

# gamma family
# use lsat or fam_inc

# beta family
# use ugpa