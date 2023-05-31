# load in 'law.school.admissions.rda'
load('law.school.admissions.rda',verbose=T) 

print(head(law.school.admissions))
hist(law.school.admissions$lsat) 
