#Wrapper for Wouter's replication of Zhang & Luck simultaneous arrays

matlab -nojvm -nodesktop -r "run scripts/ZLvstmsave.m"
Rscript scripts/analylze.r
