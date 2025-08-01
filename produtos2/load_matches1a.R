# load_matches1a.r
library(tidyverse)


qbq_cnct_matches <- read_csv("D:/Country/Brazil/TechBrazil/working/qbq/qbq_cnct_matches.csv")
cnct_qbq_matches <- read_csv("D:/Country/Brazil/TechBrazil/working/qbq/cnct_qbq_matches.csv")


save(cnct_qbq_matches, file=("D:/Country/Brazil/TechBrazil/working/qbq/cnct_qbq_matches.rda"))
save(qbq_cnct_matches, file=("D:/Country/Brazil/TechBrazil/working/qbq/qbq_cnct_matches.rda"))


load("D:/Country/Brazil/TechBrazil/working/qbq/qbq_ocup_cmento1.rda")






