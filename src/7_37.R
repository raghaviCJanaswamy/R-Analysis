##Create Vector with 230 Subjects

Total_Population <- 1:230
Total_Population

## Ginko Group (Half of the above)
ginko_sample <- sample(Total_Population, 115, replace = F)
ginko_sample

## placebo Group (The remaining Half)

placebo_sample <- setdiff(Total_Population, ginko_sample)
placebo_sample

## report the first 20 members of the ginko group
head(ginko_sample,20)

## report the 5 members of Ginko group starting from 103

Ginko_subsample <- ginko_sample[c(102:106)]

Ginko_subsample
