
## load DDR data
df <- read.csv("debate_data/DDR_results.csv")

## general difference
dfd <- subset(df, party == "D" & type == "mean")
dfr <- subset(df, party == "R" & type == "mean")

dff <- dfd[, 3:12] - dfr[, 3:12]
dff$year <- dfd$year
dff$total <- apply(dff[, 1:10], 1, function(x) sum(abs(x)))

library(ggplot2)

ggplot(dff, aes(x = year, y = total)) + geom_point() + geom_smooth()

# remove 1992
ggplot(subset(dff, !year %in% c(1992, 2012)), aes(x = year, y = total)) + geom_point() + geom_smooth(method = lm)

## fit regression
mod <- lm(total ~ year, subset(dff, !year %in% c(1992, 2012)))
summary(mod)


############## use nested data ###############
nt <- read.csv("debate_data/DDR_results_nested.csv")
#nt <- tidyr::separate(nt, ID, into = c("party", "round"), sep = "[A_Z]")
nt$party <- sub("[1-9]", "", nt$ID)
nt$round <- sub("[A-Z]", "", nt$ID)

ntD <- subset(nt, party == "D")
ntR <- subset(nt, party == "R")

ntDR <- ntD[, 3:12] - ntR[, 3:12]
ntDR <- cbind(ntDR, ntD[, c("year", "round")])
ntDR$total <- apply(ntDR[, 1:10], 1, function(x) sum(abs(x)))

ggplot(subset(ntDR, year != "2012"), aes(x = year, y = total)) + geom_point() + geom_smooth()

mod <- lm(scale(total) ~ year, ntDR)
summary(mod)

mod <- lm(scale(total) ~ year, subset(ntDR, year != "2012"))
summary(mod)

### factor analysis 
psych::fa.parallel(ntDR[, 1:10],fa='fa')

mod.f <- psych::fa(ntDR[, 1:10],2)
print(mod.f$loadings,sort=T,cut=.3) 

### correllation 
cor(ntDR[, 1:10])

### MLM

mlm <- lme4::lmer()




