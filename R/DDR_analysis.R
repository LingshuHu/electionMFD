
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

### total difference
ntDR$total <- apply(ntDR[, 1:10], 1, function(x) sum(abs(x)))

ggplot(ntDR, aes(x = year, y = total, color = round)) + 
  geom_point(size = 3) + 
  geom_smooth(method = lm, se = F, size = 1.5, color = "black") +
  theme_classic() + 
  scale_x_continuous(breaks=seq(1960,2016,10)) +
  theme(text = element_text(size = 22),
        legend.text=element_text(size=18),
        axis.text.x = element_text(angle = 45, hjust=1)) + 
  xlab("Year") + ylab("Total Difference") +
  labs(color = "Round")


ggplot(subset(ntDR, year != "2012"), aes(x = year, y = total)) + geom_point() + geom_smooth()

## fit regression
ntDR$year4 <- ntDR$year/4
mod1 <- lm(total ~ year4, ntDR)
summary(mod1)

leverages <- hatvalues(mod1) > 2 * mean(hatvalues(mod1))
ntDR[leverages, ]

library(dplyr)
dplyr::group_by(ntDR, year) %>% summarise(m = mean(total))

mod2 <- lm(total ~ year4, subset(ntDR, year != "2012"))
summary(mod2)

anova(mod1, mod2)

### one difference
ggplot(ntDR, aes(x = year, y = harm)) + geom_point() + geom_smooth(method = lm, formula = "y ~ x + x^2")

# show interaction
nt$id <- 1:nrow(nt)
nt_long <- tidyr::gather(nt, key = "moral_dim", value = "loading", 3:12)
nt_long$party <- factor(nt_long$party, levels = c("R", "D"))
nt_long$moral_dim <- factor(nt_long$moral_dim, levels = as.character(unique(nt_long$moral_dim)))
ggplot(nt_long, aes(x = year, y = loading, color = party)) + 
  geom_point() + geom_smooth(se = F) + 
  facet_wrap(~moral_dim) +
  theme_classic() +
  theme(text = element_text(size = 22),
        legend.text=element_text(size=18),
        axis.text.x = element_text(angle = 45, hjust=1)) + 
  xlab("Year") + ylab("Total Difference") +
  labs(color = "Party")

# show difference score
ntDR_dim_long <- tidyr::gather(ntDR, key = "moral_dim", value = "loading", 1:10)
ggplot(ntDR_dim_long, aes(x = year, y = loading)) + 
  geom_point() + geom_smooth(se = F) + 
  facet_wrap(~moral_dim) +
  theme_classic() +
  theme(text = element_text(size = 22),
        legend.text=element_text(size=18),
        axis.text.x = element_text(angle = 45, hjust=1)) + 
  xlab("Year") + ylab("Total Difference") +
  labs(color = "Party")

##### test each dimension
###### use moderation
morals <- unique(nt_long$moral_dim)
reg_dim <- vector("list", length = length(morals))
for (i in seq_along(morals)) {
  f <- as.formula(paste0(morals[i], "~", "year*party"))
  #m <- lm(f, nt)
  m <- lm(f, subset(nt, !year %in% c("2004", "2008", "2012")))
  coe <- summary(m)$coefficients
  coe <- data.frame(coe, row.names = NULL,
                    variable = rownames(coe),
                    moral_dim = morals[i])
  reg_dim[[i]] <- coe
}

reg_dim <- do.call("rbind", reg_dim)

sig <- which(reg_dim$Pr...t.. < .1)
reg_dim[sig, ]

##### use difference
morals <- unique(nt_long$moral_dim)
reg_dim2 <- vector("list", length = length(morals))
for (i in seq_along(morals)) {
  f <- as.formula(paste0(morals[i], "~", "year"))
  m <- lm(f, ntDR)
  coe <- summary(m)$coefficients
  coe <- data.frame(coe, row.names = NULL,
                    variable = rownames(coe),
                    moral_dim = morals[i])
  reg_dim2[[i]] <- coe
}

reg_dim2 <- do.call("rbind", reg_dim2)

sig <- which(reg_dim2$Pr...t.. < .05)
reg_dim2[sig, ]



### proportion 
ntp <- nt
ntp$sum <- apply(ntp[,3:12], 1, sum) # get moral sum of each round
ntp[,3:12] <- ntp[,3:12]/ntp$sum # get proportion of each dimention
ntp_long <- tidyr::gather(ntp, key = "moral_dim", value = "rate", 3:12)

ggplot(ntp_long, aes(x = year, y = rate, color = party)) + 
  geom_point() + geom_smooth() + facet_wrap(~moral_dim)

### factor analysis 
psych::fa.parallel(ntDR[, 1:10],fa='fa')

mod.f <- psych::fa(ntDR[, 1:10],2)
print(mod.f$loadings,sort=T,cut=.3) 

### correllation 
cor(ntDR[, 1:10])

### MLM
nt$id <- 1:nrow(nt)
nt_long <- tidyr::gather(nt, key = "moral_dim", value = "loading", 3:12)

nt_long$party <- factor(nt_long$party)
nt_long$party <- relevel(nt_long$party, ref = "D")
nt_long$moral_dim <- factor(nt_long$moral_dim)
nt_long$moral_dim <- relevel(nt_long$moral_dim, ref = "authority")
mlm <- lme4::lmer(loading ~ party + moral_dim + party * moral_dim + 
                    (1 | year) + (1 | year:round), nt_long)
mlm <- lmerTest::lmer(loading ~ party + moral_dim + party * moral_dim + 
                    (1 | year) + (1 | year:round), nt_long)
ss <- summary(mlm)
confint(mlm)
lattice::qqmath(mlm)

## generate moral differences for nested models
### function to run models with difference factor references
swith_factor_model <- function(data, formula, dims, refparty) {
  data$party <- factor(data$party)
  data$party <- relevel(data$party, ref = refparty)
  dim_diff <- vector("list", length = length(dims))
  for (i in seq_along(dims)) {
    # choose a moral dimention
    data$moral_dim <- factor(data$moral_dim)
    data$moral_dim <- relevel(data$moral_dim, 
                                 ref = dims[i])
    # run model
    mlm <- lme4::lmer(as.formula(formula), data)
    coe <- summary(mlm)$coefficients
    ci <- confint(mlm)[4:5, ]
    coe <- data.frame(coe, 
                      moral_dim = dims[i], 
                      party = row.names(coe),
                      row.names = NULL, stringsAsFactors = F)
    # change variable names
    coe$party <- gsub("(Intercept)", refparty, fixed = TRUE, coe$party)
    coe$party <- gsub("party", "", fixed = TRUE, coe$party)
    coe <- cbind(coe[1:2,], ci)
    dim_diff[[i]] <- coe
  }
  
  dim_diff <- do.call("rbind", dim_diff)
  return(dim_diff)
}

## based on value difference
dim_diffR <- swith_factor_model(
  nt_long, 
  formula = "loading ~ party + moral_dim + party * moral_dim + (1 | year) + (1 | year:round)",
  dims = as.character(unique(nt_long$moral_dim)),
  refparty = "R")

dim_diffD <- swith_factor_model(
  nt_long, 
  formula = "loading ~ party + moral_dim + party * moral_dim + (1 | year) + (1 | year:round)",
  dims = as.character(unique(nt_long$moral_dim)),
  refparty = "D")

diff_sig <- subset(dim_diffR, party == "D")
diff_sig <- subset(diff_sig, (`2.5 %` > 0 & `97.5 %` >0)|(`2.5 %` < 0 & `97.5 %` < 0))

dim_diffDD <- subset(dim_diffD, party == "D")
dim_diffRR <- subset(dim_diffR, party == "R")
dim_diff <- rbind(dim_diffDD, dim_diffRR)

dim_diff <- dplyr::mutate(dim_diff,
                          moral_dim = ifelse(dim_diff$moral_dim %in% diff_sig$moral_dim, 
                                             paste0(dim_diff$moral_dim, "*"), dim_diff$moral_dim)
)

## based on proportional difference
dim_pdiffR <- swith_factor_model(
  ntp_long, 
  formula = "rate ~ party + moral_dim + party * moral_dim + (1 | year) + (1 | year:round)",
  dims = as.character(unique(ntp_long$moral_dim)),
  refparty = "R")

dim_pdiffD <- swith_factor_model(
  ntp_long, 
  formula = "rate ~ party + moral_dim + party * moral_dim + (1 | year) + (1 | year:round)",
  dims = as.character(unique(ntp_long$moral_dim)),
  refparty = "D")

ntp_long$rate100 <- ntp_long$rate*100
mlmp <- lme4::lmer(rate100 ~ party + moral_dim + 
                     party * moral_dim + (1 | year) + (1 | year:round), 
                   ntp_long)
summary(mlmp)

diffp_sig <- subset(dim_pdiffR, party == "D")
diffp_sig <- subset(diffp_sig, (`2.5 %` > 0 & `97.5 %` >0)|(`2.5 %` < 0 & `97.5 %` < 0))

dim_pdiffDD <- subset(dim_pdiffD, party == "D")
dim_pdiffRR <- subset(dim_pdiffR, party == "R")
dim_pdiff <- rbind(dim_pdiffDD, dim_pdiffRR)

dim_pdiff <- dplyr::mutate(dim_pdiff,
                          moral_dim = ifelse(dim_pdiff$moral_dim %in% diffp_sig$moral_dim, 
                                             paste0(dim_pdiff$moral_dim, "*"), dim_pdiff$moral_dim)
)


dim_diff_wide <- tidyr::spread(dim_diff[, c("Estimate", "moral_dim", "party")], 
                               key = party, value = Estimate)

write.csv(dim_diff, "results/dim_diff_party.csv")
write.csv(dim_diff_wide, "results/dim_diff_party.csv")

D_care_mean <- mean(subset(nt_long, party == "D" & moral_dim == "care")$loading)
R_care_mean <- mean(subset(nt_long, party == "R" & moral_dim == "care")$loading)

sd(subset(nt_long, party == "D" & moral_dim == "care")$loading)
sd(subset(nt_long, party == "D" & moral_dim == "harm")$loading)


## plot moral differences between parties
colnames(dim_diff) <- c("Estimate", "SD", "t.value", "moral_dim", "party", "CIlow", "CIhigh")
dim_diff$moral_dim <- factor(dim_diff$moral_dim, levels = as.character(unique(dim_diff$moral_dim)))
dim_diff$party <- factor(dim_diff$party, levels = c("R", "D"))

ggplot(dim_diff, aes(x = moral_dim, y = Estimate, fill = party)) + 
  geom_bar(stat="identity", position = "dodge") +
  geom_errorbar(aes(ymin=CIlow, ymax=CIhigh), width=.5,
                position=position_dodge(.9)) +
  theme_classic() + 
  theme(text = element_text(size = 22),
        legend.text=element_text(size=18),
        axis.text.x = element_text(angle = 45, hjust=1)) + 
  xlab("Moral Dimension") + ylab("Moral Loading") +
  labs(fill = "Party")

## based on proportion
colnames(dim_pdiff) <- c("Estimate", "SD", "t.value", "moral_dim", "party", "CIlow", "CIhigh")
dim_pdiff$moral_dim <- factor(dim_pdiff$moral_dim, levels = as.character(unique(dim_pdiff$moral_dim)))
dim_pdiff$party <- factor(dim_pdiff$party, levels = c("R", "D"))

ggplot(dim_pdiff, aes(x = moral_dim, y = Estimate, fill = party)) + 
  geom_bar(stat="identity", position = "dodge") +
  geom_errorbar(aes(ymin=CIlow, ymax=CIhigh), width=.5,
                position=position_dodge(.9)) +
  theme_classic() + 
  theme(text = element_text(size = 22),
        legend.text=element_text(size=18),
        axis.text.x = element_text(angle = 45, hjust=1)) + 
  xlab("Moral Dimension") + ylab("Moral Loading") +
  labs(fill = "Party")

## effects of round
mlm2 <- lme4::lmer(total~ round + (1|year), ntDR_long)
summary(mlm2)
confint(mlm2)
performance::icc(mlm2)
qqmath(mlm2)

#coe <- coef(mlm)
#mean(coe[[1]]$partyD)
#mean(coe[[1]]$`(Intercept)`)

VarCorr(mlm)
confint(mlm)

sjstats::icc(mlm)
performance::icc(mlm)
# get icc
# https://stats.stackexchange.com/questions/174071/how-to-compute-intraclass-correlation-icc-for-three-level-negative-binomial-hi

alpha <- lme4::getME(mlm, "sigma")^2 # invividual variance

vars <- VarCorr(mlm)
var_l2 <- vars[[1]][1]
var_l3 <- vars[[2]][1]

ICC_l2 <- (var_l3 + var_l2)/(var_l3 + var_l2 + alpha) 
ICC_l3 <- var_l3/(var_l3 + var_l2 + alpha) 




