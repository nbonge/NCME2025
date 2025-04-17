# == == == == == == == == == == == == == == == 
# ==              Nicole Bonge              ==
# ==        Thursday, May 30, 2024          ==
# ==           Project Code - TIMSS         ==
# ==             Response Time              ==
# == == == == == == == == == == == == == == ==

# 1.0 Preliminaries ----
rm(list=ls())
getwd()

## 1.1 Libraries ----
library(plyr)
library(tidyverse)
library(haven)
library(irt)
library(irtoys)
library(mirt)
library(readxl)
library(labelled)
library(psych)
library(reshape2)
library(lme4)
library(lmerTest)
library(performance)
library(car)
library(ggeffects)
library(signs)

# 1.2 Functions ----
trim_book <- function(booklet){
  out <- as.data.frame(booklet) %>%
    filter(!grepl("A$",items)) %>%
    filter(!grepl("B$",items)) %>% 
    filter(!grepl("C$",items)) %>%
    filter(!grepl("D$",items)) %>%
    filter(!grepl("E$",items)) %>%
    filter(!grepl("F$",items)) %>%
    filter(!grepl("G$",items))
  return(out)
}

key.fun <- function(booklet){
  # This function returns a dataframe with a key appropriate for 
  # psych::score.multiple.choice
  b.no <- as.integer(booklet)
  out <- key %>%
    dplyr::filter(key$Variable %in% as.character(booklets[[b.no]]$trim$items)) %>%
    mutate(Correct.Answer = case_when(
      Correct.Answer == 10 ~ 10,
      Correct.Answer == 20 ~ 20,
      Correct.Answer == "A" ~ 1,
      Correct.Answer == "B" ~ 2,
      Correct.Answer == "C" ~ 3,
      Correct.Answer == "D" ~ 4,
    )
    ) %>%
    dplyr::select("Correct.Answer")
}

score <- function(data,booklet){
  scored.data <- data
  n <- dim(booklets[[booklet]]$trim)[1] # cols
  for(i in 1:n){
    item <- colnames(scored.data)[i]
    # finding item location in key
    item.key.location <- which(key$Variable == item)
    # matching entry to key
    ans <- key$Correct.Answer[item.key.location]
    # converting letters to numbers
    ans.num <- case_when(
      ans == 10 ~ 10,
      ans == 20 ~ 20,
      ans == "A" ~ 1,
      ans == "B" ~ 2,
      ans == "C" ~ 3,
      ans == "D" ~ 4,
    )
    item.no <- paste0("item.",i)
    item.time <- paste0("item.",i,".time")
    scored.data <- scored.data %>%
      dplyr::mutate({{item.no}} := ifelse(scored.data[i] == ans.num, 1, 0)) %>%
      dplyr::rename({{item.time}} := paste0(item,"_S"))
  }
  return(scored.data)
}

# 2.0 Import Data ----
## 2.1 Import dataset ----
dat_all <- read_sav(here::here("datasets", "bsausaz7.sav"))
# View(dat)

## 2.2 Import Key (Correct Answers) ----
key <- read_excel(here::here("datasets","eT19PSI_G8_Codebook_keyed.xlsx"),
                  sheet = "BSAZ7") %>%
  dplyr::select(c("Variable",`Question Location`, `Correct Answer`))

colnames(key) <- c("Variable", "Question.Location", "Correct.Answer")

key <- key %>%
  filter(grepl("^ME",Variable)) %>%
  filter(!grepl("_F$", Variable))

n_booklets <- 14

## 2.3 Developing booklets ----
key.1 <- key %>%
  mutate(Booklet = case_when(
    substr(key$Question.Location,4,5) == "01" ~ 1,
    substr(key$Question.Location,4,5) == "02" ~ 2,
    substr(key$Question.Location,4,5) == "03" ~ 3,
    substr(key$Question.Location,4,5) == "04" ~ 4,
    substr(key$Question.Location,4,5) == "05" ~ 5,
    substr(key$Question.Location,4,5) == "06" ~ 6,
    substr(key$Question.Location,4,5) == "07" ~ 7,
    substr(key$Question.Location,4,5) == "08" ~ 8,
    substr(key$Question.Location,4,5) == "09" ~ 9,
    substr(key$Question.Location,4,5) == "10" ~ 10,
    substr(key$Question.Location,4,5) == "11" ~ 11,
    substr(key$Question.Location,4,5) == "12" ~ 12,
    substr(key$Question.Location,4,5) == "13" ~ 13,
    substr(key$Question.Location,4,5) == "14" ~ 14
  )) %>%
  filter(!grepl("_S$",Variable))

derived_qs <- c("ME52007", "ME72180", "ME72198", "ME72170", "ME62244", 
                "ME72178", "ME72020", "ME72052", "ME72164", "ME52502", 
                "ME62288", "ME52087", "ME72055", "ME62317", "ME72095",
                "ME72232", "ME62215", "ME72225", "ME62170", "ME62048",
                "ME72081", "ME72140")

booklets <- vector("list", length = n_booklets)

for(i in 1:n_booklets){
  book <- data.frame("items" = character())
  for(j in 1:nrow(key.1)){
    if(key.1$Booklet[j] == i){
      book <- rbind(book,key.1$Variable[j])
      colnames(book) <- c("items")
    }
  }
  
  # gathering isolated items
  book_trim <- trim_book(book) %>%
    filter(!(items %in% derived_qs))
  
  # gathering associated response times
  book_S <- data.frame("items"= paste0(book_trim$items,"_S"))
  
  # storing information in "booklets" list
  booklets[[i]] = list("booklet.no" = i,
                       "items" = book, 
                       "trim" = book_trim, 
                       "time" = book_S)
}

## 2.4 Keys ----
keys <- vector("list", length = n_booklets)

for(i in 1:n_booklets){
  keys[[i]] <- key.fun(i)
}

## 2.5 Cleaning Up ----
rm("derived_qs", "book", "book_S", "book_trim")

# 3.0 Score Data ----
## 3.1 Gathering scores ----
datasets <- vector("list", length = n_booklets)

for(i in 1:n_booklets){
  dat <- dat_all %>% remove_labels %>%
    filter(IDBOOK == i) %>%
    dplyr::select(as.character(booklets[[i]]$trim$items), 
                  as.character(booklets[[i]]$time$items),
                  "BSPMRGP","IDBOOK", "IDSTUD") %>%
    score(booklet = i) %>%
    dplyr::select("BSPMRGP","IDBOOK", "IDSTUD", 
                  all_of(starts_with("item")),all_of(ends_with("_S"))) %>%
    dplyr::rename(id = IDSTUD)
  datasets[[i]] <- dat
}

# 4.0 Booklet Comparison ----
# comparing booklets # trim items and sample size
booklet.overview <- data.frame("booklet" = numeric(),
                               "isolated.items" = numeric(),
                               "sample.size" = numeric())
for(i in 1:n_booklets){
  sample.size <- as.numeric(dim(datasets[[i]])[1]) # total responses
  booklet.overview[i,] <- c(i, # booklet number
                            dim(booklets[[i]]$trim)[1], # number of items
                            sample.size # total responses # complete responses
  )
  
}


# 5.0 IRT Analysis ----

# choosing booklet 14 bc most items w/ most respondents
dat <- datasets[[14]] %>% na.omit()


## 5.1 Initial Modeling ----
mod.1PL <- mirt::mirt(data = dat[ ,17:29], 
                      model = 1, itemtype = "Rasch", SE = TRUE, guess = 0, 
                      technical = list(NCYCLES = 1000), TOL = 1e-7)

dat.guess <- keys[[14]] %>%
  mutate(dat.guess = case_when(
    Correct.Answer == 1 | Correct.Answer == 2 | Correct.Answer == 3 | Correct.Answer == 4 ~ 1/4,
    Correct.Answer == 20 | Correct.Answer == 10 ~ 1/1000
  ))

mod.quasi.1PL <- mirt::mirt(data = dat[, 17:29],
                            model = 1, itemtype = "Rasch", SE = TRUE, 
                            guess = dat.guess$dat.guess, 
                            technical = list(NCYCLES = 1000), TOL = 1e-7)


mod.2PL <- mirt::mirt(data = dat[, 17:29],
                      model = 1, itemtype = "2PL", SE = TRUE, 
                      technical = list(NCYCLES = 1000), TOL = 1e-7)


mod.3PL <- mirt::mirt(data = dat[, 17:29], 
                      model = 1, itemtype = "3PL", SE = TRUE, 
                      technical = list(NCYCLES = 1000), TOL = 1e-7)


mod.3PL.guess <- mirt::mirt(data = dat[, 17:29], 
                            model = 1, itemtype = "3PL", SE = TRUE, 
                            guess = dat.guess$dat.guess, 
                            technical = list(NCYCLES = 1000), TOL = 1e-7)

## 5.2 Model Comparison ----
AIC.1PL <- extract.mirt(mod.1PL, "AIC")
AIC.Q1PL <- extract.mirt(mod.quasi.1PL, "AIC")
AIC.2PL <- extract.mirt(mod.2PL, "AIC")
AIC.3PL <- extract.mirt(mod.3PL, "AIC")
AIC.G3PL <- extract.mirt(mod.3PL.guess, "AIC")


BIC.1PL <- extract.mirt(mod.1PL, "BIC")
BIC.Q1PL <- extract.mirt(mod.quasi.1PL, "BIC")
BIC.2PL <- extract.mirt(mod.2PL, "BIC")
BIC.3PL <- extract.mirt(mod.3PL, "BIC")
BIC.G3PL <- extract.mirt(mod.3PL.guess, "BIC")

SABIC.1PL <- extract.mirt(mod.1PL, "SABIC")
SABIC.Q1PL <- extract.mirt(mod.quasi.1PL, "SABIC")
SABIC.2PL <- extract.mirt(mod.2PL, "SABIC")
SABIC.3PL <- extract.mirt(mod.3PL, "SABIC")
SABIC.G3PL <- extract.mirt(mod.3PL.guess, "SABIC")

LL.1PL <- extract.mirt(mod.1PL, "logLik")
LL.Q1PL <- extract.mirt(mod.quasi.1PL, "logLik")
LL.2PL <- extract.mirt(mod.2PL, "logLik")
LL.3PL <- extract.mirt(mod.3PL, "logLik")
LL.G3PL <- extract.mirt(mod.3PL.guess, "logLik")


IRT.model.comparison <- data.frame(
  "AIC" = c(AIC.1PL, AIC.Q1PL, AIC.2PL, AIC.3PL, AIC.G3PL),
  "BIC" = c(BIC.1PL, BIC.Q1PL, BIC.2PL, BIC.3PL, BIC.G3PL),
  "SABIC" = c(SABIC.1PL, SABIC.Q1PL, SABIC.2PL, SABIC.3PL, SABIC.G3PL),
  "LL" = c(LL.1PL, LL.Q1PL, LL.2PL, LL.3PL, LL.G3PL),
  row.names = c("1PL", "Q1PL", "2PL", "3PL", "3PLG")
)


# 2PL model wins!
# Cleaning up
rm(list = AIC.1PL, AIC.Q1PL, AIC.2PL, AIC.3PL, AIC.G3PL, 
   BIC.1PL, BIC.Q1PL, BIC.2PL, BIC.3PL, BIC.G3PL,
   SABIC.1PL, SABIC.Q1PL, SABIC.2PL, SABIC.3PL, SABIC.G3PL,
   LL.1PL, LL.Q1PL, LL.2PL, LL.3PL, LL.G3PL, dat.guess)
rm(mod.1PL, mod.quasi.1PL, mod.3PL, mod.3PL.guess)


# 6.0 Data Analysis ----
## 6.1 Descriptives ----
parms <- coef(mod.2PL, IRTpars = TRUE, simplify = TRUE)
mean.correct <- c(mean(dat$item.1), mean(dat$item.2), mean(dat$item.3),
                  mean(dat$item.4), mean(dat$item.5), mean(dat$item.6), 
                  mean(dat$item.7), mean(dat$item.8), mean(dat$item.9),
                  mean(dat$item.10), mean(dat$item.11), mean(dat$item.12),
                  mean(dat$item.13))

mean.time <- c(mean(dat$item.1.time), mean(dat$item.2.time), 
               mean(dat$item.3.time), mean(dat$item.4.time), 
               mean(dat$item.5.time), mean(dat$item.6.time), 
               mean(dat$item.7.time), mean(dat$item.8.time), 
               mean(dat$item.9.time), mean(dat$item.10.time), 
               mean(dat$item.11.time), mean(dat$item.12.time),
               mean(dat$item.13.time))

sd.correct <- c(sd(dat$item.1), sd(dat$item.2), sd(dat$item.3),
                sd(dat$item.4), sd(dat$item.5), sd(dat$item.6), 
                sd(dat$item.7), sd(dat$item.8), sd(dat$item.9),
                sd(dat$item.10), sd(dat$item.11), sd(dat$item.12),
                sd(dat$item.13))

sd.time <- c(sd(dat$item.1.time), sd(dat$item.2.time), 
             sd(dat$item.3.time), sd(dat$item.4.time), 
             sd(dat$item.5.time), sd(dat$item.6.time), 
             sd(dat$item.7.time), sd(dat$item.8.time), 
             sd(dat$item.9.time), sd(dat$item.10.time), 
             sd(dat$item.11.time), sd(dat$item.12.time),
             sd(dat$item.13.time))

# Combining descriptives to "descriptives" table
descriptives <- data.frame("item" = paste0(1:13),
                           "sample" = rep(552,13),
                           "difficulty" = parms$items[,2],
                           "discrimination" = parms$items[,1],
                           "resp.Correct.Mean" = mean.correct,
                           "resp.Correct.Sdev" = sd.correct,
                           "resp.Time.Mean" = mean.time,
                           "resp.Time.Sdev" = sd.time)
descriptives[,3:8] <- descriptives[,3:8] %>% round(digits = 2)

# checking factor scores
mod.theta <- data.frame(dat$id, fscores(mod.2PL))
colnames(mod.theta) <- c("id", "ability")

# cleaning up
rm(parms,mean.correct,sd.correct,mean.time,sd.time)

## 6.2 Data Transformation for Main Analysis ----

## create long data for item correctness
correct.long <- melt(dat[3:29], id.vars = "id", 
                     measure.vars = paste0("item.", 1:13),
                     variable.name = "item", value.name = "correct")

# transform item variable to numeric
correct.long$item <- as.numeric(as.factor(correct.long$item))


## create long dataset for item response time
time.long <- melt(dat[3:29], id.vars = "id", 
                  measure.vars = paste0("item.", 1:13, ".time"),
                  variable.name = "item", value.name = "time")
# transform item variable to numeric
time.long$item <- as.numeric(as.factor(time.long$item))


## merge the long data to create to the final dataset
dat.long <- merge(correct.long, time.long, by = c("id", "item"))

# assign item difficulty to every item
dat.long <- merge(dat.long, 
                  descriptives[, c("item", "difficulty", "discrimination")],
                  by = "item")

# assign ability to every participant
dat.long <- merge(dat.long, mod.theta , by = "id")

# compute ability-difficulty distance for every person and item
dat.long$distance <- dat.long$ability - dat.long$difficulty

# compute an absolute value of that distance
dat.long$distance.abs <- abs(dat.long$distance)

# multiply ability-difficulty distance by discrimination
dat.long$discrim.dist.abs <- dat.long$discrimination * dat.long$distance.abs

# sort rows by id and items
dat.long <- dat.long[order(dat.long$id, dat.long$item), ]
rownames(dat.long) <- 1:nrow(dat.long)


# Cleaning up
rm(time.long,correct.long,mod.theta)
# 7.0 Main Analysis ----

## 7.1 Null Model ----
model.0 <- lmer(log(time) ~ 1 + (1 | id) + (1 | item), data = dat.long) # intercept

model.0.res <- list("model" = model.0, # model
                    "summary" = summary(model.0), # model coefficients
                    "random.effects" = rand(model.0), # significance test for random effects
                    "conf.ints" = confint(model.0), # confidence intervals for parameters
                    "r2" = r2(model.0))  # R-squared statistic
# note: random effects are in std. deviations squared to obtain variance

## 7.2 Model A1 ----
## (F > C phenomenon only)
model.a1 <- lmer(log(time) ~ correct + (1 | id) + (1 | item), data = dat.long)
model.a1.res <- list("model" = model.a1, # model
                     "summary" = summary(model.a1), # model coefficients
                     "random.effects" = rand(model.a1), # significance test for random effects
                     "conf.ints" = confint(model.a1), # confidence intervals for parameters
                     "r2" = r2(model.a1))  # R-squared statistic
# note: random effects are in std. deviations squared to obtain variance


## 7.3 Model A2 ----
## (controlling for difficulty and ability)
model.a2 <- lmer(log(time) ~ correct + difficulty + ability + (1 | id) + (1 | item), 
                 data = dat.long)

model.a2.res <- list("model" = model.a2, # model
                     "summary" = summary(model.a2), # model coefficients
                     "vif" = vif(model.a2), # multicollinearity check
                     "random.effects" = rand(model.a2), # significance test for random effects
                     "conf.ints" = confint(model.a2), # confidence intervals for parameters
                     "r2" = r2(model.a2))  # R-squared statistic
# note: random effects are in std. deviations squared to obtain variance


## 7.4 Model A3 ---- 
## (F > C and ability interaction)
model.a3 <- lmer(log(time) ~ correct + difficulty + ability + ability:correct + 
                   (1 | id) + (1 | item), data = dat.long)

model.a3.res <- list("model" = model.a3, # model
                     "summary" = summary(model.a3), # model coefficients
                     "vif" = vif(model.a3), # multicollinearity check
                     "random.effects" = rand(model.a3), # significance test for random effects
                     "conf.ints" = confint(model.a3), # confidence intervals for parameters
                     "r2" = r2(model.a3))  # R-squared statistic
# note: random effects are in std. deviations squared to obtain variance


## 7.5 Model A4 ----
## (Exploratory model with difficulty and ability interaction)
model.a4 <- lmer(log(time) ~ correct + difficulty + ability + ability:correct + difficulty:ability + 
                   (1 | id) + (1 | item), data = dat.long)

model.a4.res <- list("model" = model.a4, # model
                     "summary" = summary(model.a4), # model coefficients
                     "vif" = vif(model.a4), # multicollinearity check
                     "random.effects" = rand(model.a4), # significance test for random effects
                     "conf.ints" = confint(model.a4), # confidence intervals for parameters
                     "r2" = r2(model.a4))  # R-squared statistic
# note: random effects are in std. deviations squared to obtain variance


## Comparison of 0, A1, A2, A3, and A4 models
model.comparison1 <- anova(model.0, model.a1, model.a2, model.a3)

## 7.6 Model B1 ----
## (Thissen's model)
model.b1 <- lmer(log(time) ~ discrim.dist.abs + (1 | id) + (1 | item), 
                 data = dat.long)

model.b1.res <- list("model" = model.b1, # model
                     "summary" = summary(model.b1), # model coefficients
                     "random.effects" = rand(model.b1), # significance test for random effects
                     "conf.ints" = confint(model.b1), # confidence intervals for parameters
                     "r2" = r2(model.b1))  # R-squared statistic
# note: random effects are in std. deviations squared to obtain variance



## 7.7 Model B2 ----
## (controlling for item correctness)
model.b2 <- lmer(log(time) ~ discrim.dist.abs + correct + (1 | id) + (1 | item), 
                 data = dat.long)

model.b2.res <- list("model" = model.b2, # model
                     "summary" = summary(model.b2), # model coefficients
                     "vif" = vif(model.b2), # multicollinearity check
                     "random.effects" = rand(model.b2), # significance test for random effects
                     "conf.ints" = confint(model.b2), # confidence intervals for parameters
                     "r2" = r2(model.b2))  # R-squared statistic
# note: random effects are in std. deviations squared to obtain variance



## 7.8 Model B3 ----
## (ability-difficulty distance and item correctness interaction)
model.b3 <- lmer(log(time) ~ discrim.dist.abs + correct + discrim.dist.abs:correct + 
                   (1 | id) + (1 | item), data = dat.long)

model.b3.res <- list("model" = model.b3, # model
                     "summary" = summary(model.b3), # model coefficients
                     "vif" = vif(model.b3), # multicollinearity check
                     "random.effects" = rand(model.b3), # significance test for random effects
                     "conf.ints" = confint(model.b3), # confidence intervals for parameters
                     "r2" = r2(model.b3))  # R-squared statistic
# note: random effects are in std. deviations squared to obtain variance




## Comparison of 0, B1, B2, and B3 models
model.comparison2 <- anova(model.0, model.b1, model.b2, model.b3) %>%
  round(digits = 3)

## 7.9 Model Overview ----
model.overview <- list("descriptives" = descriptives,
                       "model.0" = model.0.res,
                       "model.a1" = model.a1.res,
                       "model.a2" = model.a2.res,
                       "model.a3" = model.a3.res,
                       "model.b1" = model.b1.res,
                       "model.b2" = model.b2.res,
                       "model.b3" = model.b3.res
)

# Cleaning Up
rm(list = c("model.0","model.0.res","model.a1","model.a2","model.a3","model.a4",
            "model.b1", "model.b2", "model.b3", 
            "model.0.res","model.a1.res", "model.a2.res",
            "model.a3.res","model.a4.res",
            "model.b1.res", "model.b2.res", "model.b3.res"))

## 7.10 Generating Outputs ----

mod.0.result.out <- data.frame("estimate" = round(model.overview[["model.0"]][["model"]]@beta, digits = 3),
                               "significance" = round(model.overview[["model.0"]][["summary"]][["coefficients"]][,5], 
                                                      digits = 3),
                               "lowerCI" = round(model.overview[["model.0"]][["conf.ints"]][4,1], digits = 3),
                               "upperCI" = round(model.overview[["model.0"]][["conf.ints"]][4,2], digits = 3)
)

mod.a1.result.out <- data.frame("estimate" = round(model.overview[["model.a1"]][["model"]]@beta, digits = 3),
                                "significance" = round(model.overview[["model.a1"]][["summary"]][["coefficients"]][,5], 
                                                       digits = 3),
                                "lowerCI" = round(model.overview[["model.a1"]][["conf.ints"]][4:5,1], digits = 3),
                                "upperCI" = round(model.overview[["model.a1"]][["conf.ints"]][4:5,2], digits = 3)
)

mod.a2.result.out <- data.frame("estimate" = round(model.overview[["model.a2"]][["model"]]@beta, digits = 3),
                                "significance" = round(model.overview[["model.a2"]][["summary"]][["coefficients"]][,5], 
                                                       digits = 3),
                                "lowerCI" = round(model.overview[["model.a2"]][["conf.ints"]][4:7,1], digits = 3),
                                "upperCI" = round(model.overview[["model.a2"]][["conf.ints"]][4:7,2], digits = 3)
)

mod.a3.result.out <- data.frame("estimate" = round(model.overview[["model.a3"]][["model"]]@beta, digits = 3),
                                "significance" = round(model.overview[["model.a3"]][["summary"]][["coefficients"]][,5], 
                                                       digits = 3),
                                "lowerCI" = round(model.overview[["model.a3"]][["conf.ints"]][4:8,1], digits = 3),
                                "upperCI" = round(model.overview[["model.a3"]][["conf.ints"]][4:8,2], digits = 3)
)

mod.b1.result.out <- data.frame("estimate" = round(model.overview[["model.b1"]][["model"]]@beta, digits = 3),
                                "significance" = round(model.overview[["model.b1"]][["summary"]][["coefficients"]][,5], 
                                                       digits = 3),
                                "lowerCI" = round(model.overview[["model.b1"]][["conf.ints"]][4:5,1], digits = 3),
                                "upperCI" = round(model.overview[["model.b1"]][["conf.ints"]][4:5,2], digits = 3)
)

mod.b2.result.out <- data.frame("estimate" = round(model.overview[["model.b2"]][["model"]]@beta, digits = 3),
                                "significance" = round(model.overview[["model.b2"]][["summary"]][["coefficients"]][,5], 
                                                       digits = 3),
                                "lowerCI" = round(model.overview[["model.b2"]][["conf.ints"]][4:6,1], digits = 3),
                                "upperCI" = round(model.overview[["model.b2"]][["conf.ints"]][4:6,2], digits = 3)
)

mod.b3.result.out <- data.frame("estimate" = round(model.overview[["model.b3"]][["model"]]@beta, digits = 3),
                                "significance" = round(model.overview[["model.b3"]][["summary"]][["coefficients"]][,5], 
                                                       digits = 3),
                                "lowerCI" = round(model.overview[["model.b3"]][["conf.ints"]][4:7,1], digits = 3),
                                "upperCI" = round(model.overview[["model.b3"]][["conf.ints"]][4:7,2], digits = 3)
)

model.fits <- data.frame("conditional.R2" = c(model.overview[["model.0"]][["r2"]][["R2_conditional"]],
                                              model.overview[["model.a1"]][["r2"]][["R2_conditional"]],
                                              model.overview[["model.a2"]][["r2"]][["R2_conditional"]],
                                              model.overview[["model.a3"]][["r2"]][["R2_conditional"]],
                                              model.overview[["model.b1"]][["r2"]][["R2_conditional"]],
                                              model.overview[["model.b2"]][["r2"]][["R2_conditional"]],
                                              model.overview[["model.b3"]][["r2"]][["R2_conditional"]]),
                         "marginal.R2" = c(model.overview[["model.0"]][["r2"]][["R2_marginal"]],
                                           model.overview[["model.a1"]][["r2"]][["R2_marginal"]],
                                           model.overview[["model.a2"]][["r2"]][["R2_marginal"]],
                                           model.overview[["model.a3"]][["r2"]][["R2_marginal"]],
                                           model.overview[["model.b1"]][["r2"]][["R2_marginal"]],
                                           model.overview[["model.b2"]][["r2"]][["R2_marginal"]],
                                           model.overview[["model.b3"]][["r2"]][["R2_marginal"]]),
                         "Log Likelihood" = c(model.comparison1$logLik[1:4], 
                                              model.comparison2$logLik[2:4]),
                         "AIC" = c(model.comparison1$AIC[1:4], 
                                   model.comparison2$AIC[2:4]),
                         "BIC" = c(model.comparison1$BIC[1:4], 
                                   model.comparison2$BIC[2:4]),
                         "Chi.Sq" = c(model.comparison1$Chisq[1:4], 
                                      model.comparison2$Chisq[2:4]),
                         "df" = c(model.comparison1$Df[1:4], 
                                  model.comparison2$Df[2:4]),
                         "p.val" = c(model.comparison1$`Pr(>Chisq)`[1:4], 
                                     model.comparison2$`Pr(>Chisq)`[2:4]),
                         row.names = c("Model 0","Model A1","Model A2","Model A3",
                                       "Model B1","Model B2","Model B3")) %>%
  round(digits = 3)




# 8.0 Plots ----
library("ggplot2")
library("ggeffects")
library("signs")
library("extrafont")


figure.a3.data <- ggpredict(model.overview$model.a3$model, terms = c("ability [all]", "correct"))

figure.a3 <- plot(figure.a3.data, show_ci = F, line_size = 2) + 
  labs(x = "Ability (θ)", y = "Reponse Time (in seconds)", title = NULL, color = "Response") + 
  scale_x_continuous(breaks = -2:2.25, limits = c(-2, 2.25), labels = signs::signs_format()) + 
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) + 
  scale_color_manual(values = c("#DB060B", "#4DB264"), labels = c("False", "Correct")) + 
  scale_fill_manual(values = c("#DB060B", "#4DB264")) + 
  theme_bw(base_size = 16) + 
  theme(axis.text = element_text(color = "black"),
        legend.position = "bottom")


# visualize the plot
figure.a3


# compute predicted values of Model B3 as a data source for the plot
figure.b3.data <- ggpredict(model.overview[["model.b3"]][["model"]], terms = c("discrim.dist.abs [all]", "correct"))

# plot syntax
figure.b3 <- plot(figure.b3.data, line_size = 2, show_ci = F) + 
  labs(x = expression(paste("Discrimination-Adjusted Ability-Difficulty Distance a|θ − ", italic("b"), "|", sep = "")), 
       y = "Reponse Time (in seconds)", title = NULL, color = "Response") + 
  scale_x_continuous(breaks = seq(0, 9.5, by = 2), limits = c(0, 9.5), labels = signs::signs_format()) +
  scale_y_continuous(breaks = seq(0, 95, by = 10), limits = c(0, 95)) + 
  scale_color_manual(values = c("#DB060B", "#4DB264"), labels = c("Incorrect", "Correct")) + 
  theme_bw(base_size = 16) + 
  theme(axis.text = element_text(color = "black"),
        legend.position = "bottom") 
#  +
#  geom_vline(xintercept = 3.8, 
#                  color = "blue", linewidth=1)


# visualize the plot
figure.b3
