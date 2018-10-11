## Robustness: officeholder

# 1805 winners and losers

# + covariates

oh.lm.covars <- lm(sub.oh.05$oh~sub.oh.05$treat + 
                         sub.oh.05$n.draw +
                         sub.oh.05$Burke +
                         sub.oh.05$Jefferson)
oh.CI.covars <- list("CI" = confint(oh.lm.covars, "sub.oh.05$treat")[1:2],
                         "ATE" = oh.lm.covars$coefficients['sub.oh.05$treat'][[1]])

print(oh.CI.covars)

# oh match

match.oh.lm.05 <- lm(sub.oh.05$match.oh~sub.oh.05$treat + sub.oh.05$n.draw)
match.oh.CI.05 <- list("CI" = confint(match.oh.lm.05, "sub.oh.05$treat")[1:2],
                           "ATE" = match.oh.lm.05$coefficients['sub.oh.05$treat'][[1]])

print(match.oh.CI.05)

# oh match + covariates

match.oh.lm.covars <- lm(sub.oh.05$match.oh~sub.oh.05$treat +
                               sub.oh.05$n.draw +
                               sub.oh.05$Burke +
                               sub.oh.05$Jefferson)
match.oh.CI.covars <- list("CI" = confint(match.oh.lm.covars, "sub.oh.05$treat")[1:2],
                               "ATE" = match.oh.lm.covars$coefficients['sub.oh.05$treat'][[1]])

print(match.oh.CI.covars)

# 1805 winners

# + covariates

oh.lm.winners.05.covars <- lm(sub.oh.05.winners$oh~sub.oh.05.winners$treat + 
                                    sub.oh.05.winners$n.draw +
                                    sub.oh.05.winners$Burke +
                                    sub.oh.05.winners$Clarke +
                                    sub.oh.05.winners$Lincoln +
                                    sub.oh.05.winners$Wilkes)
oh.CI.winners.05.covars <- list("CI" = confint(oh.lm.winners.05.covars, "sub.oh.05.winners$treat")[1:2],
                                    "ATE" = oh.lm.winners.05.covars$coefficients['sub.oh.05.winners$treat'][[1]])

print(oh.CI.winners.05.covars)

# oh match

match.oh.lm.winners.05 <- lm(sub.oh.05.winners$match.oh~sub.oh.05.winners$treat + sub.oh.05.winners$n.draw)
match.oh.CI.winners.05 <- list("CI" = confint(match.oh.lm.winners.05, "sub.oh.05.winners$treat")[1:2],
                                   "ATE" = match.oh.lm.winners.05$coefficients['sub.oh.05.winners$treat'][[1]])

print(match.oh.CI.winners.05)

# oh match + covariates

match.oh.lm.winners.05.covars <- lm(sub.oh.05.winners$match.oh~sub.oh.05.winners$treat +
                                          sub.oh.05.winners$n.draw +
                                          sub.oh.05.winners$Clarke +
                                          sub.oh.05.winners$Lincoln +
                                          sub.oh.05.winners$Wilkes)
match.oh.CI.winners.05.covars <- list("CI" = confint(match.oh.lm.winners.05.covars, "sub.oh.05.winners$treat")[1:2],
                                          "ATE" = match.oh.lm.winners.05.covars$coefficients['sub.oh.05.winners$treat'][[1]])

print(match.oh.CI.winners.05.covars)

# 1807 winners

# + covariates

oh.lm.winners.07.covars <- lm(sub.oh.07.winners$oh~sub.oh.07.winners$treat + 
                                    sub.oh.07.winners$n.draw +
                                    sub.oh.07.winners$match.census.07 +
                                    sub.oh.07.winners$prior.run +
                                    sub.oh.07.winners$junior +
                                    sub.oh.07.winners$senior +
                                    sub.oh.07.winners$surname.freq +
                                    sub.oh.07.winners$Bryan +
                                    sub.oh.07.winners$Chatham +
                                    sub.oh.07.winners$Clarke +
                                    sub.oh.07.winners$Lincoln)
oh.CI.winners.07.covars <- list("CI" = confint(oh.lm.winners.07.covars, "sub.oh.07.winners$treat")[1:2],
                                    "ATE" = oh.lm.winners.07.covars$coefficients['sub.oh.07.winners$treat'][[1]])

print(oh.CI.winners.07.covars)

# oh match

match.oh.lm.winners.07 <- lm(sub.oh.07.winners$match.oh~sub.oh.07.winners$treat + sub.oh.07.winners$n.draw)
match.oh.CI.winners.07 <- list("CI" = confint(match.oh.lm.winners.07, "sub.oh.07.winners$treat")[1:2],
                                   "ATE" = match.oh.lm.winners.07$coefficients['sub.oh.07.winners$treat'][[1]])

print(match.oh.CI.winners.07)

# oh match + covariates

match.oh.lm.winners.07.covars <- lm(sub.oh.07.winners$match.oh~sub.oh.07.winners$treat +
                                      sub.oh.07.winners$n.draw +
                                      sub.oh.07.winners$match.census.07 +
                                      sub.oh.07.winners$prior.run +
                                      sub.oh.07.winners$junior +
                                      sub.oh.07.winners$senior +
                                      sub.oh.07.winners$surname.freq +
                                      sub.oh.07.winners$Bryan +
                                      sub.oh.07.winners$Chatham +
                                      sub.oh.07.winners$Clarke +
                                      sub.oh.07.winners$Lincoln)
match.oh.CI.winners.07.covars <- list("CI" = confint(match.oh.lm.winners.07.covars, "sub.oh.07.winners$treat")[1:2],
                                          "ATE" = match.oh.lm.winners.07.covars$coefficients['sub.oh.07.winners$treat'][[1]])

print(match.oh.CI.winners.07.covars)
