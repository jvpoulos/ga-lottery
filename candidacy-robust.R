## Robustness: candidate

# 1805 winners and losers

# + covariates

candidate.lm.covars <- lm(sub.oh.05$candidate~sub.oh.05$treat + 
                         sub.oh.05$n.draw +
                         sub.oh.05$Burke +
                         sub.oh.05$Jefferson)
candidate.CI.covars <- list("CI" = confint(candidate.lm.covars, "sub.oh.05$treat")[1:2],
                         "ATE" = candidate.lm.covars$coefficients['sub.oh.05$treat'][[1]])

print(candidate.CI.covars)

# candidate match

match.candidate.lm.05 <- lm(sub.oh.05$match.votes.05~sub.oh.05$treat + sub.oh.05$n.draw)
match.candidate.CI.05 <- list("CI" = confint(match.candidate.lm.05, "sub.oh.05$treat")[1:2],
                           "ATE" = match.candidate.lm.05$coefficients['sub.oh.05$treat'][[1]])

print(match.candidate.CI.05)

# candidate match + covariates

match.candidate.lm.covars <- lm(sub.oh.05$match.votes.05~sub.oh.05$treat +
                               sub.oh.05$n.draw +
                               sub.oh.05$Burke +
                               sub.oh.05$Jefferson)
match.candidate.CI.covars <- list("CI" = confint(match.candidate.lm.covars, "sub.oh.05$treat")[1:2],
                               "ATE" = match.candidate.lm.covars$coefficients['sub.oh.05$treat'][[1]])

print(match.candidate.CI.covars)

# 1805 winners

# + covariates

candidate.lm.winners.05.covars <- lm(sub.oh.05.winners$candidate~sub.oh.05.winners$treat + 
                                    sub.oh.05.winners$n.draw +
                                    sub.oh.05.winners$Burke +
                                    sub.oh.05.winners$Clarke +
                                    sub.oh.05.winners$Lincoln +
                                    sub.oh.05.winners$Wilkes)
candidate.CI.winners.05.covars <- list("CI" = confint(candidate.lm.winners.05.covars, "sub.oh.05.winners$treat")[1:2],
                                    "ATE" = candidate.lm.winners.05.covars$coefficients['sub.oh.05.winners$treat'][[1]])

print(candidate.CI.winners.05.covars)

# candidate match

match.candidate.lm.winners.05 <- lm(sub.oh.05.winners$match.votes.05~sub.oh.05.winners$treat + sub.oh.05.winners$n.draw)
match.candidate.CI.winners.05 <- list("CI" = confint(match.candidate.lm.winners.05, "sub.oh.05.winners$treat")[1:2],
                                   "ATE" = match.candidate.lm.winners.05$coefficients['sub.oh.05.winners$treat'][[1]])

print(match.candidate.CI.winners.05)

# candidate match + covariates

match.candidate.lm.winners.05.covars <- lm(sub.oh.05.winners$match.votes.05~sub.oh.05.winners$treat +
                                          sub.oh.05.winners$n.draw +
                                          sub.oh.05.winners$Clarke +
                                          sub.oh.05.winners$Lincoln +
                                          sub.oh.05.winners$Wilkes)
match.candidate.CI.winners.05.covars <- list("CI" = confint(match.candidate.lm.winners.05.covars, "sub.oh.05.winners$treat")[1:2],
                                          "ATE" = match.candidate.lm.winners.05.covars$coefficients['sub.oh.05.winners$treat'][[1]])

print(match.candidate.CI.winners.05.covars)

# 1807 winners

# + covariates

candidate.lm.winners.07.covars <- lm(sub.oh.07.winners$candidate~sub.oh.07.winners$treat + 
                                    sub.oh.07.winners$n.draw +
                                    sub.oh.07.winners$prior.run +
                                    sub.oh.07.winners$prior.office +
                                    sub.oh.07.winners$junior +
                                    sub.oh.07.winners$senior +
                                    sub.oh.07.winners$surname.freq +
                                    sub.oh.07.winners$Bryan +
                                    sub.oh.07.winners$Burke +
                                    sub.oh.07.winners$Chatham +
                                    sub.oh.07.winners$Franklin +
                                    sub.oh.07.winners$Lincoln +
                                    sub.oh.07.winners$Richmond)
candidate.CI.winners.07.covars <- list("CI" = confint(candidate.lm.winners.07.covars, "sub.oh.07.winners$treat")[1:2],
                                    "ATE" = candidate.lm.winners.07.covars$coefficients['sub.oh.07.winners$treat'][[1]])

print(candidate.CI.winners.07.covars)

# candidate match

match.candidate.lm.winners.07 <- lm(sub.oh.07.winners$match.votes.07~sub.oh.07.winners$treat + sub.oh.07.winners$n.draw)
match.candidate.CI.winners.07 <- list("CI" = confint(match.candidate.lm.winners.07, "sub.oh.07.winners$treat")[1:2],
                                   "ATE" = match.candidate.lm.winners.07$coefficients['sub.oh.07.winners$treat'][[1]])

print(match.candidate.CI.winners.07)

# candidate match + covariates

match.candidate.lm.winners.07.covars <- lm(sub.oh.07.winners$match.votes.07~sub.oh.07.winners$treat +
                                          sub.oh.07.winners$n.draw +
                                          sub.oh.07.winners$prior.run +
                                          sub.oh.07.winners$prior.office +
                                          sub.oh.07.winners$junior +
                                          sub.oh.07.winners$senior +
                                          sub.oh.07.winners$surname.freq +
                                          sub.oh.07.winners$Bryan +
                                          sub.oh.07.winners$Burke +
                                          sub.oh.07.winners$Chatham +
                                          sub.oh.07.winners$Franklin +
                                          sub.oh.07.winners$Lincoln +
                                          sub.oh.07.winners$Richmond)
match.candidate.CI.winners.07.covars <- list("CI" = confint(match.candidate.lm.winners.07.covars, "sub.oh.07.winners$treat")[1:2],
                                          "ATE" = match.candidate.lm.winners.07.covars$coefficients['sub.oh.07.winners$treat'][[1]])

print(match.candidate.CI.winners.07.covars)
