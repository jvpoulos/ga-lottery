## Robustness: slave wealth

# 1805 winners and losers

# + covariates

slaves.lm.covars <- lm(sub.1820.05$slave.wealth.1820~sub.1820.05$treat + 
                         sub.1820.05$n.draw +
                         sub.1820.05$Burke +
                         sub.1820.05$Jefferson)
slaves.CI.covars <- list("CI" = confint(slaves.lm.covars, "sub.1820.05$treat")[1:2],
                         "ATE" = slaves.lm.covars$coefficients['sub.1820.05$treat'][[1]])

print(slaves.CI.covars)

# slave wealth match

match.slaves.lm.05 <- lm(sub.1820.05$slave.wealth.1820.w~sub.1820.05$treat + sub.1820.05$n.draw)
match.slaves.CI.05 <- list("CI" = confint(match.slaves.lm.05, "sub.1820.05$treat")[1:2],
                           "ATE" = match.slaves.lm.05$coefficients['sub.1820.05$treat'][[1]])

print(match.slaves.CI.05)

# slave wealth match + covariates

match.slaves.lm.covars <- lm(sub.1820.05$slave.wealth.1820.w~sub.1820.05$treat +
                               sub.1820.05$n.draw +
                               sub.1820.05$Burke +
                               sub.1820.05$Jefferson)
match.slaves.CI.covars <- list("CI" = confint(match.slaves.lm.covars, "sub.1820.05$treat")[1:2],
                               "ATE" = match.slaves.lm.covars$coefficients['sub.1820.05$treat'][[1]])

print(match.slaves.CI.covars)

# 1805 winners

# + covariates

slaves.lm.winners.05.covars <- lm(sub.1820.05.winners$slave.wealth.1820~sub.1820.05.winners$treat + 
                                    sub.1820.05.winners$n.draw +
                                    sub.1820.05.winners$Burke +
                                    sub.1820.05.winners$Jefferson)
slaves.CI.winners.05.covars <- list("CI" = confint(slaves.lm.winners.05.covars, "sub.1820.05.winners$treat")[1:2],
                                    "ATE" = slaves.lm.winners.05.covars$coefficients['sub.1820.05.winners$treat'][[1]])

print(slaves.CI.winners.05.covars)

# slave wealth match

match.slaves.lm.winners.05 <- lm(sub.1820.05.winners$slave.wealth.1820.w~sub.1820.05.winners$treat + sub.1820.05.winners$n.draw)
match.slaves.CI.winners.05 <- list("CI" = confint(match.slaves.lm.winners.05, "sub.1820.05.winners$treat")[1:2],
                                   "ATE" = match.slaves.lm.winners.05$coefficients['sub.1820.05.winners$treat'][[1]])

print(match.slaves.CI.winners.05)

# slave wealth match + covariates

match.slaves.lm.winners.05.covars <- lm(sub.1820.05.winners$slave.wealth.1820.w~sub.1820.05.winners$treat +
                                          sub.1820.05.winners$n.draw +
                                          sub.1820.05.winners$Burke +
                                          sub.1820.05.winners$Jefferson)
match.slaves.CI.winners.05.covars <- list("CI" = confint(match.slaves.lm.winners.05.covars, "sub.1820.05.winners$treat")[1:2],
                                          "ATE" = match.slaves.lm.winners.05.covars$coefficients['sub.1820.05.winners$treat'][[1]])

print(match.slaves.CI.winners.05.covars)

# 1807 winners

# + covariates

slaves.lm.winners.07.covars <- lm(sub.1820.07.winners$slave.wealth.1820~sub.1820.07.winners$treat + 
                                    sub.1820.07.winners$n.draw +
                                    sub.1820.07.winners$match.census.07 +
                                    sub.1820.07.winners$prior.run +
                                    sub.1820.07.winners$junior +
                                    sub.1820.07.winners$senior +
                                    sub.1820.07.winners$surname.freq +
                                    sub.1820.07.winners$Bryan +
                                    sub.1820.07.winners$Chatham +
                                    sub.1820.07.winners$Clarke +
                                    sub.1820.07.winners$Lincoln)
slaves.CI.winners.07.covars <- list("CI" = confint(slaves.lm.winners.07.covars, "sub.1820.07.winners$treat")[1:2],
                                    "ATE" = slaves.lm.winners.07.covars$coefficients['sub.1820.07.winners$treat'][[1]])

print(slaves.CI.winners.07.covars)

# slave wealth match

match.slaves.lm.winners.07 <- lm(sub.1820.07.winners$slave.wealth.1820.w~sub.1820.07.winners$treat + sub.1820.07.winners$n.draw)
match.slaves.CI.winners.07 <- list("CI" = confint(match.slaves.lm.winners.07, "sub.1820.07.winners$treat")[1:2],
                                   "ATE" = match.slaves.lm.winners.07$coefficients['sub.1820.07.winners$treat'][[1]])

print(match.slaves.CI.winners.07)

# slave wealth match + covariates

match.slaves.lm.winners.07.covars <- lm(sub.1820.07.winners$slave.wealth.1820.w~sub.1820.07.winners$treat +
                                          sub.1820.07.winners$n.draw +
                                          sub.1820.07.winners$match.census.07 +
                                          sub.1820.07.winners$prior.run +
                                          sub.1820.07.winners$junior +
                                          sub.1820.07.winners$senior +
                                          sub.1820.07.winners$surname.freq +
                                          sub.1820.07.winners$Bryan +
                                          sub.1820.07.winners$Chatham +
                                          sub.1820.07.winners$Clarke +
                                          sub.1820.07.winners$Lincoln)
match.slaves.CI.winners.07.covars <- list("CI" = confint(match.slaves.lm.winners.07.covars, "sub.1820.07.winners$treat")[1:2],
                                          "ATE" = match.slaves.lm.winners.07.covars$coefficients['sub.1820.07.winners$treat'][[1]])

print(match.slaves.CI.winners.07.covars)