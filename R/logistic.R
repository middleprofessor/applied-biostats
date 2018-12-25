# scripts for logistic

# note that the odds, p/(1-p), is the ratio of the probability of the event vs the probabilty of not the event
expit <- function(x) {exp(x)/(1+exp(x))} # the inverse logit function. This generates the probability of the event p

invlogit <- function(x){1/(1+exp(-x))} # same as expit

logit <- function(p) {log(p/(1-p))} # the log of the odds or "logodds" given the probability of an event p. This is NOT the odds ratio, which is the ratio of two odds.

p2odd <- function(p) {p/(1-p)} # the odds of the probability of an event

odd2p <- function(x) {x/(1+x)} # the probability associated with an odds
