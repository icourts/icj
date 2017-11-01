library(icj)

ROOT <- 'C:/Users/dcg601/icj'
data0017 <- get_decision_data(years = c(2000, 2017))

save(data0017, file = file.path(ROOT, 'data0017.dat'))

data0017all <- get_decisions( years = c(2000, 2017), mode = 'all', dir = ROOT)
save(data0017all, file = file.path(ROOT, 'data0017all.dat'))
