# Simulation Study - generating random data for three groups

library("ggplot2")
library("dplyr")
library("reshape2")

options(scipen = 999)
set.seed(1)

grp1 <- data_frame(grp = "A", 
                         Length = rnorm(150, mean = 5.6, sd = 2.1))
grp2 <- data_frame(grp = "B", 
                         Length = rnorm(150, mean = 13.2, sd = 3.2))
grp3 <- data_frame(grp = "C", 
                         Length = rnorm(150, mean = 20.9, sd = 2.8))

# Expect that the mixture weights are equal here - at 1/3 for each group

grpdata <- bind_rows(grp1, grp2, grp3)

grpdata %>%
  group_by(grp) %>%
  summarize(mean_vals = mean(Length),
            sd_vals = sd(Length))

grpdata.df <- grpdata$Length


# K - means will show you initial assignments ## insert chart into report to show how initialisation step works

sim.kmeans <- kmeans(grpdata.df, 3)
sim.cluster <- sim.kmeans$cluster
sim.df <- data.frame(x = grpdata.df, cluster = sim.cluster)

sim.df %>%
  mutate(num = row_number()) %>%
  ggplot(aes(y = num, x = x, color = factor(cluster))) +
  geom_point() +
  ylab("Values") +
  ylab("Data Point Number") +
  scale_color_discrete(name = "Cluster") +
  ggtitle("K-means clustering: Initialisation Step")

sim.summary <- sim.df %>%
  group_by(cluster) %>%
  summarize(mu = mean(x), sigma = sd(x), size = n())

sim.summary %>%
  select(cluster, mu, sigma)

sim.summary.df <- sim.summary %>%
  mutate(lambda = size / sum(size))

sim.summary.df %>%
  select(cluster, lambda)

params <- data.frame(select(sim.summary.df, -c(cluster, size))); params

# Running the Team EM function on these data:

sim <- teamEM(grpdata)

sim.mean <- sim$estimates["mu"]
sim.sd <- sim$estimates["sigma"]
sim.lambda <- sim$estimates["lambda"]


plot_mix_comps <- function(x, mu, sigma, lam) {
  lam * dnorm(x, mu, sigma)
}

data.frame(x = grpdata$Length) %>%
  ggplot() +
  geom_histogram(aes(x, ..density..), binwidth = 1, colour = "seashell4", 
                 fill = "gray98") +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(sim.mean$mu[1], sim.sd$sigma[1], 
                            lam = sim.lambda$lambda[1]),
                colour = "cyan4", lwd = 1) +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(sim.mean$mu[2], sim.sd$sigma[2], 
                lam = sim.lambda$lambda[2]), 
                colour = "hotpink3", lwd = 1) +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(sim.mean$mu[3], sim.sd$sigma[3], 
                lam = sim.lambda$lambda[3]), 
colour = "plum4", lwd = 1) +
  ylab("Density") +
  xlab("Values") +
  ggtitle("Gaussian Mixture Model for Simulated Data")


result <- data.frame(params,"mu_EM" = sim$estimates["mu"], "sd_EM" = sim$estimates["sigma"], "lambda_EM" = sim$estimates["lambda"])

mean_percent_diff <- apply(result[,c('mu', 'mu.1')], 1, function(x) { (x[1]-x[2])/x[2] * 100 } )
sd_percent_diff <- apply(result[,c('sigma', 'sigma.1')], 1, function(x) { (x[1]-x[2])/x[2] * 100 } )
lambda_percent_diff <- apply(result[,c('lambda', 'lambda.1')], 1, function(x) { (x[1]-x[2])/x[2] * 100 } )

percentage_diff <- data.frame(mean_percent_diff, sd_percent_diff, lambda_percent_diff)


