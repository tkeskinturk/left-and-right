
# - Some Stuff About Left/Right Position ---------------------------------------------------------- #
# ------------------------------------------------------------------------------------------------- #

# - part 1: prep ---------------------------------------------------------------------------------- # 

# loading the packages
pacman::p_load(tidyverse, ggrepel, hrbrthemes)

# load the data and clean-up the relevant variables
d <- readRDS('evs-wvs_joint.rds') |>
  # select the variables of interest
  select(
    cntry,
    ## political ideology
    ideo = e033,
    ## beliefs about economics and economic policy
    ineq = e035,
    priv = e036,
    govt = e037,
    comp = e039,
    ## beliefs about cultural politics
    homo = d081,
    gend = d059,
    abor = f120,
    immg = c002_01
  ) |> 
  # clean-up the variables
  mutate(across(everything(), ~ ifelse(. < 0, NA_real_, .)))

# construct correlations for each country
c <- d |>
  group_by(cntry) |>
  summarize(
    econ1 = abs(cor(ideo, ineq, use = "pairwise.complete.obs")),
    econ2 = abs(cor(ideo, priv, use = "pairwise.complete.obs")),
    econ3 = abs(cor(ideo, govt, use = "pairwise.complete.obs")),
    econ4 = abs(cor(ideo, comp, use = "pairwise.complete.obs")),
    cult1 = abs(cor(ideo, homo, use = "pairwise.complete.obs")),
    cult2 = abs(cor(ideo, gend, use = "pairwise.complete.obs")),
    cult3 = abs(cor(ideo, abor, use = "pairwise.complete.obs")),
    cult4 = abs(cor(ideo, immg, use = "pairwise.complete.obs"))
  ) |> mutate(
    econcor = (econ1 + econ2 + econ3 + econ4) / 4,
    cultcor = (cult1 + cult2 + cult3 + cult4) / 4
  ) |>
  select(cntry, econcor, cultcor) |>
  drop_na() |>
  as_tibble()

# labels for countries
l <- read.csv("evs-wvs_nationlabels.csv")
c <- left_join(c, l, by = "cntry")

# - part 2: visuals ------------------------------------------------------------------------------- # 

# how culture and econ work together?
png('left-right1.png', w = 12.5, h = 7.5, units = "in", res = 300)
c |>
  ggplot(aes(x = econcor, y = cultcor)) +
  geom_point(size = 1.5, col = "#720500") +
  geom_text_repel(aes(label = label), size = 2.25, max.overlaps = 12.5) +
  theme_ipsum_rc() +
  labs(
    title = "Coherence of Left/Right Position with Economic & Cultural Beliefs",
    subtitle = "Mean Absolute Correlations Between Left/Right Position and Economic and Cultural Beliefs Across Countries",
    x = "Economic Coherence",
    y = "Cultural Coherence",
    caption = "World Values Survey & European Values Study, Joint Survey. 2017-2022."
  )
dev.off()

# economic beliefs
png('left-right2.png', w = 10, h = 12.5, units = "in", res = 1000)
ggplot(c, aes(x = reorder(label, econcor), y = econcor)) +
  geom_point(size = 1) +
  coord_flip() +
  theme_ipsum_rc(grid = "Y") +
  labs(
    title = "Correlation of Left/Right Position with Economic Beliefs",
    subtitle = "Left/Right Position with Inequality, Private Ownership, Government Responsibilities, and Competition",
    x = "",
    y = "Country-Level Mean Absolute Correlation",
    caption = "World Values Survey & European Values Study, Joint Survey. 2017-2022.")
dev.off()

# cultural beliefs
png('left-right3.png', w = 10, h = 12.5, units = "in", res = 1000)
ggplot(c, aes(x = reorder(label, cultcor), y = cultcor)) +
  geom_point(size = 1) +
  coord_flip() +
  theme_ipsum_rc(grid = "Y") +
  labs(
    title = "Correlation of Left/Right Position with Cultural Beliefs",
    subtitle = "Left/Right Position with Homosexuality, Gender Attitudes, Abortion and Immigration",
    x = "",
    y = "Country-Level Mean Absolute Correlation",
    caption = "World Values Survey & European Values Study, Joint Survey. 2017-2022.")
dev.off()

# ------------------------------------------------------------------------------------------------- # 
