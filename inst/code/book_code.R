# Preparing tools ----

radius <- c(1, 2, 3, 4, 5) # assign values
area <- pi * x ^ 2 # pi is a built-in constant
plot(x = radius, y = area) # draw a figure

install.packages('remotes')
install.packages(c('Epi', 'data.table', 'tidyverse'))

library(remotes)
install_github('pzhaonet/rosr')
remotes::install_github('pzhaonet/rosr')

citation('pubh')
citation('Epi')
toBibtex(citation('Epi'))
print(citation('Epi'), bibtex = TRUE)
epi_downloads <- cranlogs::cran_downloads('Epi')
epi_downloads

library(ggplot2)
library(dplyr)
cranlogs_cum <- function(x) {
  cranlogs::cran_downloads(
    x,
    from = Sys.Date() - 365,
    to = Sys.Date()) |>
    transform(CumDownload = cumsum(count))
}
c('Epi', 'EpiModel', 'epibasix', 'epiDisplay', 'epiR',
  'epitools', 'nCov2019', 'pubh', 'survival', 'survey',
  'surveillance', 'SPARSEMODr', 'WHO') |>
  lapply(cranlogs_cum) |>
  bind_rows() |>
  ggplot() +
  geom_line(aes(date, CumDownload)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  facet_wrap(~package, scales = 'free_y')

data()
data(package = 'Epi')
data(diet, package = 'Epi')
help(diet, package = "Epi")
head(diet) # print the first 6 rows
ls_data <- data(package = c('Epi', 'pubh', 'NHANES', 'survey', 'boot'))
View(ls_data$results)
ls_data$results[, 3]

help(package = "Epi")
help("install_github", package = "remotes")
help(DMepi, package = 'Epi')

library(remotes)
help("install_github")
?install_github

library(Epi)
help(DMepi)
?DMepi
help.search("analysis of variance")
??"analysis of variance"

example(stat.table, package = 'Epi')
demo()
demo(package = c('epiDisplay', 'surveillance'))
demo(biosurvbook, package = 'surveillance')
demo(package = .packages(all.available = TRUE))

sessionInfo()
devtools::session_info()

# 1. Create the index for the subjects:
index <- 1:nrow(diet)
# 2. Create a new variable 'fibre-fat':
diet$fibre_fat <- paste0(diet$fibre, '/', diet$fat)
# 3. Find which is the maximum dietary fibre in take in each group:
tb <- tapply(diet$fibre, diet$job, which.max)
# 4. Find the row number of the maximum dietary fibre in take in each group:
rowi <- sapply(names(tb),
               function(x) index[diet$job == x][tb[names(tb) == x]])
# 5. Display the result:
diet[rowi, c('job', 'fibre_fat')]


# 1. Create a new variable 'fibre-fat':
diet$fibre_fat <- paste0(diet$fibre, '/', diet$fat)
# 2. Define a function for finding the maximum fat in 'job-fat'
find_max <-  function(x)
  x[which.max(data.frame(strsplit(x, '/'))[1, ])]
# 3. Apply the function 'find_max' to 'job-fat' grouped by 'job'
tb <- tapply(diet$fibre_fat, diet$job, find_max)
# 4 Display the result
data.frame(job = names(tb), fibre_fat = tb, row.names = NULL)

# 1. Load the package:
library(data.table)
# 2. Converts diet from a data.frame into a data.table:
setDT(diet)
# 3. Do the task and display the result:
diet[,.SD[fibre == max(fibre, na.rm = TRUE),
          .(fibre_fat = paste0(fibre, "/", fat))],
     by = .(job)]

# 1. Load the packages:
library(tidyverse)
# 2. Specify the data frame:
diet %>%
# 3. Group the dataset with job:
  group_by(job) %>%
# 4. Pick out the target rows:
  filter(fibre == max(fibre, na.rm = TRUE)) %>%
# 5. Create the target columns:
  transmute(job = job,
            fibre_fat = paste0(fibre, '/', fat))


tidyverse::tidyverse_packages()

# Planning data ----

bibliometrix::biblioshiny()

library(scholar)
gsid <- get_scholar_id(first_name = "marius", last_name = "wamsiedel")
gsid

# functions for a person
gs_person <- function(id){
  list(citation = get_citation_history(id),
       coauthor = get_coauthors(id),
       n_article = get_num_articles(id),
       n_journal = get_num_distinct_journals(id),
       n_top = get_num_top_journals(id),
       oldest = get_oldest_article(id),
       profile = get_profile(id),
       pub = get_publications(id))
}

# functions for a journal
gs_journal <- function(journal){
  list(IF = get_impactfactor(journal),
       rank = get_journalrank(journal))
}

# functions for a publication
gs_pub <- function(id, pubid){
  list(cite = get_article_cite_history(id, pubid),
       authors = get_complete_authors(id, pubid))
}

ls_person <- gs_person(gsid)
names(ls_person$pub)
ls_journal <- gs_journal(ls_person$pub$journal[1])
ls_pub <- gs_pub(gsid, ls_person$pub$pubid[1])

library(ggplot2)
library(dplyr)
library(patchwork)
p_citation <-
  ls_person$citation |>
  ggplot(aes(year, cites)) + geom_bar(stat = 'identity')
p_pub <-
  ls_pub$cite |>
  ggplot(aes(year, cites)) +
  geom_segment(aes(xend = year, yend = 0), size=1, color='darkgrey') +
  geom_point(size=3, color='firebrick')
p_journal <- ls_person$pub |>
  filter(journal != '') |>
  group_by(journal) |>
  summarise(n = length(journal)) |>
  arrange(desc(n)) |>
  mutate(journal = reorder(journal, n)) |>
  head() |>
  ggplot() +
  geom_bar(aes(x = journal, y = n), stat = 'identity') +
  coord_flip()
p_colab <-
  plot_coauthors(ls_person$coauthor, size_labels = 3) +
  labs(title = '')
(p_citation | p_journal) / (p_pub | p_colab) + plot_annotation(tag_levels = 'A')

library(mindr)
demo_txt <- c(
  '# Data',
  '## Creating',
  '## Importing',
  '### Files',
  '### Via applications',
  '## Saving/exporting',
  '## R script files',
  '# Manupulation',
  '## Columns (variables)',
  '## Rows (cases/subjects)',
  '# R Environment',
  '## Non-CRAN packages',
  '## Workspaces',
  '## Ojbects',
  '## History',
  '# Code development',
  '## RStudio',
  '## Notepad++'
)
mindmap_demo <- mm(from = demo_txt,
                   output_type = c('widget', 'mindmap'),
                   root = 'Processing data with R')
mindmap_demo$widget
htmlwidgets::saveWidget(mindmap_demo$widget, file = 'mindmap_demo.html')
writeLines(mindmap_demo$mindmap, 'mindmap_demo.mm')

x <- tibble::tribble(
  ~Start,        ~End,         ~Activity,        ~Status,
  "2021-01-01", "2021-03-31", "Research design", 'done',
  "2021-04-01", "2021-09-30", "Lab preparation", 'done',
  "2021-10-01", "2021-10-31", "Outdoor test",    'active',
  "2021-11-01", "2023-01-31", "Collect data",    'crit, active',
  "2021-05-01", "2022-04-30", "R package dev.",  'active',
  "2023-02-01", "2023-08-31", "Analyze data",    '',
  "2023-06-01", "2024-01-01", "Manuscript",      ''
  )
library(DiagrammeR)
create_gannt_txt <- function(x){
         paste(
           paste0(x$Activity, ':',
                  x$Status, ifelse(x$Status == '', '', ', '),
                  1:nrow(x), ', ', x$Start, ',', x$End),
           collapse = '\n')
}
gannt_txt <- paste('gantt',
                    'dateFormat  YYYY-MM-DD',
                    'section Basic Tasks',
                    create_gannt_txt(x[1:4,]),
                    'section Products',
                    create_gannt_txt(x[5:7,]),
                    sep = '\n')
mermaid(gannt_txt)

library(ggplot2)
library(tidyr)
plot_gannt <- function(x){
  acts <- x$Activity
  gannt <- gather(x, "state", "date", 1:2) %>%
    transform(
      Activity=factor(Activity, acts[length(acts):1]),
      date = as.Date(date)) %>%
    ggplot(aes(date, Activity, color = Activity)) +
    geom_line(size = 10) +
    scale_x_date(
      breaks = seq.Date(
        as.Date(x$Start[1]),
        as.Date(x$End[nrow(x)]), "quarter"),
      minor_breaks = seq.Date(
        as.Date(x$Start[1]),
        as.Date(x$End[nrow(x)]), "month"),
      labels=c(1, "", "", "", 2, "", "", "", 3, "", "", "", "")) +
    theme(legend.position = "", axis.ticks = element_blank())+
    labs(x = "Project Year", y = "")
  gannt
}
plot_gannt(x)

# Collecting data ----

N <- 12
id <- 1:N
gender <-  factor(sample(rep(c(1, 2), c(4, 8)), 12))
x <- data.frame(id = id,
                yi = 1,
                gd = gender)
library(ggplot2)
p <- ggplot(x) +
 geom_point(aes(id, yi, colour = gd, shape = gd), size = 6) +
 labs(x = '', y = '') +
 scale_shape_manual(values = c(1, 16)) +
 lims(y = c(0.5, 2.5)) +
 theme_void() +
 theme(panel.border = element_rect(fill = NA), legend.position = "none")
p

ggsample <- function(x, i) {
  # x is the input data frame
  # i is the index of the chosen individuals
  geom_segment(aes(x = id, y = 2, xend = id, yend = 1.2),
               data = x[i, ], size = 2, arrow = arrow())
}

n <- 3
i_sim <- sample(1:N, n)
p + ggsample(x, i_sim)

i <- N %/% n
r <- sample(1:i, 1)
i_sys <- seq(r, r + i * (n - 1), i)
systematic_sampling <- function(N, n) {
  i <- N %/% n
  r <- sample(1:i, 1)
  seq(r, r + i * (n - 1), i)
}
i_sys <- systematic_sampling(N, n)
p + ggsample(x, i_sim)

library(dplyr)
x_str <- x |>
  group_by(gd) |>
  sample_frac(size = 0.3)
i_str <- x_str$id
p + ggsample(x, i_str)

x_str <- x |>
  group_by(gd) |>
  sample_n(size = 2)
i_str <- x_str$id

n_clu <- 1
sample_clu <- sample(1: (N / n), n_clu)
i_clu <- (n * (sample_clu - 1) + 1) : (n * sample_clu)

cluster_sampling <- function(N, n, n_clu) {
  sample_clu <- sample(1: (N / n), n_clu)
  c(sapply(sample_clu, function(sample_clu) (n * (sample_clu - 1) + 1) : (n * sample_clu)))
}
i_clu <- cluster_sampling(N, n, n_clu)
p + ggsample(x, i_clu)

# Importing and exporting data -----

dtf_eating <- data.frame(
  Outcome = c('Eating disorder', 'No eating disorder'),
  Girls = c(100, 14900),
  Boys = c(25, 9975))

dtf_eating <- tibble::tribble(
  ~Outcome,             ~Girls, ~Boys,
  'Eating discorder',    100,      25,
  'No eating discorder', 14900,  9975
)


sample1 <- c(162, 173, 181)
sample2 <- c(155, 168, 172, 180)

ls_sample <- list(sample1 = c(162, 173, 181),
                  sample2 = c(155, 168, 172, 180))
ls_sample

dtf_sample <- data.frame(group = rep(c('sample1', 'sample2'), c(3, 4)),
                         value = c(162, 173, 181, 155, 168, 172, 180))
dtf_sample

dtf_csv <- read.csv('data/sm-demo.csv', skip = 2, header = FALSE)
# or
dtf_csv <- read.table('data/sm-demo.csv', skip = 2, header = FALSE, sep = ',')

write.csv(dtf_csv, 'data/sm-demo-new.csv', row.names = FALSE)
# or
write.table(dtf_csv, 'data/sm-demo-new.csv', row.names = FALSE, sep = ',')

bf <- readRDS('data/bf.rds')
saveRDS(bf, 'data/bf2.rds')
ls()
save(bf, ls_sample, sample1, file = 'data/save.rda')
load('data/save.rda')
sample1 <- 1
load('data/save.rda')
save.image('save.rda')

dtf_xlsx <- xlsx::read.xlsx("data/sm-demo.xlsx", sheetIndex = 1, header = FALSE, startRow = 3)
# or
dtf_xlsx <- readxl::read_xlsx("data/sm-demo.xlsx", col_names = FALSE, skip = 2)

xlsx::write.xlsx(dtf_xlsx, 'data/sm-demo2.xlsx')

dtf_spss <- foreign::read.spss('data/sm-demo.sav', to.data.frame = TRUE)

library(RODBC)
mymdb <- odbcConnectAccess2007("data/pptless.mdb")
sqlTables(mymdb)
dtf_mdb <- sqlFetch(mymdb, "pgcert1")

remotes::install_github("expersso/WHO")
library(WHO)
codes <- get_codes()
whod <- get_data("MDG_0000000007")

remotes::install_github("GuangchuangYu/nCov2019")
library(nCov2019)
ncovr_data <- load_nCov2019()

list.files(file.path(system.file(package = "MSG"), 'extdata'))
file.path(system.file(package = "MSG"), 'extdata', 'covidcountries.rds')


# Cleaning data ----

myfile <- "data/data_cleaning_example1.txt"
file.info(myfile)
file.show(myfile)

ex1 <- read.csv(myfile, header = FALSE)
str(ex1)
dim(ex1) # dimension
names(ex1) # or colnames(ex1)
View(ex1)

unique(ex1$V3)
sapply(ex1, unique) # or lapply(ex1, unique)

fecitr::plot_summary(ex1, if_quote = TRUE)

names(ex1) <- c("age", "height", "gender")
library(dplyr)
ex1 <- rename(ex1, age = V1, height = V2, gender = V3)
ex1 <- read.csv(myfile, col.names = c("age", "height", "gender"))
ex1 <- sjlabelled::var_labels(ex1,
                              age = "years",
                              height = "inches")
str(ex1)

class(ex1) # object type
class(ex1$age)
is.matrix(ex1)
is.data.frame(ex1)
is.list(ex1)
is.numeric(ex1$age)
is.character(ex1$age)
sapply(ex1, class) #class of each column

as.matrix(ex1)
as.list(ex1)
as.character(ex1$age)
as.numeric(ex1$height)

ex1$height_cor <- ex1$height
ex1$height_cor <- readr::parse_number(ex1$height_cor)
class(ex1$height_cor)

ex1$gender_cor <- c('male', 'male', 'female', 'male', 'female', 'female')
trimws(ex1$gender)
ex1$gender_cor <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", ex1$gender)
unique(ex1$gender_cor)
ifelse(ex1$gender_cor == "M",
       "male",
       "female")

sapply(ex1$gender_cor,
       function(x)
         switch(x,
                "M" = "male",
                "Female" = "female",
                "fem" = "female")
)

tbl_ref <- data.frame(
  tibble::tribble(
    ~old,   ~new,
    'm',    'male',
    'man',  'male',
    'male', 'male',
    'f',    'female',
    'fem',  'female',
    'woman','female',
    'female','female'
  )
)
ex1$gender_cor <- tbl_ref[match(tolower(ex1$gender_cor), tbl_ref$old), 'new']

ex1$gender_cor <- as.factor(ex1$gender_cor)
str(ex1)

ex1$age_cor <- ex1$age
ex1[ex1$age_cor == -9999, 'age_cor'] <- NA

ex1$note <- NA
ex1[ex1$height_cor == 62, 'note'] <- 'add a decimal point'
ex1[ex1$height_cor == 62, 'height_cor'] <- 6.2


library(ggplot2)
ex1 |>
  ggplot() +
  geom_boxplot(aes(age_cor))

ex_box <- boxplot.stats(ex1$age_cor)
ex_box

ex1$age_outlier <- 0
ex1$age_outlier[ex1$age_cor %in% ex_box$out] <- 1

boxplot(ex1$age_cor, range = 3)
ex1 |>
  ggplot() +
  geom_boxplot(aes(age_cor), coef = 3)
boxplot.stats(ex1$age_cor, coef = 3)

outliers::outlier(ex1$age_cor)

mean(ex1$age_cor)
mean(ex1$age_cor, na.rm = TRUE)

read.csv(myfile,
         header = FALSE,
         na.strings = c("-9999", "N.A."))

anyNA(ex1)
which(apply(ex1, 1, anyNA))
which(apply(ex1, 2, anyNA))
is.na(ex1)
sum(is.na(ex1))
ex2 <- na.omit(ex1)

# Describing data ----

tab1 <- table(diet$job) # one-way table
tab1
tab2 <- table(diet[, c('job', 'chd')]) # two-way table
tab2
tab3 <- table(diet[, c('job', 'chd', 'energy.grp')]) # three-way table
tab3

tabp2 <- prop.table(tab2)
tabp2
tabp21 <- prop.table(tab2, margin = 1)
tabp21
tabp22 <- prop.table(tab2, margin = 2)
tabp22

library(pubh)
diet[, c('job', 'chd')] |>
  sjlabelled::var_labels(job = "Job", chd = "CHD") |>
  cross_tbl(by = "chd") |>
  theme_pubh()

ftable(tab3)
dtf3 <- data.frame(tab3)

library(dplyr)
dtf2 <- diet |>
  count(job, chd)
dtfp2 <- transform(dtf2, prop = prop.table(n))

xtab2 <- xtabs(n ~ job + chd, dtf2)
dtf2w <- pivot_wider(dtf2, names_from = chd, values_from = n)

rowSums(tab2)
rowMeans(tab2)
colSums(tab2)
colMeans(tab2)
apply(tab2, MARGIN = 1, max) # maximum for each row
apply(tab2, MARGIN = 2, range) # range for each column
addmargins(tab2)
addmargins(tab2, FUN = max)

library(Epi)
stat.table(list(job, energy.grp), data = diet, margins = TRUE)

data(diet, package = "Epi")
dietn <- diet[, c('y', 'energy', 'height', 'weight', 'fat', 'fibre')]
summary(dietn)

mean(dietn$height, na.rm = TRUE)
mean(dietn$height, trim = 0.1, na.rm = TRUE)

n <- sum(!is.na(dietn$height))
n / sum(1/dietn$height, na.rm = TRUE)
# or
1 / mean(1/dietn$height, na.rm = TRUE)


x <- 1:10
prod(x) ^ (1 / 10)
prod(dietn$height, na.rm = TRUE) ^ (1/n)
exp(mean(log(dietn$height), na.rm = TRUE))

median(dietn$height, na.rm = TRUE)

max(dietn$height, na.rm = TRUE)
min(dietn$height, na.rm = TRUE)
range(dietn$height, na.rm = TRUE)
max(dietn$height, na.rm = TRUE) - min(dietn$height, na.rm = TRUE)
# or
diff(range(dietn$height, na.rm = TRUE))

var(dietn$height, na.rm = TRUE)
sd(dietn$height, na.rm = TRUE)
quantile(dietn$height, probs = 0.10, na.rm = TRUE)
quantile(dietn$height, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
IQR(dietn$height, na.rm = TRUE)
fivenum(dietn$height, na.rm = TRUE)

fecitr::plot_summary(x = dietn, base = 'hist', if_box = TRUE, if_normline = FALSE, if_skewness = FALSE, digits = c(2, 1, 0, 1, 1, 2))

e1071::skewness(dietn$height)
psych::skew(dietn)
# or
apply(dietn, MARGIN = 2, e1071::skewness, na.rm = TRUE)

e1071::kurtosis(dietn$height, na.rm = TRUE)
psych::kurtosi(dietn)

library(psych)
dtf_desc <- describe(diet, quant = c(0.25, 0.75), IQR = TRUE)
dtf_des1 <- describe(diet ~ job)
dtf_des2 <- describe(diet ~ job + fail)

dtf_pastecs <- pastecs::stat.desc(diet, norm = TRUE)

tapply(diet$height, diet$job, mean, na.rm = TRUE)
tapply(diet$height, list(diet$job, diet$energy.grp), mean, na.rm = TRUE)

stat.table(list(job, energy.grp),
           contents = list(mean(height, na.rm = TRUE),
                           sd(height, na.rm = TRUE)),
           data = diet, margins = TRUE)

diet |>
  group_by(job, energy.grp) |>
  summarise(mean = mean(height, na.rm = TRUE),
            sd = sd(height, na.rm = TRUE),
            median = median(height, na.rm = TRUE))

dtf2td1 <- dtf2 |>
  group_by(chd) |>
  mutate(prop = prop.table(n))

dtf2td2 <- dtf2 |>
  group_by(job) |>
  mutate(prop = prop.table(n))

library(fecitr)
diet[, c('height', 'weight', 'job', 'energy.grp')] |>
  plot_summary(if_box = TRUE)

diet[, c('height', 'weight')] |>
  plot_summary(base = 'hist', if_box = TRUE,
               if_normline = TRUE, if_skewness = TRUE)

# Analyzing data ----

x1 <- 1
d1 <- dnorm(x = x1, mean = 0, sd = 1)
d1

p1 <- pnorm(q = x1, mean = 0, sd = 1)
p1

integrate(dnorm, -Inf, x1)

qnorm(p1, mean = 0, sd = 1)
rnorm(n = 1,mean = 0, sd = 1)

x <- diet$weight[as.numeric(diet$energy.grp) == 1]
t.test(x, alternative = 'less', mu = 75, conf.level = 0.9)

# method 1
y <- diet$weight[as.numeric(diet$energy.grp) == 2]
t.test(x, y, alternative = 't', var.equal = FALSE)
# method 2
t.test(diet$weight ~ diet$energy.grp, alternative = 't', var.equal = FALSE)

chisq.test(x = c(90, 101, 192), p = c(0.25, 0.25, 0.5))
tbl <- table(diet[, c('job', 'chd')])
chisq.test(tbl)
chisq.test(x = diet$job, y = diet$chd)

library(pubh)
data(Oncho)
chisq.test(x = Oncho$area, y = Oncho$mf, correct = TRUE)
contingency(mf ~ area, data = Oncho)

summary(aov(weight ~ job, diet))
dtf_wide <- data.frame(Treatment1 = c(9, 9, 10),
                       Treatment2 = c(12, 12, 13),
                       Treatment3 = c(12, 13, 14))
dtf_long <- stack(dtf_wide)
myaov <- aov(values ~ ind, data = dtf_long)
summary(myaov)

set.seed(2022)
data(NHANES, package = 'NHANES')
dtf <- NHANES[NHANES$Age > 17, c('Age', 'BPSysAve', 'Gender')]
dtf_sub <- dtf[sample(1:nrow(dtf), 60), ]
lm_ba <- lm(BPSysAve ~ Age, data = dtf_sub)
summary(lm_ba)
confint(lm_ba, level = 0.95)
dtf_glm <- pubh::glm_coef(lm_ba, alpha = 0.05, type = 'ext')
dtf_ci <- psychometric::CI.Rsqlm(lm_ba)
dtf_pred <- jtools::make_predictions(lm_ba, new_data = data.frame(Age = 2:7 * 10))
library(ggplot2)
ggplot(dtf_sub, aes(Age, BPSysAve)) + geom_point() + geom_smooth(method = 'lm')

dtf_naomit <- na.omit(dtf_sub)
UsingR::simple.lm(dtf_naomit$Age, dtf_naomit$BPSysAve, show.ci = TRUE, show.residuals = TRUE)

lm_bag <- lm(BPSysAve ~ Age + Gender, data = dtf)
summary(lm_bag)
as.integer(factor(levels(dtf$Gender)))
dtf_dict <-  data.frame(level = levels(dtf$Gender),
                        value = as.integer(factor(levels(dtf$Gender))) - 1)
lm_bagi <- lm(BPSysAve ~ Age * Gender, data = dtf)
summary(lm_bagi)

data(melanoma, package = "boot")
melanoma2 <- melanoma[melanoma$status != 3, c('status', "thickness")]
melanoma2$status[melanoma2$status == 2] <- 0
glm_st <- glm(status~thickness, data = melanoma2, family = "binomial")
summary(glm_st)
confint(glm_st)
exp(confint(glm_st))

plot(melanoma2$thickness, melanoma2$status)
curve(predict(glm_st, data.frame(thickness = x), type = 'response'), add = TRUE)

ggplot(melanoma2, aes(thickness, status)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

melanoma3 <- melanoma[melanoma$status != 3, c('status', "thickness", "ulcer")]
melanoma3$status[melanoma3$status == 2] <- 0
glm_stu <- glm(status~thickness + ulcer, data = melanoma3, family = "binomial")
summary(glm_stu)

library(survival)
coxph(Surv(time, status) ~ sex, data = lung)
cox.zph(coxph(Surv(time, status) ~ sex, data = lung))

# Visualizing data ----

demo(graphics)
demo(persp)
demo(image)

data(diet, package = "Epi")
library(ggplot2)
ggplot(diet) + # initialization
  geom_point(aes(height, weight)) + # add scatter points
  theme_bw() # set the theme

example(qplot)

library(GGally)
p1 <- diet[, c('height', 'weight', 'fat', 'fibre', 'chd')] |>
  transform(chd = factor(chd)) |>
  ggpairs(aes(colour = chd, alpha = 0.1)) +
  theme_bw()
p1

library(plotly)
ggplotly(p1)
library(plotly)
demo('sf-plotly-3D-globe')

library(lattice)
bwplot(job ~ fat | factor(chd), data = diet)

tab1 <- sort(tab1, decreasing = TRUE)
pie_label <- paste0(names(tab1), ': ', tab1,
                    ' (', round(prop.table(tab1) * 100), '%)')
pie(tab1, labels = pie_label, density = 6, angle = 120 * 1:3)

dtf1 <- data.frame(n = c(tab1), Job = names(tab1))
library(ggplot2)
ggplot(dtf1, aes(x = "", y = n, fill = Job))+
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = 'y', start = 0) +
  theme_void()

library(ggpubr)
ggpie(dtf1, "n", fill = "Job")

library(plotly)
plot_ly(data.frame(tab1), labels = ~Var1, values = ~Freq, type = 'pie')

library(plotrix)
pie3D(tab1, labels = pie_label, explode = 0.1)

barplot(tab1, horiz = TRUE, las = 1)
dtf1 |>
  mutate(Job = reorder(Job, -n)) |>
  ggplot() +
  geom_bar(aes(x = Job, y = n), stat = 'identity') +
  coord_flip()
plot_ly(x = dtf1$n, y = dtf1$Job, type = "bar", orientation = 'h')
ggplot(diet) + geom_bar(aes(job))

barplot(tab2, xlab = 'CHD',
        density = 6, angle = 120 * 1:3, beside = TRUE, col = 1,
        legend.text = TRUE, args.legend = list(
          x =  8, y = 100,
          legend = dimnames(tab2)$job,
          bty = 'n',
          density = 15, angle = 120 * 1:3))
ggplot(diet) +
  geom_bar(aes(chd, fill = job), position = 'dodge')
plot_ly(dtf2, x = ~chd, y = ~n, color = ~job, type = "bar")

barplot(tab2, xlab = 'CHD')
ggplot(diet) + geom_bar(aes(chd, fill = job))
plot_ly(dtf2, x = ~chd, y = ~n, color = ~job, type = "bar")  |>
  layout(barmode = 'stack')

ggplot(diet) +
  geom_bar(aes(chd, fill = job)) +
  facet_grid( ~ energy.grp) # or facet_wrap( ~ energy.grp)

mosaicplot(tab3, shade = TRUE)

thecol <- rgb(0, 0, 0, alpha = 0.5)
thexlab <- 'Tumour thickness (mm)'
dotchart(melanoma$thickness, pch = 15, xaxt = 'n', lcolor = "white")
stripchart(melanoma$thickness, col = thecol, pch = 15, xaxt = 'n')
stripchart(melanoma$thickness, col = thecol, pch = 15, xlab = thexlab, method = "stack")

hist(melanoma$thickness)
ggplot(melanoma) +
  geom_histogram(aes(x = thickness)) +
  theme_bw()
plot_ly(x = ~melanoma$thickness, type = "histogram")

library(patchwork)
p1 <-
  ggplot(melanoma, aes(x = thickness)) +
  geom_histogram() +
  # geom_histogram(aes(y= ..density..), fill = 'grey') +
  # geom_density() +
  xlim(-2, 18) +
  labs(x = NULL) +
  # labs(x = 'Tumour thickness (mm)', y = 'Density') +
  theme_bw()

p2 <-
  ggplot(melanoma, aes(x = thickness, y = 0)) +
  geom_violin(fill = 'grey') +
  geom_boxplot(width = 0.1) +
  xlim(-2, 18) +
  labs(x = 'Tumour thickness (mm)') +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank())
p1 / p2 +  plot_annotation(tag_levels = 'A')

nclass.Sturges(melanoma$thickness)
nclass.scott(melanoma$thickness)
nclass.FD(melanoma$thickness)

mean_th <- data.frame(sex = c(0, 1),
                      mean = tapply(melanoma$thickness, melanoma$sex, mean))
boxplot(melanoma$thickness ~ melanoma$sex, horizontal = TRUE)
points(mean_th$mean, 1:2, pch = 4)
ggplot(melanoma, aes(x = sex, y = thickness, group = sex)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 4) +
  coord_flip() +
  theme_bw()
plot_ly(x = ~melanoma$thickness, y = ~melanoma$sex, orientation = 'h',
        type = "box", boxmean = TRUE)

library(vioplot)
vioplot(thickness ~ sex, horizontal = TRUE, data = melanoma)
points(mean_th$mean, 1:2, pch = 4, col = "white")
ggplot(melanoma, aes(x = sex, y = thickness, group = sex)) +
  geom_violin(fill = 'grey') +
  geom_boxplot(width = 0.1) +
  stat_summary(fun = "mean", geom = "point", shape = 4) +
  coord_flip() +
  theme_bw()
plot_ly(x = ~thickness, y = ~sex, type = "violin", data = melanoma,
        orientation = 'h',
        box = list(visible = TRUE), meanline = list(visible = TRUE))

data(diet, package = 'Epi')
dietlm <- lm(weight~height, data = diet)
plot(diet$height, diet$weight,
     xlab = 'Height (cm)', ylab = 'Weight (kg)', las  = 1, pch = 16)
abline(dietlm)
ggplot(diet, aes(height, weight)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = 'Height(cm)', y = 'Weight (km)') +
  theme_bw()
diet1 <- na.omit(diet[, c('height', 'weight')])
diet1lm <- lm(weight~height, data = diet1)
plot_ly(diet1, x = ~height) |>
  add_markers(x = ~height, y = ~weight) |>
  add_lines(x = ~height, y = fitted(diet1lm))

data(NHANES, package = 'NHANES')
plot(NHANES$Age, NHANES$BPSysAve, pch = 16, col = rgb(0, 0, 0, alpha = 0.2))
rug(NHANES$Age, side = 3, col = rgb(1, 0, 0, alpha = 0.2))
rug(NHANES$BPSysAve, side = 4, col = rgb(1, 0, 0, alpha = 0.2))

ggplot(NHANES, aes(Age, BPSysAve)) +
  geom_point(alpha = 0.2, shape = 15) +
  theme_bw()
ggplot(NHANES, aes(Age, BPSysAve)) +
  geom_bin2d() +
  theme_bw()

plot_ly(x = NHANES$Age, y = NHANES$BPSysAve) |> add_histogram2d()

library(survey)
data(api)
dstrat <- svydesign(id = ~ 1, strata = ~ stype, weights = ~ pw, data = apistrat, fpc = ~ fpc)
svyplot(api00 ~ api99, design = dstrat, style = "bubble")

pairs(dietn)
plot(dietn)
library(GGally)
ggpairs(dietn)
ggplotly(ggpairs(dietn))

panel.hist <- function(x){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "grey")
}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use = 'complete.obs'))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
panel.reg <- function(x, y) {
  points(x, y, col = 'grey', pch = 16)
  abline(lm(y~x), col = 'red')
}
pairs(dietn,
      diag.panel = panel.hist,
      upper.panel = panel.cor,
      lower.panel = panel.reg)

my_fn <- function(data, mapping){
   ggplot(data = data, mapping = mapping) +
    geom_point() +
    geom_smooth(method='lm')
}
ggpairs(dietn, lower = list(continuous = my_fn))

ggplotly(ggpairs(dietn))

library(rgl)
plot3d(diet$height, diet$weight, diet$fat)

set.seed(2022)
diet_sub <- diet[sample(1:nrow(diet), 30), ]
ggplot(diet_sub) +
  geom_point(aes(x = weight, y = height, colour = fat))
ggplot(diet_sub) +
  geom_point(aes(x = weight, y = height, size = fat), shape = 1)

coplot(weight ~ height | fat, data = diet, pch = 16)

Sys.setlocale('LC_CTYPE', 'Chinese')
library(MSG)
source(system.file("extdata", "ChinaPop.R", package = "MSG"), encoding = "UTF-8")
library(pinyin)
dimnames(ChinaPop)[[1]] <- toupper(
  py(dimnames(ChinaPop)[[1]], sep = '',
     dic = pydic(method = 'toneless', dic = 'pinyin2')
     )
  )
dimnames(ChinaPop)[[2]] <- c('Growth rate', 'Population', 'Urbanization level',
                             'Life expectancy', 'Well-educated population')
ChinaPop[, 5] <- ChinaPop[, 5]/1000
ChinaPop[, 1:2] <- apply(ChinaPop[, 1:2], 2, function(x)
  20 * (x - min(x)) / (max(x) - min(x)) + 5)
Sys.setlocale('LC_CTYPE', 'English')
symbols(ChinaPop[, 4], ChinaPop[, 5],
        thermometers = ChinaPop[, 1:3], fg = "gray40",
        inches = 0.5, las = 1,
        xlab = "Average Life Expectancy (years)",
        ylab = "Well-educated population (thousand)"
)
library(KernSmooth)
est <- bkde2D(ChinaPop[, 4:5], apply(ChinaPop[, 4:5], 2, dpik))
contour(est$x1, est$x2, est$fhat, add = TRUE, lty = "12")
for (i in 1:nrow(ChinaPop)) {
  text(ChinaPop[i, 4], ChinaPop[i, 5], rownames(ChinaPop)[i],
       cex = 0.75, adj = attr(ChinaPop, "adj")[i, ], col = 'red',
  )
}
rug(ChinaPop[, 4], 0.02, side = 3, col = "blue")
rug(ChinaPop[, 5], 0.02, side = 4, col = "blue")
boxplot(ChinaPop[, 4],
        horizontal = TRUE, pars = list(
          boxwex = 7,
          staplewex = 0.8, outwex = 0.8
        ), at = -6, add = TRUE, notch = TRUE, col = "skyblue",
        xaxt = "n"
)
boxplot(ChinaPop[, 5],
        at = 63, pars = list(
          boxwex = 1.4,
          staplewex = 0.8, outwex = 0.8
        ), add = TRUE, notch = TRUE, col = "skyblue",
        yaxt = "n"
)

library(nCov2019)
data_covid <- query()
dtf_covid <- data_covid$historical$table
dtf_usa <- dtf_covid[dtf_covid$country == 'USA', ]
plot(dtf_usa$date, dtf_usa$deaths, type = 'l')
ggplot(dtf_usa) +
  geom_line(aes(x = date, y = deaths)) +
  theme_bw()
plot_ly(dtf_usa, x = ~date, y = ~deaths, type = 'scatter', mode = 'lines')

plot(dtf_usa$date, dtf_usa$deaths, type = 'n')
polygon(c(dtf_usa$date, dtf_usa$date[nrow(dtf_usa)]),
        c(dtf_usa$deaths, 0), border = NA, col = 'grey')
ggplot(dtf_usa) +
  geom_area(aes(x = date, y = deaths), fill = 'grey')
plot_ly(dtf_usa, x = ~date, y = ~deaths,
        mode = 'lines', fill = 'tozeroy')
dtf_na <- dtf_covid[dtf_covid$country %in% c('USA', 'Canada', 'Mexico'), ]
ggplot(dtf_na) +
  geom_line(aes(x = date, y = deaths, colour = country))
ggplot(dtf_na) +
  geom_area(aes(x = date, y = deaths, fill = country))

dtf_wide <- dtf_na[, c('date', 'country', 'cases')] |>
  tidyr::pivot_wider(names_from = country, values_from = cases)
xts::xts(dtf_wide[, -1], order.by = dtf_wide$date) |>
  dygraphs::dygraph()

pdf('diet-fat-hist.pdf')
hist(diet$fat)
dev.off()

p1 <- ggplot(diet) + geom_histogram(aes(fat))
ggsave('diet-fat-hist-gg.pdf', p1)

g1 <- ggplotly(p1)
htmlwidgets::saveWidget(g1, 'diet-fat-hist-plotly.html')

# Presenting data ----

tinytex::install_tinytex()

rticles::journals()

z <- list(h_mean = round(mean(diet$height, na.rm = TRUE)),
          h_sd = round(sd(diet$height, na.rm = TRUE)),
          h_n = sum(!is.na(diet$height)))

x <- c(176.8, 181.4, 168.7, 187.8, 167.6, 180.3, 172.5, 165.4, 175.9, 165.9)
mu <- 170
xt <- t.test(x, mu = mu)
z1 <- list(n = length(x),                 # sample size
           m = round(xt$estimate, 1),               # sample mean
           s = round(sd(x), 1),                     # sample standard deviation
           ci = round(xt$conf.int, 1), # confidence interval
           t = round(xt$statistic, 3),              # t score
           df = xt$parameter,             # degree of freedom
           p = round(xt$p.value, 3))             # p-value

set.seed(2022)
data(NHANES, package = 'NHANES')
dtf <- NHANES[NHANES$Age > 17, c('Age', 'BPSysAve', 'Gender')]
dtf_sub <- dtf[sample(1:nrow(dtf), 60), ]
lm_ba <- lm(BPSysAve ~ Age, data = dtf_sub)
summary(lm_ba)
lm_bas <- summary(lm_ba)
str(lm_bas)
z$lm_r2 <- round(lm_bas$r.squared, 3)
z$lm_p <- round(lm_bas$coefficients[2, 4], 6)
z$lm_f <- round(lm_bas$fstatistic[1], 3)
z$lm_t <- round(lm_bas$coefficients[2, 3], 2)
z$lm_n <- length(lm_bas$residuals)
z$lm_df <- lm_bas$df[1:2]
z$lm_b0 <- round(lm_bas$coefficients[1, 1])
z$lm_b1 <- signif(lm_bas$coefficients[2, 1], digits = 3)

equatiomatic::extract_eq(lm_ba, use_coefs = TRUE)


library(tibble)
dtf_summary1 <- tribble(
  ~ Variable, ~ Mean, ~ `Standard deviation`, ~ `Sample Size`,
  'Age'      ,  37   , 22                  , 10000       ,
  'Height'   ,  162  , 20                  , 9647        ,
  'Weight'   ,  71.0 , 29.1                , 9922        ,
  'BPSysAve' ,  118  , 17                  , 8551        ,
)
knitr::kable(dtf_summary1, format = 'latex', caption = 'Summary of the NHANES dataset')


dtf_NHANES <- psych::describe(NHANES[, c('Age', 'Height', 'Weight', 'BPSysAve')])
knitr::kable(dtf_NHANES[, 1:5],
             format = 'latex',
             caption = 'Summary for NHANES dataset by the psych package.')

stargazer::stargazer(lm_ba,
                     header = FALSE,
                     label = 'summary3',
                     title = 'Summary of the linear regression.')

p1 + p2 + plot_annotation(tag_levels = 'A')

# Organize 6 figures with par()
par(mfrow = c(2, 3)) # 2 rows, 3 columns
hist(women$height)
boxplot(women$height)
plot(women$height, women$weight)
plot(0, type = 'n', axes = FALSE, xlab = '', ylab = '')
barplot(VADeaths)
pie(VADeaths[, 1])

# Organize 6 figures with layout()
layout(matrix(1:6, nrow = 2), widths = c(3, 2, 1))
hist(women$height)
boxplot(women$height)
plot(women$height, women$weight)
plot(0, type = 'n', axes = FALSE, xlab = '', ylab = '')
barplot(VADeaths)
pie(VADeaths[, 1])

data(diet, package = "Epi")
diet[, c('height', 'weight', 'job', 'energy.grp')] |>
  fecitr::plot_summary(if_box = TRUE)

library(gridExtra)
library(grid)
grid.arrange(
  tableGrob(women),
  ggplot(women) + geom_point(aes(height, weight)),
  ncol = 2,
  widths = c(1.5, 1),
  top = "Title of the page",
  bottom = textGrob(
    "this footnote is right-justified",
    gp = gpar(fontface = 3, fontsize = 9),
    hjust = 1,
    x = 1))

# Managing data ----

prodigenr::setup_project("dph")
mydir <- list.files('dph', full.names = TRUE, recursive = TRUE)
mytree <- data.tree::as.Node(data.frame(pathString = mydir))
print(mytree)
rosr::create_rosr('dph2')
rosr::create_rosr('dph2', if_render = TRUE)

help(Vanderpump, package = 'pubh')
LEB <- WHO::get_data("WHOSIS_000001")
LEB <- as.data.frame(unclass(LEB[, -c(5, 6)]),
                     stringsAsFactors = TRUE)

names(LEB)
LEB2 <- LEB
names(LEB2)[5] <- 'Life expectancy (years)'

require(sjlabelled)
LEB3 <- var_labels(LEB,
                   value = "Life expectancy (years)")
str(LEB3$value)
get_label(LEB3)
write.csv(LEB, 'LEB.csv', row.names = FALSE, quote = FALSE)
read.csv('LEB.csv', skip = 6)

save(LEB, file = 'data/LEB.rda')
class(LEB)
dim(LEB)
str(LEB)
roxygen_colname <- function(x) {
  y <- paste0("#'   \\item{", names(x), "}{}")
  writeLines(y, 'clipboard')
}
roxygen_fct <- function(x){
  y <- paste("factor with", nlevels(x), "levels:",
             paste(levels(x), collapse = ', '))
  writeLines(y, 'clipboard')
}
roxygen_colname(LEB)
devtools::document()
devtools::load_all('D:/temp/phdata')
data(LEB)
help(LEB)
