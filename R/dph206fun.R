#' Get the daily downloads of a package in the recent one year
#'
#' @param x Package name
#' @return a data frame
#' @export
#' @examples
#' c('Epi', 'EpiModel', 'epibasix', 'epiDisplay', 'epiR', 'epitools', 'nCov2019', 'pubh', 'survival', 'survey', 'surveillance', 'SPARSEMODr', 'WHO') |>
#'   lapply(cranlogs_cum) |>
#'   bind_rows() |>
#'   ggplot() +
#'   geom_line(aes(date, CumDownload)) +
#'   theme(axis.text.x = element_text(angle = 90)) +
#'   scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
#'   facet_wrap(~package, scales = 'free_y')
cranlogs_cum <- function(x) {
  cranlogs::cran_downloads(
    x,
    from = Sys.Date() - 365,
    to = Sys.Date()) |>
    transform(CumDownload = cumsum(count))
}

#' Retrieve historical citation data, the network of co-authors, the total publication number, the total distinct journal number, the top journal number, the year of the oldest article, the profile information, and the publication list for a scientist.
#'
#' @param id Google Scholar ID
#'
#' @return a list
#' @export
#'
#' @examples
#' gsid <- 'JoN88RAAAAAJ'
#' ls_person <- gs_person(gsid)
#' ls_journal <- gs_journal(ls_person$pub$journal[1])
#' ls_pub <- gs_pub(gsid, ls_person$pub$pubid[1])
gs_person <- function(id){
  list(citation = scholar::get_citation_history(id),
       coauthor = scholar::get_coauthors(id),
       n_article = scholar::get_num_articles(id),
       n_journal = scholar::get_num_distinct_journals(id),
       n_top = scholar::get_num_top_journals(id),
       oldest = scholar::get_oldest_article(id),
       profile = scholar::get_profile(id),
       pub = scholar::get_publications(id))
}

#' Retrieve the cites, impact factor (IF), the Eigenfactor score, the rank, the ISSN, the publisher, and so on, of a journal.
#'
#' @param journal
#'
#' @return a list
#' @export
#'
#' @examples
gs_journal <- function(journal){
  list(IF = scholar::get_impactfactor(journal),
       rank = scholar::get_journalrank(journal))
}

#' Retrieve the historic annual citations and the complete author list of a publication.
#'
#' @param id
#' @param pubid
#'
#' @return
#' @export
#'
#' @examples
gs_pub <- function(id, pubid){
  list(cite = scholar::get_article_cite_history(id, pubid),
       authors = scholar::get_complete_authors(id, pubid))
}

#' Create text for a Gannt chart
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' x <- tibble::tribble(
#' ~Start,        ~End,         ~Activity,        ~Status,
#' "2021-01-01", "2021-03-31", "Research design", 'done',
#' "2021-04-01", "2021-09-30", "Lab preparation", 'done',
#' "2021-10-01", "2021-10-31", "Outdoor test",    'active',
#' "2021-11-01", "2023-01-31", "Collect data",    'crit, active',
#' "2021-05-01", "2022-04-30", "R package dev.",  'active',
#' "2023-02-01", "2023-08-31", "Analyze data",    '',
#' "2023-06-01", "2024-01-01", "Manuscript",      ''
#' )
#' library(DiagrammeR)
#' gannt_txt <- paste('gantt',
#'                    'dateFormat  YYYY-MM-DD',
#'                    'section Basic Tasks',
#'                    create_gannt_txt(x[1:4,]),
#'                    'section Products',
#'                    create_gannt_txt(x[5:7,]),
#'                    sep = '\n')
#' mermaid(gannt_txt)
create_gannt_txt <- function(x){
  paste(
    paste0(x$Activity, ':',
           x$Status, ifelse(x$Status == '', '', ', '),
           1:nrow(x), ', ', x$Start, ',', x$End),
    collapse = '\n')
}

#' Plot a Gannt chart with ggplot2
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' library(ggplot2)
#' x <- tibble::tribble(
#' ~Start,        ~End,         ~Activity,        ~Status,
#' "2021-01-01", "2021-03-31", "Research design", 'done',
#' "2021-04-01", "2021-09-30", "Lab preparation", 'done',
#' "2021-10-01", "2021-10-31", "Outdoor test",    'active',
#' "2021-11-01", "2023-01-31", "Collect data",    'crit, active',
#' "2021-05-01", "2022-04-30", "R package dev.",  'active',
#' "2023-02-01", "2023-08-31", "Analyze data",    '',
#' "2023-06-01", "2024-01-01", "Manuscript",      ''
#' )
#' library(tidyr)
#' plot_gannt(x)
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

#' Generate roxygen items for a data frame
#'
#' @param x a dataframe
#'
#' @return
#' @export
#'
#' @examples
roxygen_colname <- function(x) {
  y <- paste0("#'   \\item{", names(x), "}{}")
  writeLines(y, 'clipboard')
}

#' Generate a roxygen item for a factor
#'
#' @param x A factor vector
#'
#' @return
#' @export
#'
#' @examples
roxygen_fct <- function(x){
  y <- paste("factor with", nlevels(x), "levels:",
             paste(levels(x), collapse = ', '))
  writeLines(y, 'clipboard')
}
