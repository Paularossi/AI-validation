source("C:/Users/P70090005/OneDrive - Maastricht University/Desktop/phd/AI-validation/agreement_calculation/agreement_functions.R")

library(dplyr)
library(purrr)
library(tibble)
library(readxl)
library(writexl)

set.seed(123)

# encode a set comma-separated, "-" for empty
encode_set <- function(x) {
  x <- sort(unique(x))
  if (length(x) == 0) "-" else paste(x, collapse = ", ")
}

# sample a latent "true" set (allows empty set = "None")
sample_true_set <- function(K, size_dist = c("poisson", "empirical"),
                            lambda = 2, empirical_sizes = NULL) {
  size_dist <- match.arg(size_dist)

  s <- if (size_dist == "poisson") {
    rpois(1, lambda)
  } else {
    if (is.null(empirical_sizes)) stop("empirical_sizes must be provided for size_dist='empirical'.")
    sample(empirical_sizes, size = 1, replace = TRUE)
  }

  s <- max(0, min(s, K)) # bound size to [0, K]
  if (s == 0) return(integer(0))
  sample.int(K, size = s, replace = FALSE)
}

# generate one rater's set from the true set with TPR/FPR noise
gen_rater_set <- function(true_set, K, tpr, fpr) {
  labels <- 1:K
  in_true <- labels %in% true_set

  include <- logical(K)
  include[in_true] <- runif(sum(in_true)) < tpr
  include[!in_true] <- runif(sum(!in_true)) < fpr

  labels[include]
}


# simulate N items with R raters
simulate_multilabel_question <- function(N, K, R = 3, size_dist = "poisson", lambda = 2, empirical_sizes = NULL,
                                         tpr = 0.9, fpr = 0.01, prefix = "q") {
  true_sets <- replicate(N,
    sample_true_set(K, size_dist = size_dist, lambda = lambda,
                    empirical_sizes = empirical_sizes),
    simplify = FALSE
  )

  raters <- map(1:R, function(j) {
    map(true_sets, ~ gen_rater_set(.x, K, tpr = tpr, fpr = fpr)) |>
      map_chr(encode_set)
  })

  out <- tibble(item = 1:N)
  for (j in 1:R) out[[paste0(prefix, "_coder", j)]] <- raters[[j]]
  out
}

# to get the empirical sizes from our data
set_size <- function(x) {
    x <- as.character(x)
    ifelse(is.na(x) | x == "-" | trimws(x) == "", 0L,
            lengths(strsplit(gsub(" ", "", x), ",")))
}

get_empirical_sizes <- function(df, var) {
    c(
      set_size(df[[paste0(var, "_coder1")]]),
      set_size(df[[paste0(var, "_coder2")]]),
      set_size(df[[paste0(var, "_coder3")]])
    )
}




root_folder <- "C:/Users/P70090005/OneDrive - Maastricht University/Desktop/phd/AI-validation/data/"
plot_folder <- "C:/Users/P70090005/OneDrive - Maastricht University/Desktop/phd/AI-validation/plots/"

humans <- read_excel(paste(root_folder, "responses_human_final_sensitivity.xlsx", sep=""))

# simulate under the three scenarios (per question)
scenarios <- tribble(
  ~scenario,           ~tpr,  ~fpr,
  "near_disagreement", 0.30,  0.10,
  "random_like",       0.50,  0.05,
  "near_perfect",      0.90,  0.01
)

N <- 1000 # nr of ads
R <- 3 # nr of raters

# nr of labels per question
question_specs <- list(
  prem_offer = list(K = 10),
  marketing_str = list(K = 11),
  who_cat = list(K = 24)
)

# collect all observed coder set sizes across the 3 coders for all questions
emp_sizes <- list(
  prem_offer = get_empirical_sizes(humans, "prem_offer"),
  marketing_str = get_empirical_sizes(humans, "marketing_str"),
  who_cat = get_empirical_sizes(humans, "who_cat_clean")
)


sim_results <- imap_dfr(question_specs, function(spec, qname) {
    pmap_dfr(scenarios, function(scenario, tpr, fpr) {

    print(paste("Simulating question:", qname, "under scenario:", scenario))
    dat_sim <- simulate_multilabel_question(
      N = N,
      K = spec$K,
      R = R,
      empirical_sizes = emp_sizes[[qname]],
      tpr = tpr,
      fpr = fpr,
      prefix = qname
    )

    out <- list()
    idx <- 1

    # compute pairwise krippendorff's alpha and jaccard
    rater_pairs <- combn(names(dat_sim)[-1], 2, simplify = FALSE)
    for (pair in rater_pairs) {
      df1 <- dat_sim[[pair[1]]]
      df2 <- dat_sim[[pair[2]]]
      
      sims <- mapply(jaccard_similarity, df1, df2)
      jacc <- mean(sims, na.rm = TRUE)

      wt <- MASI_simmilarity_matrix(data.frame(df1, df2), sep = ", ")
      alpha_masi <- krippen.alpha.raw(ratings = data.frame(df1, df2), weights = wt)$est
      
      out[[idx]] <- data.frame(
        question = qname,
        rater1 = pair[1],
        rater2 = pair[2],
        n = nrow(dat_sim),
        jaccard = jacc,
        kripp_alpha_masi = alpha_masi$coeff.val,
        kripp_alpha_masi_ci = alpha_masi$conf.int,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }

    res <- bind_rows(out) %>%
      mutate(
        scenario = scenario,
        tpr = tpr,
        fpr = fpr
      )
  })
})

# save the results
write_xlsx(sim_results, file.path(plot_folder, "multi_agreement_simulation_results.xlsx"))