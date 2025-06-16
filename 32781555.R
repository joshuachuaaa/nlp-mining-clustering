# 1 ── Load packages (install once if needed) ──────────────────────────────
# install.packages(c("readtext", "quanteda"))
library(readtext)
library(quanteda)

# 2 ── Read the metadata CSV ───────────────────────────────────────────────
meta <- read.csv("docs_manifest.csv", stringsAsFactors = FALSE)

# 3 ── Make the IDs in the CSV match the filenames (add “.txt”) ────────────
meta$doc_id <- paste0(meta$doc_id, ".txt")

# 4 ── Read every text file in docs/  (nothing else) ───────────────────────
rt <- readtext("docs/*.txt")          # doc_id = filename with .txt

# 5 ── Merge text with metadata on doc_id ──────────────────────────────────
rt <- merge(rt, meta, by = "doc_id", all.x = TRUE)

# 6 ── Turn it into a Quanteda corpus ──────────────────────────────────────
corp <- corpus(rt)                    # metadata becomes docvars()

# 7 ── Quick check: first 5 docs should show filled-in metadata ───────────
summary(corp, n = 5)

# 1 - Load the required packages ------------------------------------------
# (run the install line only the very first time)
# install.packages(c("readtext", "quanteda", "stringi", "tools"))
library(readtext)
library(quanteda)
library(stringi)
library(tools)


# 3 - Read the manifest and align IDs -------------------------------------
meta <- read.csv("docs_manifest.csv", stringsAsFactors = FALSE)
meta$doc_id <- paste0(meta$doc_id, ".txt")     # add ".txt" so IDs = filenames

# 4 - Read all .txt files in docs/ ----------------------------------------
rt <- readtext("docs/*.txt", encoding = "UTF-8")   # doc_id is the filename

# 5 - Merge full text with metadata ---------------------------------------
rt <- merge(rt, meta, by = "doc_id", all.x = TRUE, sort = FALSE)

# 6 - Build the first corpus ----------------------------------------------
corp <- corpus(rt)                         # metadata becomes docvars()

# 7 - Clean out [Music] & timestamps ---------------------------------------
clean_txt <- stri_replace_all_regex(
  as.character(corp),
  pattern = "(\\[.*?\\]|\\b\\d{1,2}:\\d{2}\\b)",
  replacement = "",
  vectorize_all = FALSE
)

corp <- corpus(
  clean_txt,
  docnames = docnames(corp),               # keep gaming_01.txt, etc.
  docvars  = docvars(corp)                 # keep genre, title, …
)

# 8 - Tokenise, lowercase, remove stops, stem -----------------------------
toks <- tokens(corp, remove_numbers = TRUE, remove_punct = TRUE) |>
  tokens_tolower() |>
  tokens_remove(stopwords("en")) |>
  tokens_wordstem()

# 9 - Build a trimmed tf–idf DTM ------------------------------------------
dtm <- dfm(toks) |>
  dfm_trim(min_docfreq = 2) |>
  dfm_tfidf()

dtm

# 10 - Save a 25-token DTM for the appendix -------------------------------
dtm_small <- dfm_keep(dtm, pattern = names(topfeatures(dtm, 25)))
write.csv(convert(dtm_small, to = "data.frame"),
          "appendix_dtm25.csv", row.names = TRUE)

# 11 - Quick sanity check --------------------------------------------------
summary(corp, n = 5)      # should show real genre / title, not <NA>
