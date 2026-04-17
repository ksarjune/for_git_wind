# ===========================================================================
# data2use.R — Data preparation for verbal fluency switching analysis
#
# Input:  data2run.csv (wide format, one row per participant)
#         category_dictionary_pruned.csv (valid category members)
# Output: dat_long.csv (trial-level data with cue/switch coding)
#
# Each participant completed 4 types of verbal fluency lists (8 lists each):
#   semantic:    name items from a CATEGORY (e.g. animals)
#   phonological: name items starting with a LETTER (e.g. F)
#   disjunctive: name items from a category OR starting with a letter
#   conjunctive: name items from a category AND starting with a letter
#
# The raw data stores each list as a comma-separated string of alternating
# timestamps and responses: "0, dog, 2031, cat, 4502, ..."
# ===========================================================================

library(data.table)
rm(list = ls())

# ---- Helper: normalize text for matching ----
clean <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("\\s+", " ", x)
  x
}

# ---- 1. Read wide-format data ----
dat <- fread("data2run.csv")

# Column names for all 32 lists (4 conditions x 8 lists)
cols <- c(
  paste0("semantic_", 1:8),
  paste0("phono_", 1:8),
  paste0("disj_", 1:8),
  paste0("conj_", 1:8)
)

# ---- 2. Reshape wide -> long (one row per list per participant) ----
long_lists <- melt(
  dat,
  id.vars      = "id",
  measure.vars = cols,
  variable.name = "listtype",
  value.name    = "raw_string"
)

# Split listtype into condition label ("semantic", "phono", etc.) and list number
long_lists[, c("list", "listnum") := tstrsplit(listtype, "_")]
long_lists[, listnum := as.integer(listnum)]

# Drop empty lists
long_lists <- long_lists[!is.na(raw_string) & nzchar(trimws(raw_string))]

# ---- 3. Parse raw strings into individual responses + inter-response times ----
# Each raw_string is: "timestamp1, response1, timestamp2, response2, ..."
# We extract responses and compute RT as the difference between consecutive timestamps
parse_list <- function(x) {
  parts <- trimws(strsplit(x, ",")[[1]])
  ts  <- suppressWarnings(as.numeric(parts[seq(1, length(parts), 2)]))
  rsp <- parts[seq(2, length(parts), 2)]

  ts  <- ts[!is.na(ts)]
  rsp <- rsp[seq_len(length(ts) - 1)]

  data.table(
    itemnum  = seq_along(rsp),     # position within this list (1, 2, 3, ...)
    rt       = diff(ts),           # inter-response time in ms
    response = rsp
  )
}

# Apply parse_list to every list, producing one row per response
dat_long <- long_lists[, parse_list(raw_string), by = .(id, listtype, list, listnum)]

dat_long[, response     := trimws(response)]
dat_long[, response_norm := clean(response)]

# ---- 4. Assign category and letter cues to each listtype ----
# Semantic lists have only a category (letter = NA)
# Phonological lists have only a letter (category = NA)
# Disjunctive and conjunctive lists have BOTH a category and a letter

dat_long[, category := fcase(
  listtype == "semantic_1", "clothing",
  listtype == "semantic_2", "food",
  listtype == "semantic_3", "furniture",
  listtype == "semantic_4", "bodyparts",
  listtype == "semantic_5", "instruments",
  listtype == "semantic_6", "tools",
  listtype == "semantic_7", "animals",
  listtype == "semantic_8", "toys",

  listtype == "disj_1", "furniture",
  listtype == "disj_2", "bodyparts",
  listtype == "disj_3", "instruments",
  listtype == "disj_4", "tools",
  listtype == "disj_5", "animals",
  listtype == "disj_6", "toys",
  listtype == "disj_7", "clothing",
  listtype == "disj_8", "food",

  listtype == "conj_1", "instruments",
  listtype == "conj_2", "tools",
  listtype == "conj_3", "animals",
  listtype == "conj_4", "toys",
  listtype == "conj_5", "clothing",
  listtype == "conj_6", "food",
  listtype == "conj_7", "furniture",
  listtype == "conj_8", "bodyparts",

  default = NA_character_
)]

dat_long[, letter := fcase(
  listtype == "phono_1", "f",
  listtype == "phono_2", "p",
  listtype == "phono_3", "s",
  listtype == "phono_4", "b",
  listtype == "phono_5", "n",
  listtype == "phono_6", "a",
  listtype == "phono_7", "j",
  listtype == "phono_8", "h",

  listtype == "disj_1", "n",
  listtype == "disj_2", "a",
  listtype == "disj_3", "j",
  listtype == "disj_4", "h",
  listtype == "disj_5", "f",
  listtype == "disj_6", "p",
  listtype == "disj_7", "s",
  listtype == "disj_8", "b",

  listtype == "conj_1", "j",
  listtype == "conj_2", "h",
  listtype == "conj_3", "f",
  listtype == "conj_4", "p",
  listtype == "conj_5", "s",
  listtype == "conj_6", "b",
  listtype == "conj_7", "n",
  listtype == "conj_8", "a",

  default = NA_character_
)]

# ---- 5. Score each response against its cues ----

# Load pruned category dictionary (valid category members)
cat_dict <- fread("category_dictionary_pruned.csv")
cat_dict[, category     := clean(category)]
cat_dict[, response_norm := clean(response_norm)]

# semantic = "yes" if the response is a valid member of its assigned category
dat_long[, semantic := "no"]
dat_long[cat_dict, semantic := "yes", on = .(category, response_norm)]

# phonological = "yes" if the response starts with the assigned letter
dat_long[, phonological := fifelse(
  !is.na(letter) & startsWith(response_norm, letter), "yes", "no"
)]

# overlap = "yes" if the response satisfies BOTH cues simultaneously
dat_long[, overlap := fifelse(semantic == "yes" & phonological == "yes", "yes", "no")]

# ---- 6. Code transitions (switches) between consecutive valid responses ----
# State labels for each response:
#   A  = letter cue only (phonological match)
#   B  = category cue only (semantic match)
#   AB = both cues (overlap)
#   0  = neither cue (invalid)

dat_long[, state := fcase(
  semantic == "yes" & phonological == "no",  "B",
  semantic == "no"  & phonological == "yes", "A",
  semantic == "yes" & phonological == "yes", "AB",
  default = "0"
)]

setorder(dat_long, id, listtype, itemnum)

# For switch detection, we only look at valid responses (state != "0")
dat_long[, state_valid := fifelse(state != "0", state, NA_character_)]

# Get the previous valid state within each list (skipping invalid responses)
prev_dt <- dat_long[
  state_valid %in% c("A", "B", "AB"),
  .(id, listtype, itemnum, prev_state = shift(state_valid)),
  by = .(id, listtype)
]
dat_long[, prev_state := NA_character_]
dat_long[prev_dt, prev_state := i.prev_state, on = .(id, listtype, itemnum)]

# hard switch: single cue -> different single cue (A->B or B->A)
dat_long[, hard_switch := fifelse(
  prev_state %in% c("A", "B") & state_valid %in% c("A", "B") & prev_state != state_valid,
  "yes", "no"
)]

# soft switch: single cue -> overlap, i.e. a cue is ADDED (A->AB or B->AB)
# Note: dropping a cue (AB->A or AB->B) is NOT a soft switch; it counts as no switch
dat_long[, soft_switch := fifelse(
  prev_state %in% c("A", "B") & state_valid == "AB",
  "yes", "no"
)]

# any switch: either hard or soft
dat_long[, any_switch := fifelse(hard_switch == "yes" | soft_switch == "yes", "yes", "no")]

# First item in each list cannot be a switch (no previous response)
dat_long[itemnum == 1, `:=`(hard_switch = "no", soft_switch = "no", any_switch = "no")]

# ---- 7. Clean up temporary columns and set final column order ----

dat_long[, c("state", "state_valid", "prev_state", "response_norm") := NULL]

setcolorder(dat_long, c(
  "id", "listtype", "list", "listnum", "category", "letter",
  "itemnum", "response", "rt",
  "semantic", "phonological", "overlap",
  "hard_switch", "soft_switch", "any_switch"
))

# ---- 8. Add derived columns ----

# cue_combo: which cue(s) this response satisfies
dat_long[, cue_combo := fcase(
  semantic == "yes" & phonological == "no",  "semantic_only",
  semantic == "no"  & phonological == "yes", "letter_only",
  semantic == "yes" & phonological == "yes", "both",
  default = "neither"
)]
dat_long[, cue_combo := factor(cue_combo, levels = c("semantic_only", "letter_only", "both", "neither"))]

# cue_valid: "yes" if the response fits at least one cue
dat_long[, cue_valid := fifelse(semantic == "yes" | phonological == "yes", "yes", "no")]

# condition: human-readable condition label
dat_long[, condition := factor(
  list,
  levels = c("semantic", "phono", "disj", "conj"),
  labels = c("semantic", "phonological", "disjunctive", "conjunctive")
)]

# ---- 9. Write processed data to disk ----
fwrite(dat_long, "dat_long.csv")
cat("Wrote dat_long.csv:", nrow(dat_long), "rows,", length(unique(dat_long$id)), "participants\n")
