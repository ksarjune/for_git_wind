# Experiment: Each participant completed multiple verbal fluency tasks: one
# semantic, one phonological, one conjunctive (words must start with a letter
# AND belong to a category), and one disjunctive (words must start with a letter
# AND/OR belong to a category).
#
# The exact set of categories/letters differs across participants (counterbalanced
# via listtype) and is controlled for with (1 | listtype) in mixed models.
#
# Focusing on the disjunctive condition: what sort of switching leads to good
# performance (high N)?
#
# Definitions:
#   cue:         letter or category
#   hard switch: single cue -> different single cue (A->B or B->A)
#                e.g. "dog" (animal) -> "park" (P word)
#   no switch:   same cue(s) OR dropping a cue (A->A, B->B, AB->AB, AB->A, AB->B)
#                e.g. "park" -> "pancake" (both P words), or "panda" -> "fox" (drop P)
#   soft switch: single cue -> overlap only (A->AB or B->AB), i.e. a cue is ADDED
#                e.g. "park" (P) -> "panda" (P + animal)
#                note: dropping a cue (AB->A, AB->B) counts as no switch
#   overlap:     response fits both cues simultaneously (AB state)
#
# Research questions:
#   Q1) Do switch rates predict performance? (hard, soft, no-switch, overlap -> n_valid)
#   Q2) Which transition type has the fastest RT?
#   Q3) Do people tend to switch cues after an overlap? (e.g. "park" -> "panda" -> "fox")

library(data.table)
library(lmerTest)

# ---- Read trial-level data produced by data2use.R ----
dat_long <- fread("dat_long.csv")

# Restore factor columns lost during CSV round-trip
dat_long[, cue_combo := factor(cue_combo, levels = c("semantic_only", "letter_only", "both", "neither"))]
dat_long[, condition := factor(condition)]

# ==== Setup: per-participant metrics for disjunctive condition ====

# Compute switch rates per participant PER LIST (each participant has multiple
# disjunctive lists with different category/letter combos)
# Denominator = sum(itemnum > 1) = actual transitions (excludes 1st item)
disj_rates <- dat_long[condition == "disjunctive" & cue_valid == "yes", .(
  n_valid             = .N,
  overlap_proportion  = sum(overlap == "yes") / .N,
  hard_switch_rate    = sum(hard_switch == "yes") / sum(itemnum > 1),
  soft_switch_rate    = sum(soft_switch == "yes") / sum(itemnum > 1),
  no_switch_rate      = sum(any_switch == "no" & itemnum > 1) / sum(itemnum > 1)
), by = .(id, listtype)]

# Label each trial's switch type for trial-level models
dat_long[, switch_type := fifelse(
  soft_switch == "yes", "soft",
  fifelse(hard_switch == "yes", "hard", "no_switch")
)]

# Working subset: valid disjunctive responses
disj <- dat_long[condition == "disjunctive" & cue_valid == "yes"]


# ==== Q1: Do switch rates predict performance (n_valid)? ====
# Mixed models: does the rate of each transition type predict total words per list?
# (1 | id) = random intercept for participant
# (1 | listtype) = random intercept for category/letter combo

# Overlap proportion -> performance
#lmm_q1_overlap <- lmer(n_valid ~ overlap_proportion + (1 | id) + (1 | listtype), data = disj_rates)
#summary(lmm_q1_overlap)

# Hard switch rate -> performance
lmm_q1_hard <- lmer(n_valid ~ hard_switch_rate + (1 | id) + (1 | listtype), data = disj_rates)
summary(lmm_q1_hard)

# Soft switch rate -> performance
lmm_q1_soft <- lmer(n_valid ~ soft_switch_rate + (1 | id) + (1 | listtype), data = disj_rates)
summary(lmm_q1_soft)

# No-switch rate -> performance
lmm_q1_noswitch <- lmer(n_valid ~ no_switch_rate + (1 | id) + (1 | listtype), data = disj_rates)
summary(lmm_q1_noswitch)


# ==== Q2: Which transition type has the fastest RT? ====

# Descriptive: mean RT per switch type (exclude first items -- not transitions)
cat("=== Mean RT by switch type ===\n")
print(disj[itemnum > 1, .(mean_rt = mean(rt), n = .N), by = switch_type])

# Mixed model: switch_type -> RT
# (1 | id) = random intercept for participant
# (1 | listtype) = random intercept for category/letter combo
# Reference level = "hard" (alphabetical); coefficients for no_switch and soft
# show how much faster they are relative to hard switches
lmm_q2 <- lmer(rt ~ switch_type + (1 | id) + (1 | listtype),
               data = disj)
summary(lmm_q2)

# Pairwise comparisons (Bonferroni-corrected alpha = .05/3 = .017)

# hard vs soft
lmm_q2_hard_soft <- lmer(rt ~ switch_type + (1 | id) + (1 | listtype),
                         data = disj[switch_type %in% c("hard", "soft")])
summary(lmm_q2_hard_soft)

# hard vs no_switch
lmm_q2_hard_no <- lmer(rt ~ switch_type + (1 | id) + (1 | listtype),
                       data = disj[switch_type %in% c("hard", "no_switch")])
summary(lmm_q2_hard_no)

# soft vs no_switch
lmm_q2_soft_no <- lmer(rt ~ switch_type + (1 | id) + (1 | listtype),
                       data = disj[switch_type %in% c("soft", "no_switch")])
summary(lmm_q2_soft_no)


# ==== Q3: Do overlaps act as bridges between cues? ====
# The pattern "park" (P) -> "panda" (P+animal) -> "fox" (animal) is a bridge:
# the person entered the overlap from cue A and exited to cue B.
# If overlaps facilitate cue switching, bridges (A->AB->B) should occur more
# often than returns (A->AB->A). Under chance, the rate would be 50%.

# Get the cue type before and after each response
setorder(disj, id, listtype, itemnum)
disj[, prev_cue_combo := shift(cue_combo, 1, type = "lag"), by = .(id, listtype)]
disj[, next_cue_combo := shift(cue_combo, 1, type = "lead"), by = .(id, listtype)]

# Find overlap responses that are sandwiched between two single-cue responses
# (these are the only cases where we can test bridge vs return)
overlap_triplets <- disj[
  cue_combo == "both" &
  prev_cue_combo %in% c("semantic_only", "letter_only") &
  next_cue_combo %in% c("semantic_only", "letter_only")
]

# Bridge = entered from one cue, exited to the other (A->AB->B)
# Return = entered and exited to the same cue (A->AB->A)
overlap_triplets[, bridged := prev_cue_combo != next_cue_combo]

# Descriptive
cat("=== Bridge (A->AB->B) vs Return (A->AB->A) ===\n")
print(overlap_triplets[, .(n = .N), by = bridged])
cat("Bridge rate:", round(mean(overlap_triplets$bridged), 3), "\n")

# Binomial test against chance (50%)
print(binom.test(sum(overlap_triplets$bridged), nrow(overlap_triplets), p = 0.5))

# Mixed model: intercept-only logistic model controlling for participant and listtype
# Intercept > 0 (log-odds) = bridges more common than returns
# Intercept < 0 = returns more common (people go back to original cue)
# Intercept ~ 0 = no preference (50/50)
overlap_triplets[, bridged_int := as.integer(bridged)]
lmm_q3 <- glmer(bridged_int ~ 1 + (1 | id) + (1 | listtype),
                data = overlap_triplets, family = binomial)
summary(lmm_q3)

# Q4 - which switch type is most common

switches_only <- disj[switch_type %in% c("soft", "hard")]
switches_only[, is_soft := as.integer(switch_type == "soft")]
lmm_q4 <- glmer(is_soft ~ 1 + (1 | id) + (1 | listtype), data = switches_only, family = binomial)
summary(lmm_q4)

# > switches_only[,.N,by=is_soft]
#    is_soft     N
#      <int> <int>
# 1:       0   433
# 2:       1   214


# ==== Q5: Is performance correlated across conditions? ====
# Do people who generate more words in one condition also generate more in others?

# Performance per participant per list (one row per participant x listtype)
perf_by_list <- dat_long[cue_valid == "yes", .(n_valid = .N), by = .(id, condition, listtype)]

# Mixed model: condition predicts n_valid, with random intercepts for id and listtype
# Large (1 | id) variance = strong cross-condition consistency (individual fluency)
# ICC for id = proportion of variance due to stable individual differences
#lmm_q5 <- lmer(n_valid ~ condition + (1 | id) + (1 | listtype), data = perf_by_list)
#summary(lmm_q5)

# ICC: what proportion of variance is between-person (shared across conditions)?
#vc <- as.data.table(VarCorr(lmm_q5))
#id_var <- vc[grp == "id", vcov]
#total_var <- sum(vc$vcov)
#cat("ICC for participant:", round(id_var / total_var, 3), "\n")

# Pairwise correlations at the participant level (aggregate across lists per condition)
perf_by_cond <- perf_by_list[, .(n_valid = sum(n_valid)), by = .(id, condition)]
perf_wide <- dcast(perf_by_cond, id ~ condition, value.var = "n_valid")

cat("\n=== Semantic vs Disjunctive ===\n")
print(cor.test(perf_wide$semantic, perf_wide$disjunctive))

cat("\n=== Semantic vs Conjunctive ===\n")
print(cor.test(perf_wide$semantic, perf_wide$conjunctive))

cat("\n=== Semantic vs Phonological ===\n")
print(cor.test(perf_wide$semantic, perf_wide$phonological))

cat("\n=== Phonological vs Disjunctive ===\n")
print(cor.test(perf_wide$phonological, perf_wide$disjunctive))

cat("\n=== Phonological vs Conjunctive ===\n")
print(cor.test(perf_wide$phonological, perf_wide$conjunctive))

cat("\n=== Disjunctive vs Conjunctive ===\n")
print(cor.test(perf_wide$disjunctive, perf_wide$conjunctive))