# commented analyses 

# hard switch = switching from one cue to another, A->B
# soft switch = switching using one cue from another, AB->A; AB->B
# overlap = uses both cues to transition, AB->AB

# Create response_analysis with n_valid included -- im redoing this from my old code
response_analysis <- dat_long[condition == "disjunctive" & cue_valid == "yes", .(
  n_valid = .N,                                                    # Add this line!
  overlap_proportion = sum(overlap == "yes") / .N,
  hard_switch_rate = sum(hard_switch == "yes") / (.N - 1),
  soft_switch_rate = sum(soft_switch == "yes") / (.N - 1)
), by = id]

dat_long[, cluster_id := rleid(cue_combo), by = .(id, listtype)]
dat_long[, position_in_cluster := seq_len(.N), by = .(id, listtype, cluster_id)]

disj <- dat_long[condition == "disjunctive" & cue_valid == "yes"]

ss <- disj[soft_switch == "yes", .(rt = mean(rt)), by = id] # soft
ss[,mean(rt)] # 4132.509
ss

hs <- disj[hard_switch == "yes", .(rt = mean(rt)), by = id] # hard
hs[,mean(rt)] # 5523.611
hs

ns <- disj[any_switch == "yes", .(rt = mean(rt)), by = id] # no switch
ns[,mean(rt)] # 4847.041
ns 
# soft faster than no, no faster than hard

# research questions

# q1: Our first question asks how often participants rely on automatic, associative retrieval versus explicit, controlled cluster switching; this analysis is comparing overlap proportion with total valid responses 
disjunctive_data_clean[, overlap_proportion := n_overlap/n_valid]

# Model with overlap proportion
lm_q1_clean <- lm(n_valid ~ overlap_proportion, data = disjunctive_data_clean)
summary(lm_q1_clean)
# overlap proportion = -17.43, p < .001
# for every 1 increase in overlap proportion, there are 17.4 fewer words retrieved 
# when overlap is 0, intercept = 26.45 which means people are doing about 24-25 words? on average?
# so are they doing worse...? 


# q2: Our second research question is about what kinds of transitions occur as people navigate retrieval; this analysis is meant to compare soft switches v no switches; hard switches v no switches
# Calculate transition rates by participant
transition_analysis <- dat_long[condition == "disjunctive" & cue_valid == "yes", .(
  total_transitions = .N - 1,
  soft_transitions = sum(soft_switch == "yes"),
  hard_transitions = sum(hard_switch == "yes"),
  no_transitions = sum(any_switch == "no")
), by = id]

# Calculate transition rates (proportions)
transition_analysis[, `:=`(
  soft_transition_rate = soft_transitions / total_transitions,
  hard_transition_rate = hard_transitions / total_transitions,
  no_transition_rate = no_transitions / total_transitions
)]

# Compare soft vs. hard transition rates
t_test_transitions <- t.test(transition_analysis$soft_transition_rate, 
                            transition_analysis$hard_transition_rate, 
                            paired = TRUE)
t_test_transitions

# mixed ver of ttest
dat_long[, switch_type := fifelse(
  soft_switch == "yes", "soft",
  fifelse(hard_switch == "yes", "hard", "no_switch")
)]

# Mixed model: switch type predicts response time
lmm_q2 <- lmer(rt ~ switch_type + (1 + switch_type | id), 
               data = dat_long[condition == "disjunctive" & cue_valid == "yes"])
summary(lmm_q2)
# both the soft and no switch are significantly faster than hard switch, soft switch = fastest, soft witches also faster than hard switch. i guess this makes sense? because if youre already transitioning with overlap youre already soft switching 

# q3: The third research question asks if overlap-based retrieval is associated with better performance
# Create transition_analysis from dat_long
transition_analysis <- dat_long[condition == "disjunctive" & cue_valid == "yes", .(
  n_valid = .N,                                    # Total performance
  overlap_responses = sum(overlap == "yes"),        # Overlap count
  soft_transitions = sum(soft_switch == "yes"),     # Soft switch count
  hard_transitions = sum(hard_switch == "yes"),     # Hard switch count
  total_transitions = .N - 1                       # Total possible transitions
), by = id]
 
# Calculate rates and proportions
transition_analysis[, `:=`(
  overlap_proportion = overlap_responses / n_valid,
  soft_transition_rate = soft_transitions / total_transitions,
  hard_transition_rate = hard_transitions / total_transitions
)]
 
# Now the three models
lm_q3_overlap <- lm(n_valid ~ overlap_proportion, data = transition_analysis)
summary(lm_q3_overlap)
# looks similar to previous, overlap is hurting performance by b = -17.43


lm_q3_soft <- lm(n_valid ~ soft_transition_rate, data = transition_analysis)
summary(lm_q3_soft)
# don't seem to predict performance, p = .75


lm_q3_hard <- lm(n_valid ~ hard_transition_rate, data = transition_analysis)
summary(lm_q3_hard)
# also doesnt seem to predict performance, p = .277

# overall switch rate
# Calculate overall switch rate
transition_analysis[, overall_switch_rate := (soft_transitions + hard_transitions) / total_transitions]

# Test overall switching → performance; do people who switch more frequently generate more or fewer words?
lm_overall <- lm(n_valid ~ overall_switch_rate, data = transition_analysis)
summary(lm_overall)
# yea, not significant, p = .199 so overall switch rate isn't predicting performance.counting valid responses compared to the overall switch rate

# overall switch rate
lm_overall <- lm(n_valid ~ overall_switch_rate, data = transition_analysis)
summary(lm_overall)





# low overlap = highest productivity? soft switches = more efficient (faster) but not more words 
# switch more = more responses? no not really, no effect
# listing by switch type? soft = fastest, hard = slowest
# overlap proportion? people with high overlaps = worse
# more switches = more words? not really
# IRT difference? yes, hard > no > soft
# low overlap not no switching = more productivity


# look at semantic data and see how people generate 
# what proportion of animals start with P in semantic fluency, calculate proportion in each participant, calculate proportion of animals that start with P in the disjunctive, is there a difference between the two?


# Productivity Score = (Low Overlap Proportion) + (Soft Switch Efficiency)
#                   = Strong Positive Effect   + Neutral Speed Benefit
#                   = HIGHEST PRODUCTIVITY


## 4/17 notes
# ==== FOCUS ON DISJUNCTIVE CONDITION (WHERE SWITCHING HAPPENS) ====

# Get only disjunctive condition data
disj_corrs <- dat_long[condition == "disjunctive" & cue_valid == "yes", .(
  n_valid = .N,
  overlap_proportion = sum(overlap == "yes") / .N,
  soft_switch_rate = sum(soft_switch == "yes") / (.N - 1),
  hard_switch_rate = sum(hard_switch == "yes") / (.N - 1),
  overall_switch_rate = sum(any_switch == "yes") / (.N - 1)
), by = id]

disj_corrs_clean <- disj_corrs[overall_switch_rate > 0]

print(cor.test(disj_corrs_clean$n_valid, disj_corrs_clean$overlap_proportion))
# performance and overlap: corr = -.29 means negative relationship. as overlap increases, performance decreases. statistically significant*****

print(cor.test(disj_corrs_clean$n_valid, disj_corrs_clean$soft_switch_rate))
# soft switch/performance: corr = -.11, negative relationship. as soft switches increase, performance decreases, but ns

print(cor.test(disj_corrs_clean$n_valid, disj_corrs_clean$hard_switch_rate))
# hard switch/performance: corr = -.18, negative relationship. as hard switches increase, performance decreases, yes significant*****


print(cor.test(disj_corrs_clean$n_valid, disj_corrs_clean$overall_switch_rate))
# overall switch rate/performance: corr = -.25, negative relationship. as overall switchrate increases, performance decreases? people who switch more generate fewer words....???????? yes significant ***** 


#windsurf suggestion, i dont understand, please help:)
cor_matrix <- cor(disj_corrs_clean[, .(n_valid, overlap_proportion, soft_switch_rate, hard_switch_rate, overall_switch_rate)], use = "complete.obs")
print(cor_matrix)

# Check variability
cat("\n=== VARIABILITY CHECK ===\n")
print(disj_corrs_clean[, .(min_overlap = min(overlap_proportion),
                          max_overlap = max(overlap_proportion),
                          min_soft = min(soft_switch_rate),
                          max_soft = max(soft_switch_rate),
                          min_hard = min(hard_switch_rate),
                          max_hard = max(hard_switch_rate))])



## this creates the "blocks" and matches the participants based on the listtype they have
cat("Disjunctive listtypes:\n")
print(unique(dat_long[grepl("^disj_", listtype), listtype]))

# Create group column based on listtype
dat_long[, group := fifelse(
  listtype %in% c("disj_1", "disj_2"), "group_1",
  fifelse(listtype %in% c("disj_3", "disj_4"), "group_2",
  fifelse(listtype %in% c("disj_5", "disj_6"), "group_3",
  fifelse(listtype %in% c("disj_7", "disj_8"), "group_4", "other")
  )))]

# Check the grouping
cat("\n=== GROUP ASSIGNMENTS ===\n")
group_check <- dat_long[grepl("^disj_", listtype), .(
  participants = length(unique(id)),
  listtypes = paste(unique(listtype), collapse = ", ")
), by = group]
print(group_check)

# Create dataset with just disjunctive trials
disj_grouped <- dat_long[grepl("^disj_", listtype) & group != "other"]

# Verify
# cat("\n=== DISJUNCTIVE DATASET WITH GROUPS ===\n")
# cat("Total rows:", nrow(disj_grouped), "\n")
# cat("Total participants:", length(unique(disj_grouped$id)), "\n")
# print(disj_grouped[, .(n_rows = .N, participants = length(unique(id))), by = .(group, listtype)])

# ==== 1. CORRELATIONS PREDICTING PERFORMANCE (ALL 4 CONDITIONS) ====

# Performance metrics by condition and participant
performance_by_condition <- dat_long[cue_valid == "yes", .(
  n_valid = .N,
  overlap_proportion = sum(overlap == "yes") / .N,
  soft_switch_rate = sum(soft_switch == "yes") / (.N - 1),
  hard_switch_rate = sum(hard_switch == "yes") / (.N - 1),
  overall_switch_rate = sum(any_switch == "yes") / (.N - 1)
), by = .(id, condition)]

# Correlations for each condition
cat("=== PERFORMANCE CORRELATIONS BY CONDITION ===\n")
for(cond in unique(performance_by_condition$condition)) {
  cat(paste("\n", cond, "Condition:\n"))
  cond_data <- performance_by_condition[condition == cond]
  
  # Skip overlap for single-cue conditions
  if(cond %in% c("semantic", "phonological")) {
    cat("Soft switch ↔ Performance:", cor(cond_data$n_valid, cond_data$soft_switch_rate, use = "complete.obs"), "\n")
    cat("Hard switch ↔ Performance:", cor(cond_data$n_valid, cond_data$hard_switch_rate, use = "complete.obs"), "\n")
    cat("Overall switch ↔ Performance:", cor(cond_data$n_valid, cond_data$overall_switch_rate, use = "complete.obs"), "\n")
  } else {
    cat("Overlap ↔ Performance:", cor(cond_data$n_valid, cond_data$overlap_proportion, use = "complete.obs"), "\n")
    cat("Soft switch ↔ Performance:", cor(cond_data$n_valid, cond_data$soft_switch_rate, use = "complete.obs"), "\n")
    cat("Hard switch ↔ Performance:", cor(cond_data$n_valid, cond_data$hard_switch_rate, use = "complete.obs"), "\n")
  }
}

# semantic and phonological = NA because no variability (they have no switching or overlap)

# disjunctive condition = overlap and performance, -.35, soft switch and performance -.03, hard switch and performance -.09.; overlap hurts performance (fewer words), switching doesn't matter (soft/hard switches don't predict performance)

# conjunctive condition = overlap helps performance (.11), switching doesnt matter (soft = -.08, hard = .10)


# do people who do well in one condition also do well in another? 

# Create wide format for within-subject correlations
wide_performance <- dcast(performance_by_condition, id ~ condition, value.var = "n_valid")

# Correlations between conditions
cat("Performance correlations between conditions:\n")
condition_corrs <- cor(wide_performance[, .(semantic, phonological, disjunctive, conjunctive)], use = "complete.obs")
print(condition_corrs)


# mixed models

# Prepare data for mixed models
mixed_data <- merge(
  performance_by_condition[condition == "disjunctive", .(id, disj_perf = n_valid)],
  performance_by_condition[condition == "semantic", .(id, semantic_perf = n_valid)],
  by = "id"
)
mixed_data <- merge(
  mixed_data,
  performance_by_condition[condition == "phonological", .(id, phono_perf = n_valid)],
  by = "id"
)
mixed_data <- merge(
  mixed_data,
  performance_by_condition[condition == "conjunctive", .(id, conj_perf = n_valid)],
  by = "id"
)

# Add block information (using groups from earlier)
if(!"group" %in% names(dat_long)) {
  dat_long[, group := fifelse(
    listtype %in% c("disj_1", "disj_2"), "block_1",
    fifelse(listtype %in% c("disj_3", "disj_4"), "block_2",
    fifelse(listtype %in% c("disj_5", "disj_6"), "block_3", "block_4"))
  )]
}

# Get block for each participant (from disjunctive condition)
participant_blocks <- dat_long[condition == "disjunctive", .(block = unique(group)), by = id]
mixed_data <- merge(mixed_data, participant_blocks, by = "id")

# Mixed models
# Model 1: performance with Disjunctive ~ Semantic + (1|block)
model1 <- lmer(disj_perf ~ semantic_perf + (1 | block), data = mixed_data)
print(summary(model1))
# aka can we predict how well someone will do in the disj condition based on how the ydid on semantic? 
# better semantic performance -> better disj performance, *** 
# for every 1 word increase in semantic, disj increases by .51 words 


# Model 2: Disjunctive ~ Phonological + (1|block)
model2 <- lmer(disj_perf ~ phono_perf + (1 | block), data = mixed_data)
print(summary(model2))
# can we predict how well someone will do in disj based on phhonological perf?
# better phono performance -> better disj performance, ***
# for every 1 word increase in phohono, disj increases by .66 words


# Model 3: Disjunctive ~ Conjunctive + (1|block)
model3 <- lmer(disj_perf ~ conj_perf + (1 | block), data = mixed_data)
print(summary(model3))
# disj based on conj?
# better conj -> better disj performance ***
# for every 1 word increase in conj, disj increases .45

# Model 4: All predictors together
model4 <- lmer(disj_perf ~ semantic_perf + phono_perf + conj_perf + (1 | block), data = mixed_data)
print(summary(model4))
# what is the significance of phono here...?


# within subject
# Paired t-tests
cat("Semantic vs Disjunctive:\n")
print(t.test(wide_performance$semantic, wide_performance$disjunctive, paired = TRUE))
# do people perform differently in semantic vs disjunctive?
# semantic > disj, -6.27 less in disj than semantic


cat("\nPhonological vs Disjunctive:\n")
print(t.test(wide_performance$phonological, wide_performance$disjunctive, paired = TRUE))
# do ppl perform diff in phon vs disj?
# yes, but ns. 

cat("\nConjunctive vs Disjunctive:\n")
print(t.test(wide_performance$conjunctive, wide_performance$disjunctive, paired = TRUE))
# do ppl perform diff in conj than disj?
# yes, disj > conj, -16 words for conj compared to disj