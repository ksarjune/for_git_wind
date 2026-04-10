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
