library(data.table)

rm(list = ls())

clean <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("\\s+", " ", x)
  x
}

dat <- fread("data2run.csv")

cols <- c(
  paste0("semantic_",1:8),
  paste0("phono_",1:8),
  paste0("disj_",1:8),
  paste0("conj_",1:8)
)

# wide to long

long_lists <- melt(
  dat,
  id.vars = "id",
  measure.vars = cols,
  variable.name = "listtype",
  value.name   = "raw_string"
)

long_lists[, c("list","listnum") := tstrsplit(listtype, "_")]
long_lists[, listnum := as.integer(listnum)]

long_lists <- long_lists[
  !is.na(raw_string) & nzchar(trimws(raw_string))
]

# parse func for rt
parse_list <- function(x) {
  parts <- trimws(strsplit(x, ",")[[1]])
  ts  <- suppressWarnings(as.numeric(parts[seq(1, length(parts), 2)]))
  rsp <- parts[seq(2, length(parts), 2)]

  ts  <- ts[!is.na(ts)]
  rsp <- rsp[seq_len(length(ts) - 1)]

  data.table(
    itemnum =seq_along(rsp),
    rt=diff(ts),
    response=rsp
  )
}

dat_long <- long_lists[
  , parse_list(raw_string),
  by=.(id, listtype, list, listnum)
]

dat_long[, response:= trimws(response)]
dat_long[, response_norm:= clean(response)]

# categories and letters

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

# filtered dictionary

cat_dict <- fread("category_dictionary_pruned.csv")
cat_dict[, category:= clean(category)]
cat_dict[, response_norm:= clean(response_norm)]

# semantic/phono/overlap

dat_long[, semantic := "no"]
dat_long[cat_dict, semantic := "yes",
         on =.(category, response_norm)]

dat_long[, phonological:= fifelse(!is.na(letter) & startsWith(response_norm, letter),"yes","no"
)]

dat_long[, overlap:= fifelse(semantic=="yes"& phonological=="yes","yes","no")]

# switch

dat_long[, state := fcase(semantic=="yes" & phonological=="no",  "B",semantic=="no"  & phonological=="yes", "A",semantic=="yes" & phonological=="yes", "AB",default = "0")]

setorder(dat_long, id, listtype, itemnum)

dat_long[, state_valid := fifelse(state!="0", state, NA_character_)]

prev_dt <- dat_long[state_valid %in% c("A","B","AB"),.(id, listtype, itemnum, prev_state = shift(state_valid)),by = .(id, listtype)]

dat_long[, prev_state := NA_character_]
dat_long[prev_dt, prev_state := i.prev_state,on =.(id, listtype, itemnum)]

dat_long[, hard_switch := fifelse(prev_state %in% c("A","B") & state_valid %in% c("A","B") & prev_state != state_valid,"yes","no")]

dat_long[, soft_switch := fifelse((prev_state %in% c("A","B") & state_valid=="AB") |(prev_state=="AB"& state_valid %in% c("A","B")),"yes","no")]

dat_long[, any_switch := fifelse(hard_switch=="yes" | soft_switch=="yes","yes","no")]

dat_long[itemnum==1,`:=`(hard_switch="no",soft_switch="no",any_switch ="no")]

# # clean up~

dat_long[, c("state","state_valid","prev_state","response_norm") := NULL]
setcolorder(dat_long, c("id","listtype","list","listnum","category","letter","itemnum","response","rt","semantic","phonological","overlap","hard_switch","soft_switch","any_switch"))

dat_long[, cue_combo := fcase(semantic=="yes" & phonological=="no",  "semantic_only",semantic=="no"  & phonological=="yes", "letter_only",semantic=="yes" & phonological=="yes", "both",default = "neither")]

dat_long[, cue_combo := factor(cue_combo,levels = c("semantic_only","letter_only","both","neither"))]

# this is for raw counts
# valid = fits at least one cue
dat_long[, cue_valid := fifelse(semantic=="yes" | phonological=="yes", "yes","no")]

# individual ocounts
counts_id <- dat_long[cue_valid=="yes",.(n_valid = .N,n_overlap =sum(overlap=="yes", na.rm=TRUE),n_soft  =sum(soft_switch=="yes", na.rm=TRUE),n_hard  =sum(hard_switch=="yes", na.rm=TRUE),n_any=sum(any_switch=="yes",  na.rm=TRUE)),by=id]

counts_id[]

dat_long[, condition := factor(list, levels = c("semantic","phono","disj","conj"),labels = c("semantic","phonological","disjunctive","conjunctive"))]

# separate dt cuz its easier to understand to me with valid responses, soft switch, hard switch, and any switch
# gonna look at overlap which is both cues and then sfotcues will be ...whether they go from semantic & phono --> semantic or phono
# im a little shaky on this but it makes sense in my head <:(
counts_id_cond <- dat_long[cue_valid=="yes",.(n_valid = .N,n_overlap= sum(overlap=="yes", na.rm=TRUE), n_soft= sum(soft_switch=="yes", na.rm=TRUE),n_hard= sum(hard_switch=="yes", na.rm=TRUE),n_any = sum(any_switch=="yes",  na.rm=TRUE)),by =.(id, condition)]

counts_id_cond[]


## check <- dat_long[, .(category, letter, response, semantic, phonological, overlap, hard_switch, soft_switch)]