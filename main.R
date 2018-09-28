# load libraries
libs <- c("magrittr", "Publish", "tidyverse",
          "lme4", "sjstats", "TMB")
lapply(libs, require, character.only = TRUE)

# load homemade functions
source("homemade_funs.R")

#### Dig&Wor ####
### clean df span num&words ###
# load df
span_nw_df <- read.delim("Span Test schools_num&words_24_03_18_1 May 2018_16.04.csv", sep=",", 
                         header=F, stringsAsFactors = F, check.names = F)

# set colnames
colnames(span_nw_df) <- span_nw_df[1,]

# filter out trials
span_nw_df %<>% 
  filter(id != "prova") %>%
  filter(id != "30")

# filter out 2 row with not useful questionnaire ids
span_nw_df <- span_nw_df[-3,]

# join header with first coloumn
for (i in seq_along(span_nw_df)) {
  span_nw_df[1,i] <- paste(span_nw_df[1:2,i], collapse = "&")
} ; rm(i)

# set new colnames and delete 1st and 2nd row with repetition of colnames
colnames(span_nw_df) <- span_nw_df[1,]
span_nw_df <- span_nw_df[-c(1,2),]

# select coloums of interest
span_nw_df %<>% 
  select(`StartDate&Start Date`, `id&id`:`FL_5_DO&FL_5 - Block Randomizer - Display Order`)
  
# convert Start date variable in date format
span_nw_df$`StartDate&Start Date` <- as.Date(span_nw_df$`StartDate&Start Date`)

# create new variable joining age month with age year, and convert to date format
span_nw_df %<>%
  unite("age", c("age month&age month", "age year&age year"), sep = " ")

span_nw_df$age <- as.Date(paste("15", span_nw_df$age, sep = " "), "%d %m %Y")

# convert age in age in months
span_nw_df$age_month <-  abs(mondf(span_nw_df$`StartDate&Start Date`, span_nw_df$age))

# filter df for new coloums 
span_nw_df %<>% 
  select(id = `id&id`, age_month, gender = `gender&gender`, 
         list_ord = `FL_5_DO&FL_5 - Block Randomizer - Display Order`,
         `list1 i1_1&seq: 2 9 - finish`:`list2 i16_19&seq: food house water bear bed cup school door boat - other`) %>%
  mutate(id = as.numeric(id))

# create an age table as a reference (for later)
age_table <- span_nw_df %>%
  select(id, age_month) %>%
  arrange(id)

# combine all lists coloums in a sigle one (from wide to long format df)
span_nw_df %<>%
  gather(item, resp, 
         `list1 i1_1&seq: 2 9 - finish`:`list2 i16_19&seq: food house water bear bed cup school door boat - other`)

# separate item coloum in multiple coloums
span_nw_df$list <- gsub("^list", "", str_extract(span_nw_df$item, "^list[12]{1}"))
span_nw_df$list <- ifelse(span_nw_df$list == "1", "digits", "words")
span_nw_df$seq_num <- gsub("^i","", str_extract(span_nw_df$item, "i[0-9]{1,2}"))
span_nw_df$num <- gsub("^- ", "", str_extract(span_nw_df$item, "-.*$"))
span_nw_df$seq <- gsub(" -$", "", gsub("^seq: ", "", str_extract(span_nw_df$item, "seq:.*-")))
span_nw_df$len_seq <- sapply(span_nw_df$seq, function(x) length(str_split(x, " ")[[1]]))

# get rid of variable item and rearrange coloums order
span_nw_df %<>%
  select(id:list_ord, list, seq_num, seq, len_seq, num, resp)

### create reference table with correct responses ###
corr_nw_table <- span_nw_df %>%
  distinct(seq_num, seq, num) %>%
  filter(num != "other") %>%
  group_by(seq) %>%
  mutate(corr_resp = c(length(seq), 1:(length(seq)-1)))

# check if the participant gave the correct response of each stimulus
span_nw_df %<>% 
  inner_join(corr_nw_table, span_nw_df, by = c("seq_num", "seq", "num")) %>%
  mutate(corr_resp = resp == corr_resp)

# arrange df by id
span_nw_df %<>%
  arrange(id)

# create a variable of correct list recall (for span size and span total)
span_nw_df %<>%
  group_by(id, seq) %>%
  mutate(corr_list = ifelse(sum(corr_resp) == length(corr_resp), TRUE, FALSE))

# span size (length of longest list accurately recalled)
span_size_nw <- span_nw_df %>%
  filter(corr_list == TRUE) %>%
  group_by(id, list) %>%
  summarize(span_size = len_seq[length(len_seq)])

# save raw span size digits/words for scatterplot by age
span_size_nw_raw <- span_size_nw %>%
  inner_join(age_table, span_size_nw, by = c("id"))
  
`CI_95%` <- ci.mean(span_size~list, data=span_size_nw, normal = F)

span_size_nw %<>%
  group_by(list) %>%
  summarize(M = mean(span_size), 
            SD = sd(span_size)) %>%
  mutate(`lower_CI_95%` = `CI_95%`$lower,
         `upper_CI_95%` = `CI_95%`$upper) ; rm(`CI_95%`)

# span total (number of lists accurately recalled)
span_tot_nw <- span_nw_df %>%
  filter(corr_list == TRUE) %>%
  group_by(id, list) %>%
  summarize(span_tot = length(unique(seq)))

# save raw span tot digits/words for scatterplot by age
span_tot_nw_raw <- span_tot_nw %>%
  inner_join(age_table, span_tot_nw, by = c("id"))

`CI_95%` <- ci.mean(span_tot~list, data=span_tot_nw, normal = F)

span_tot_nw %<>%
  group_by(list) %>%
  summarize(M = mean(span_tot), 
            SD = sd(span_tot)) %>%
  mutate(`lower_CI_95%` = `CI_95%`$lower,
         `upper_CI_95%` = `CI_95%`$upper) ; rm(`CI_95%`)

# digit span advantage (digit span size minus word span size) as a function of span length
span_size_diff_nw <- span_nw_df %>%
  filter(corr_list == TRUE) %>%
  group_by(id, list) %>%
  summarize(span_size = len_seq[length(len_seq)]) %>%
  spread(list, span_size) %>%
  mutate(diff = digits-words) 

span_size_diff_nw_N <- span_size_diff_nw %>%
  group_by(digits) %>%
  summarize(n())

`CI_95%` <- ci.mean(diff~digits, data=span_size_diff_nw, normal = F)

span_size_diff_nw %<>%
  group_by(digits) %>%
  summarize(M = mean(diff), 
            SD = sd(diff)) %>%
  mutate(`lower_CI_95%` = `CI_95%`$lower,
         `upper_CI_95%` = `CI_95%`$upper,
         N = span_size_diff_nw_N$`n()`) ; rm(`CI_95%`)

#### Mixed ####
# load df
span_mix_df <- read.delim("Span Test schools_mixed_24_03_18_1 May 2018_16.08.csv", sep=",", header=F, stringsAsFactors = F, check.names = F)

# set colnames
colnames(span_mix_df) <- span_mix_df[1,]

# filter out trials
span_mix_df %<>% 
  filter(!id %in% c("prova", "poo")) %>%
  filter(id != "30")

# change wrong id (double 23)
span_mix_df$id[which(span_mix_df$`age month` == "05" & span_mix_df$id == "23")] <- "24"

# filter out 2 row with not useful questionnaire ids
span_mix_df <- span_mix_df[-3,]

# join header with first coloumn
for (i in seq_along(span_mix_df)) {
  span_mix_df[1,i] <- paste(span_mix_df[1:2,i], collapse = "&")
} ; rm(i)

# set new colnames and delete 1st and 2nd row with repetition of colnames
colnames(span_mix_df) <- span_mix_df[1,]
span_mix_df <- span_mix_df[-c(1,2),]

# select coloums of interest
span_mix_df %<>% 
  select(`StartDate&Start Date`, `id&id`:`FL_5_DO&FL_5 - Block Randomizer - Display Order`)

# convert Start date variable in date format
span_mix_df$`StartDate&Start Date` <- as.Date(span_mix_df$`StartDate&Start Date`)

# create new variable joining age month with age year, and convert to date format
span_mix_df %<>%
  unite("age", c("age month&age month", "age year&age year"), sep = " ")

span_mix_df$age <- as.Date(paste("15", span_mix_df$age, sep = " "), "%d %m %Y")

# convert age in age in months
span_mix_df$age_month <-  abs(mondf(span_mix_df$`StartDate&Start Date`, span_mix_df$age))

# filter df for new coloums 
span_mix_df %<>% 
  select(id = `id&id`, age_month, gender = `gender&gender`, 
         list_ord = `FL_5_DO&FL_5 - Block Randomizer - Display Order`,
         `list3 i1_1&seq: 2 water - finish`:`list4 i16_19&seq: school water 7 cup 6 9 8 house bed - other`) %>%
  mutate(id = as.numeric(id))

# match age with nw
span_mix_df$age_month <- age_table$age_month

# combine all lists coloums in a sigle one (from wide to long format df)
span_mix_df %<>%
  gather(item, resp, 
         `list3 i1_1&seq: 2 water - finish`:`list4 i16_19&seq: school water 7 cup 6 9 8 house bed - other`)
  
# separate item coloum in multiple coloums
span_mix_df$list <- gsub("^list", "", str_extract(span_mix_df$item, "^list[34]{1}"))
span_mix_df$list <- ifelse(span_mix_df$list == "3", "mixed1", "mixed2")
span_mix_df$seq_num <- gsub("^i","", str_extract(span_mix_df$item, "i[0-9]{1,2}"))
span_mix_df$num <- gsub("^- ", "", str_extract(span_mix_df$item, "-.*$"))
span_mix_df$seq <- gsub(" -$", "", gsub("^seq: ", "", str_extract(span_mix_df$item, "seq:.*-")))
span_mix_df$len_seq <- sapply(span_mix_df$seq, function(x) length(str_split(x, " ")[[1]]))

# get rid of variable item and rearrange coloums order
span_mix_df %<>%
  select(id:list_ord, list, seq_num, seq, len_seq, num, resp)

### create reference table with correct responses ###
corr_mix_table <- span_mix_df %>%
  distinct(seq_num, seq, num) %>%
  filter(num != "other") %>%
  group_by(seq) %>%
  mutate(corr_resp = c(length(seq), 1:(length(seq)-1)))

# check if the participant gave the correct response of each stimulus
span_mix_df %<>% 
  inner_join(corr_mix_table, span_mix_df, by = c("seq_num", "seq", "num")) %>%
  mutate(corr_resp = resp == corr_resp)

# arrange df by id
span_mix_df %<>%
  arrange(id)

# create a variable of correct list recall (for span size and span total)
span_mix_df %<>%
  group_by(id, seq) %>%
  mutate(corr_list = ifelse(sum(corr_resp) == length(corr_resp), TRUE, FALSE))

# span size (length of longest list accurately recalled)
span_size_mix <- span_mix_df %>%
  filter(corr_list == TRUE) %>%
  group_by(id, list) %>%
  summarize(span_size = len_seq[length(len_seq)]) %>%
  inner_join(age_table, span_size_mix, by = c("id")) %>%
  mutate(age_year = round(age_month/12,0))

# list unique age groups in years
age_table_unique <- sort(unique(round(age_table$age_month/12,0)))

span_size_mix_raw <- span_size_mix

span_size_mix67 <- span_size_mix %>%
  filter(age_year %in% c(6,7))

span_size_mix8 <- span_size_mix %>%
  filter(age_year %in% c(8))

span_size_mix1011 <- span_size_mix %>%
  filter(age_year %in% c(10,11))


# create a function to create a span table (size or tot) to use it for different age groups
span_table <- function(df, span_var) {
  if (span_var == "size") {
    `CI_95%` <- ci.mean(span_size~list, data=df, normal = F)
    
    df %<>%
      group_by(list) %>%
      summarize(M = mean(span_size), 
                SD = sd(span_size)) %>%
      mutate(`lower_CI_95%` = `CI_95%`$lower,
             `upper_CI_95%` = `CI_95%`$upper) ; rm(`CI_95%`)
    df
  } else if (span_var == "tot") {
    `CI_95%` <- ci.mean(span_tot~list, data=df, normal = F)
    
    df %<>%
      group_by(list) %>%
      summarize(M = mean(span_tot), 
                SD = sd(span_tot)) %>%
      mutate(`lower_CI_95%` = `CI_95%`$lower,
             `upper_CI_95%` = `CI_95%`$upper) ; rm(`CI_95%`)
    df
  } else {
    stop("you can only choose between the variables: size, tot." )
  }
}

span_size_mix <- span_table(span_size_mix, "size")
span_size_mix67 <- span_table(span_size_mix67, "size")
span_size_mix8 <- span_table(span_size_mix8, "size")
span_size_mix1011 <- span_table(span_size_mix1011, "size")

# span total (number of lists accurately recalled)
span_tot_mix <- span_mix_df %>%
  filter(corr_list == TRUE) %>%
  group_by(id, list) %>%
  summarize(span_tot = length(unique(seq))) %>%
  inner_join(age_table, span_tot_mix, by = c("id")) %>%
  mutate(age_year = round(age_month/12,0))

span_tot_mix_raw <- span_tot_mix

span_tot_mix67 <- span_tot_mix %>%
  filter(age_year %in% c(6,7))

span_tot_mix8 <- span_tot_mix %>%
  filter(age_year %in% c(8))

span_tot_mix1011 <- span_tot_mix %>%
  filter(age_year %in% c(10,11))

span_tot_mix <- span_table(span_tot_mix, "tot")
span_tot_mix67 <- span_table(span_tot_mix67, "tot")
span_tot_mix8 <- span_table(span_tot_mix8, "tot")
span_tot_mix1011 <- span_table(span_tot_mix1011, "tot")

# digit span advantage (digit span size minus word span size) as a function of span length
span_size_diff_mix <- span_mix_df %>%
  filter(corr_list == TRUE) %>%
  group_by(id, list) %>%
  summarize(span_size = len_seq[length(len_seq)]) %>%
  spread(list, span_size) %>%
  mutate(diff = mixed1-mixed2) 

span_size_diff_mix_N <- span_size_diff_mix %>%
  group_by(mixed1) %>%
  summarize(n())

`CI_95%` <- ci.mean(diff~mixed1, data=span_size_diff_mix, normal = F)

span_size_diff_mix %<>%
  group_by(mixed1) %>%
  summarize(M = mean(diff), 
            SD = sd(diff)) %>%
  mutate(`lower_CI_95%` = `CI_95%`$lower,
         `upper_CI_95%` = `CI_95%`$upper,
         N = span_size_diff_mix_N$`n()`) ; rm(`CI_95%`)

# the proportion of accurate recall of isolated digits, isolated words, digit pairs, 
# and word pairs for the final four span lists of each participant. (not considering position)

# filter for the last 4 lists recalled for each participant
# create a reference table for responses
mix_prop_table <- span_mix_df %>%
  filter((age_month/12) > 0 & (age_month/12) < 15) %>% # QUESTO PER MANIPOLARE L'ETà
  filter(resp != "") %>% # CAMBIARE QUI SE SI VUOLE PRENDERE LE ULTIME REALI 4 LISTE
  mutate(seq_num = as.numeric(seq_num)) %>%
  group_by(id, list) %>%
  filter(seq_num %in% unique(seq_num)[(ifelse(length(unique(seq_num))-3 < 0, 1, length(unique(seq_num))-3)):
                              length(unique(seq_num))])

# create 4 variables for isolated digits, words and pairs
span_mix_prop_df <- mix_prop_table %>%
  select(id:len_seq) %>%
  distinct(seq, .keep_all = T)

for (i in seq_along(span_mix_prop_df$seq)) {
  seq <- unlist(str_match_all(span_mix_prop_df$seq[i], "[0-9 ]*"))
  seq <- seq[!seq %in% c("", " ")]
  seq <- gsub(" $", "", gsub("^ ", "", seq))
  seq <- str_split(seq, " ")
  if (length(seq[lengths(seq) > 2]) > 0) {
    first_pair <- list(seq[lengths(seq) > 2][[1]][1:2])
    second_pair <- list(seq[lengths(seq) > 2][[1]][2:3])
    
    seq <- c(seq[lengths(seq) <= 2], first_pair, second_pair)
  }
  
  span_mix_prop_df$single_d[[i]] <- if (length(seq[lengths(seq) == 1]) == 0) NA else seq[lengths(seq) == 1]
  span_mix_prop_df$pair_d[[i]] <- if (length(seq[lengths(seq) == 2]) == 0) NA else seq[lengths(seq) == 2]
} ; rm(seq, first_pair, second_pair, i)

span_mix_prop_df$single_w <- list(rep(NA, nrow(span_mix_prop_df)))
span_mix_prop_df$pair_w <- list(rep(NA, nrow(span_mix_prop_df)))
for (i in seq_along(span_mix_prop_df$seq)) {
  seq <- unlist(str_match_all(span_mix_prop_df$seq[i], "[a-z ]*"))
  seq <- seq[!seq %in% c("", " ")]
  seq <- gsub(" $", "", gsub("^ ", "", seq))
  seq <- str_split(seq, " ")
  if (length(seq[lengths(seq) > 2]) > 0) {
    first_pair <- list(seq[lengths(seq) > 2][[1]][1:2])
    second_pair <- list(seq[lengths(seq) > 2][[1]][2:3])
    
    seq <- c(seq[lengths(seq) <= 2], first_pair, second_pair)
  }
  
  span_mix_prop_df$single_w[[i]] <- if (length(seq[lengths(seq) == 1]) == 0) NA else seq[lengths(seq) == 1]
  span_mix_prop_df$pair_w[[i]] <- if (length(seq[lengths(seq) == 2]) == 0) NA else seq[lengths(seq) == 2]
} ; rm(seq, first_pair, second_pair, i)

# filter out stimuli with a position higher than finish in each sequence
mix_prop_table %<>%
  mutate(resp = as.numeric(resp)) %>%
  group_by(id, list, seq_num) %>%
  filter(resp < resp[1])

# assign position (on empty position) to each isolated/pair element in spa_mix_prop_df
span_mix_prop_df %<>%
  mutate(single_d_resp = single_d,
         pair_d_resp = pair_d,
         single_w_resp = single_w,
         pair_w_resp = pair_w)

for (vars in c("single_d_resp", "pair_d_resp", "single_w_resp", "pair_w_resp")) {
  for (i in seq_along(span_mix_prop_df[[vars]])) {
    for (j in seq_along(span_mix_prop_df[[vars]][[i]])) {
      for (z in seq_along(span_mix_prop_df[[vars]][[i]][[j]])) {
         resp <- mix_prop_table$resp[which(mix_prop_table$id == span_mix_prop_df$id[i] &
                                                                             mix_prop_table$list == span_mix_prop_df$list[i] &
                                                                             mix_prop_table$seq == span_mix_prop_df$seq[i] &
                                                                             mix_prop_table$num == span_mix_prop_df[[vars]][[i]][[j]][z])]
         if (length(resp) == 0) {
           span_mix_prop_df[[vars]][[i]][[j]][z] <- NA
         } else {
           span_mix_prop_df[[vars]][[i]][[j]][z] <- resp
         }
      }
    }
  }
} ; rm(vars, i, j, z, resp)

# replace pair not recalled in the right order with NA
for (vars in c("pair_d_resp", "pair_w_resp")) {
  for (i in seq_along(span_mix_prop_df[[vars]])) {
    for (j in seq_along(span_mix_prop_df[[vars]][[i]])) {
        check_order <- as.numeric(span_mix_prop_df[[vars]][[i]][[j]][1]) - as.numeric(span_mix_prop_df[[vars]][[i]][[j]][2])
        
        if (is.na(check_order) | check_order != -1) {
          span_mix_prop_df[[vars]][[i]][[j]] <- NA
        }
                                                                         
    }
  }
} ; rm(vars, i, j, check_order)

span_mix_prop <- span_mix_prop_df %>%
  #ungroup() %>% # TOGLIERE QUESTO PER MANTENERE I GRUPPI E POI CALCOLARE PER OGNI ID, FARE MEDIA E CI
  select(single_d:pair_w,
         single_d_resp:pair_w_resp)

# replace positional values with TRUE and FALSE
for (vars in c("single_d", "pair_d", "single_w", "pair_w",
               "single_d_resp", "pair_d_resp", "single_w_resp", "pair_w_resp")) {
  for (i in seq_along(span_mix_prop[[vars]])) {
    for (j in seq_along(span_mix_prop[[vars]][[i]])) {
      stimulus <- span_mix_prop[[vars]][[i]][[j]]
      
      if (is.na(sum(nchar(stimulus)))) {
        span_mix_prop[[vars]][[i]][[j]] <- F
      } else {
        span_mix_prop[[vars]][[i]][[j]] <- T
      }
    }
  }
} ; rm(stimulus, vars, i, j)

# percentages of isolated and pairs stimuli
span_mix_prop %<>%
  group_by(id) %>%
  summarize(single_d = sum(unlist(single_d_resp))/sum(unlist(single_d)),
            single_w = sum(unlist(single_w_resp))/sum(unlist(single_w)),
            pair_d = sum(unlist(pair_d_resp))/sum(unlist(pair_d)),
            pair_w = sum(unlist(pair_w_resp))/sum(unlist(pair_w))) %>%
  inner_join(age_table, span_mix_prop, by = c("id"))

span_mix_prop57 <- span_mix_prop %>%
  filter(age_month/12 < 7)

span_mix_prop79 <- span_mix_prop %>%
  filter(age_month/12 > 7 & age_month/12 < 9)

span_mix_prop911 <- span_mix_prop %>%
  filter(age_month/12 > 9)


span_mix_prop_all <- data.frame(Stimulus = c("Isolated digit", "Isolated word",
                                             "Digit pair", "Digit word"),
                                Mean = c(mean(span_mix_prop$single_d), mean(span_mix_prop$single_w),
                                         mean(span_mix_prop$pair_d), mean(span_mix_prop$pair_w)),
                                SD = c(sd(span_mix_prop$single_d), sd(span_mix_prop$single_w),
                                       sd(span_mix_prop$pair_d), sd(span_mix_prop$pair_w)),
                                `Lower_CI_95%` =   c(ci.mean(span_mix_prop$single_d)$lower,
                                                     ci.mean(span_mix_prop$single_w)$lower,
                                                     ci.mean(span_mix_prop$pair_d)$lower,
                                                     ci.mean(span_mix_prop$pair_w)$lower),
                                `Upper_CI_95%` =   c(ci.mean(span_mix_prop$single_d)$upper,
                                                     ci.mean(span_mix_prop$single_w)$upper,
                                                     ci.mean(span_mix_prop$pair_d)$upper,
                                                     ci.mean(span_mix_prop$pair_w)$upper), check.names = F)
  
span_mix_prop57 <- data.frame(Stimulus = c("Isolated digit", "Isolated word",
                                             "Digit pair", "Digit word"),
                                Mean = c(mean(span_mix_prop57$single_d), mean(span_mix_prop57$single_w),
                                         mean(span_mix_prop57$pair_d), mean(span_mix_prop57$pair_w)),
                                SD = c(sd(span_mix_prop57$single_d), sd(span_mix_prop57$single_w),
                                       sd(span_mix_prop57$pair_d), sd(span_mix_prop57$pair_w)),
                                `Lower_CI_95%` =   c(ci.mean(span_mix_prop57$single_d)$lower,
                                                     ci.mean(span_mix_prop57$single_w)$lower,
                                                     ci.mean(span_mix_prop57$pair_d)$lower,
                                                     ci.mean(span_mix_prop57$pair_w)$lower),
                                `Upper_CI_95%` =   c(ci.mean(span_mix_prop57$single_d)$upper,
                                                     ci.mean(span_mix_prop57$single_w)$upper,
                                                     ci.mean(span_mix_prop57$pair_d)$upper,
                                                     ci.mean(span_mix_prop57$pair_w)$upper), check.names = F)
span_mix_prop79 <- data.frame(Stimulus = c("Isolated digit", "Isolated word",
                                             "Digit pair", "Digit word"),
                                Mean = c(mean(span_mix_prop79$single_d), mean(span_mix_prop79$single_w),
                                         mean(span_mix_prop79$pair_d), mean(span_mix_prop79$pair_w)),
                                SD = c(sd(span_mix_prop79$single_d), sd(span_mix_prop79$single_w),
                                       sd(span_mix_prop79$pair_d), sd(span_mix_prop79$pair_w)),
                                `Lower_CI_95%` =   c(ci.mean(span_mix_prop79$single_d)$lower,
                                                     ci.mean(span_mix_prop79$single_w)$lower,
                                                     ci.mean(span_mix_prop79$pair_d)$lower,
                                                     ci.mean(span_mix_prop79$pair_w)$lower),
                                `Upper_CI_95%` =   c(ci.mean(span_mix_prop79$single_d)$upper,
                                                     ci.mean(span_mix_prop79$single_w)$upper,
                                                     ci.mean(span_mix_prop79$pair_d)$upper,
                                                     ci.mean(span_mix_prop79$pair_w)$upper), check.names = F)
span_mix_prop911 <- data.frame(Stimulus = c("Isolated digit", "Isolated word",
                                             "Digit pair", "Digit word"),
                                Mean = c(mean(span_mix_prop911$single_d), mean(span_mix_prop911$single_w),
                                         mean(span_mix_prop911$pair_d), mean(span_mix_prop911$pair_w)),
                                SD = c(sd(span_mix_prop911$single_d), sd(span_mix_prop911$single_w),
                                       sd(span_mix_prop911$pair_d), sd(span_mix_prop911$pair_w)),
                                `Lower_CI_95%` =   c(ci.mean(span_mix_prop911$single_d)$lower,
                                                     ci.mean(span_mix_prop911$single_w)$lower,
                                                     ci.mean(span_mix_prop911$pair_d)$lower,
                                                     ci.mean(span_mix_prop911$pair_w)$lower),
                                `Upper_CI_95%` =   c(ci.mean(span_mix_prop911$single_d)$upper,
                                                     ci.mean(span_mix_prop911$single_w)$upper,
                                                     ci.mean(span_mix_prop911$pair_d)$upper,
                                                     ci.mean(span_mix_prop911$pair_w)$upper), check.names = F)





#### New dfs ####
# import new tidy df
span_nw_df_2 <- "Digit Span and Word Span.csv" %>%
  read_csv() %>%
  select(ID:Sex, `Longest Digit`, `Longest Word`) %T>%
  {
    colnames(.) <- c("id", "age", "sex", "digits", "words")
  } %>%
  gather(stimulus_type, span_size, c(digits, words))

# span size (length of longest list accurately recalled)
`CI_95%` <- ci.mean(span_size~stimulus_type, data=span_nw_df_2, normal = F)

span_size_nw_2 <- span_nw_df_2 %>%
  rename(`Stimulus type` = stimulus_type) %>%
  group_by(`Stimulus type`) %>%
  summarize(M = mean(span_size), 
            SD = sd(span_size)) %>%
  mutate(`lower_CI_95%` = `CI_95%`$lower,
         `upper_CI_95%` = `CI_95%`$upper) ; rm(`CI_95%`)

# joined df
span_size_df_joined <- span_size_nw_raw %T>%
{
  colnames(.) <- c("id", "stimulus_type", "span_size", "age")
} %>%
  select(id, age, stimulus_type, span_size) %>%
  ungroup() %>%
  mutate(id = as.character(id)) %>%
    rbind(span_nw_df_2 %>%
            select(-sex))

# span size (length of longest list accurately recalled)
`CI_95%` <- ci.mean(span_size~stimulus_type, data=span_size_df_joined, normal = F)

span_size_nw_joined <- span_size_df_joined %>%
  rename(`Stimulus type` = stimulus_type) %>%
  group_by(`Stimulus type`) %>%
  summarize(M = mean(span_size), 
            SD = sd(span_size)) %>%
  mutate(`lower_CI_95%` = `CI_95%`$lower,
         `upper_CI_95%` = `CI_95%`$upper) ; rm(`CI_95%`)