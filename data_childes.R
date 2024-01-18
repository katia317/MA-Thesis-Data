library(childesr)
library(dplyr)
# Providence Corpus, 6 children (3 girls, 3 boys)
pro <- get_utterances(role = "Mother", age = c(12,36), corpus = "Providence")
pro_utterance <- select(pro, gloss, target_child_sex, part_of_speech)

# Gleason Corpus, 7 children (3 girls, 4 boys)
# Laurel, Martin, Nanette, Patricia, Richard, Victor, William
gleason <- get_utterances(role = "Mother", age = c(12,36), corpus = "Gleason")
gle_utterance <- select(gleason, gloss, target_child_sex, part_of_speech)

# McCune Corpus, 10 children
mcc <- get_utterances(role = "Mother", age = c(12,36), corpus = "McCune")
mcc_utterance <- select(mcc, gloss, target_child_sex, part_of_speech)

# Warren Corpus, 9 children (4 girls, 5 boys)
warren <- get_utterances(role = "Mother", age = c(12,36), corpus = "Warren")
wr_utterance <- select(warren, gloss, target_child_sex, part_of_speech)

# Davis Corpus, 20 children (9 girls, 11 boys)
davis <- get_utterances(role = "Mother", age = c(12,36), corpus = "Davis")
dv_utterance <- select(davis, gloss, target_child_sex, part_of_speech)

# Weist Corpus, 6 children (3 girls, 3 boys)
weist <- get_utterances(role = "Mother", age = c(12,36), corpus = "Weist")
wst_utterance <- select(weist, gloss, target_child_name, part_of_speech)

# combined utterance
combined_utterance <- bind_rows(wst_utterance, dv_utterance, wr_utterance,
                                mcc_utterance, gle_utterance, pro_utterance)

# filter utterances that include adj.
adj_utterance <- combined_utterance %>% 
  filter(grepl("adj", part_of_speech))

# filter utterances that include nouns.
noun_utterance <- combined_utterance %>% 
  filter(grepl("n", part_of_speech))

# export tibbles
write.csv(adj_utterance, file = "adj_utterance.csv", row.names = FALSE)
write.csv(noun_utterance, file = "noun_utterance.csv", row.names = FALSE)

# female biased nouns: dress, doll, necklace, purse, baby, sweater, girl
# male biased nouns: hammer, truck, firetruck, broom, shovel, motorcycle, train

fnoun_utterance <- noun_utterance %>% 
  filter(grepl("dress", gloss))
