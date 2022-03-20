library(tidyverse)

train_set=read_csv("Data/Train Set.csv")

row_numbers=train_set %>% select(seas_id) %>% mutate(row=row_number())

pre_sim_matrix=train_set %>% mutate(type=case_when(type=="UFA"~0,type=="RFA"~1)) %>% 
  select(-c(contract_yrs,first_year_percent_of_cap)) %>%
  select(age:percent_of_pos_vorp)

similarity_scores=as_tibble(as.matrix(dist(scale(pre_sim_matrix)))) %>% 
  rowid_to_column() %>% 
  pivot_longer(cols=`1`:`1227`,names_to="to_compare",values_to="similarity",names_transform=as.integer) %>%
  mutate(self_compare=(rowid==to_compare),
         similarity=
           scales::rescale(similarity,to=c(1,0))) %>%
           #round(1-similarity/ncol(pre_sim_matrix),digits = 4)) %>% 
  filter(!self_compare) %>%
  left_join(.,row_numbers,by=c('rowid'='row')) %>%
  left_join(.,row_numbers,by=c('to_compare'='row')) %>% select(-c(rowid,to_compare,self_compare)) %>%
  rename(seas_id_base=seas_id.x,to_compare=seas_id.y)

write_csv(similarity_scores,"Data/Similarity Scores.csv")
