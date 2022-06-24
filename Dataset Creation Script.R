library(tidyverse)
library(nbastatR)

train_set=read_csv("Data/Train Set.csv")

eval_set=read_csv("Data/Eval Set.csv")

train_eval=add_row(train_set,eval_set)

write_csv(train_eval,"Data/Train & Eval Set Combined.csv")

row_numbers=train_eval %>% select(seas_id) %>% mutate(row=row_number())

pre_sim_matrix=train_eval %>% mutate(type=case_when(type=="UFA"~0,type=="RFA"~1)) %>% 
  select(-c(contract_yrs,first_year_percent_of_cap)) %>%
  select(age:percent_of_pos_vorp)

similarity_scores=as_tibble(as.matrix(dist(scale(pre_sim_matrix)))) %>% 
  rowid_to_column() %>% 
  pivot_longer(cols=`1`:`1417`,names_to="to_compare",values_to="similarity",names_transform=as.integer) %>%
  mutate(self_compare=(rowid==to_compare),
         similarity=
           scales::rescale(similarity,to=c(1,0))) %>%
           #round(1-similarity/ncol(pre_sim_matrix),digits = 4)) %>% 
  filter(!self_compare) %>%
  left_join(.,row_numbers,by=c('rowid'='row')) %>%
  left_join(.,row_numbers,by=c('to_compare'='row')) %>% select(-c(rowid,to_compare,self_compare)) %>%
  rename(seas_id_base=seas_id.x,to_compare=seas_id.y)

write_csv(similarity_scores,"Data/Similarity Scores.csv")

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
assign_nba_players()

playerPhotos=left_join(train_eval %>% distinct(player_id,player) %>% mutate(remove_accent=stringi::stri_trans_general(str=player,id="Latin-ASCII")),
                       df_dict_nba_players %>% filter(yearSeasonLast>2013) %>% select(namePlayer,idPlayer),by=c("remove_accent"="namePlayer")) %>% 
  select(player_id,player,idPlayer)

players_to_change=playerPhotos %>% filter(is.na(idPlayer)) %>% select(-idPlayer) %>%
  mutate(namePlayer=case_when(str_detect(player,"\\.")~str_remove_all(player,"\\."),
                              str_detect(player,"PJ")~"P.J. Dozier",
                              str_detect(player,"Kanter")~"Enes Freedom",
                              str_detect(player,"Marcus Morris")~"Marcus Morris Sr.",
                              str_detect(player,"Creek")~"Mitchell Creek",
                              str_detect(player,"Vince Edwards")~"Vincent Edwards",
                              str_detect(player,"Wade Baldwin")~"Wade Baldwin IV",
                              str_detect(player,"Bowen|Michael F")~paste(player,"II"),
                              str_detect(player,"Juan")~"Juancho Hernangomez",
                              str_detect(player,"Iwundu")~"Wes Iwundu",
                              str_detect(player,"Sviato")~"Svi Mykhailiuk",
                              str_detect(player,"Kevin Knox")~"Kevin Knox II",
                              str_detect(player,"Lonnie")~"Lonnie Walker IV",
                              str_detect(player,"Clax")~"Nic Claxton",
                              str_detect(player,"Woodard")~"Robert Woodard II",
                              str_detect(player,"Ennis|Giles|O'Bryant|Andrew White")~paste(player,"III"),
                              TRUE~paste(player,"Jr."))) %>%
  left_join(.,df_dict_nba_players %>% select(namePlayer,idPlayer))

finalizedPlayerPhotos=playerPhotos %>% filter(!is.na(idPlayer)) %>% bind_rows(players_to_change %>% select(-namePlayer)) %>% 
  mutate(urlPlayerThumbnail=paste0("https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/",idPlayer,".png"))
write_csv(finalizedPlayerPhotos,"Data/Player Photos.csv")
