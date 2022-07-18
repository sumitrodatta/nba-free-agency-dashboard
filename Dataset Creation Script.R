library(tidyverse)
library(nbastatR)
library(tidymodels)
#for cleaning variable names
library(janitor)
#zoo allows rolling operations
library(zoo)
#matrix stats
library(matrixStats)

#edit these every year from https://basketball.realgm.com/nba/info/salary_cap
first_yr_min_salary=1637966
current_cap=123655000

cols_for_stats=cols(
  .default = col_double(),
  player = col_character(),
  pos = col_character(),
  lg = col_character(),
  tm = col_character()
)

advanced<-read_csv("Data/Input Data/Advanced.csv",col_types = cols_for_stats) %>%
  select(seas_id:mp,ows:ws,vorp) %>% mutate(ws_per_48=ws/mp*48,.before="vorp")
totals<-read_csv("Data/Input Data/Player Totals.csv",col_types = cols_for_stats)
#max games per season for players on multiple teams
max_games_tots=totals %>% filter(tm=="TOT") %>% group_by(season,lg,tm) %>%
  summarize(max_games_tot=max(g,na.rm = TRUE)) %>% ungroup()
#max games per season for players on single team
max_games=totals %>% filter(tm !="TOT") %>% group_by(season,lg) %>%
  summarize(max_games_non_tot=max(g,na.rm = TRUE)) %>% ungroup()
#coalesce above two into one column in totals df
totals_enhanced=left_join(totals,max_games_tots) %>% left_join(.,max_games) %>%
  mutate(max_games_playable=coalesce(max_games_tot,max_games_non_tot)) %>% 
  select(-c(max_games_non_tot,max_games_tot))
advanced_and_totals<-left_join(totals_enhanced,advanced) %>%
  #if player played for multiple teams in season, only take total row
  mutate(tm=ifelse(tm=="TOT","1TOT",tm)) %>% 
  group_by(player_id,season) %>% arrange(tm) %>% slice(1) %>% 
  mutate(tm=ifelse(tm=="1TOT","TOT",tm)) %>% 
  arrange(season,player) %>%
  mutate(g_percent=g/max_games_playable,gs_percent=gs/g,.before=g) %>% 
  select(-c(gs,max_games_playable)) %>% 
  #filter for only last ten years for faster pre-processing
  filter(season > 2009) %>% ungroup()

play_by_play<-read_csv("Data/Input Data/Player Play By Play.csv") %>% 
  filter(tm!="TOT") %>%
  select(seas_id:player,mp:c_percent)

#replace NA's with zeroes
play_by_play[is.na(play_by_play)] <- 0

pbp_pos_mins=play_by_play %>% 
  #convert percents to minutes played at position
  mutate(across(pg_percent:c_percent,~./100*mp)) %>% 
  rename_with(.fn=~gsub(x = ., pattern = "_percent", replacement = "_mp"),
              .cols=pg_percent:c_percent) %>%
  #sum season minutes across different teams
  group_by(season,player_id) %>% 
  mutate(across(mp:c_mp,sum,.names="{col}_summed")) %>% 
  slice(1) %>% ungroup() %>% select(-c(mp:c_mp))

rm(cols_for_stats,advanced,totals,max_games_tots,max_games,totals_enhanced,
   play_by_play)

past_free_agents<-read_csv("Data/Input Data/Past Free Agents.csv")

#subtract one from year to match up with offseason in which contract was signed
salary_cap_hist<-read_csv("Data/Input Data/Salary Cap History.csv") %>% mutate(season=season-1)
#create variable of first year salary as percentage of cap
#easier to compare across years
past_free_agents<-past_free_agents %>% select(-c(terms,Source)) %>% 
  left_join(.,salary_cap_hist) %>% 
  mutate(first_year_percent_of_cap=yr_1_salary/cap) %>% 
  select(-c(yr_1_salary,cap))

current_fa<-read_csv("Data/Input Data/Current Free Agents.csv")
#separate out options to compare what players options get if declined
current_fa_options<-current_fa %>% filter(str_detect(type,"PO|CO")) %>% 
  select(-c(experience,contract_yrs)) %>% 
  rename(option_type=type,option_amt=first_year_percent_of_cap)
#make player options all declined (UFA's)
#make club options ufa or rfa depending on exp
current_fa<-current_fa %>%
  mutate(type=case_when((type=="PO"|(type=="CO" & experience >= 4))~"UFA",
                        (type=="CO" & experience < 4)~"RFA",
                        TRUE~type)) %>% 
  group_by(player) %>% select(-experience) %>% slice(1) %>% ungroup() %>% 
  mutate(first_year_percent_of_cap=NA)

three_year_rolling_stats=advanced_and_totals %>% group_by(player_id) %>% 
  #three year sum
  mutate(across(-c(1:9,fg_percent,x3p_percent,
                   x2p_percent:e_fg_percent,ft_percent),
                list(three_yrs=~rollapplyr(.,3,sum,partial=TRUE)),
                .names="{col}_last_3_yrs")) %>%
  mutate(ws_per_48_last_3_yrs=ws_last_3_yrs/mp_last_3_yrs*48) %>%
  mutate(fg_percent=ifelse(fga==0,0,fg/fga),
         x3p_percent=ifelse(x3pa==0,0,x3p/x3pa),
         x2p_percent=ifelse(x2pa==0,0,x2p/x2pa),
         e_fg_percent=ifelse(fga==0,0,(fg+0.5*x3p)/fga),
         ft_percent=ifelse(fta==0,0,ft/fta)) %>%
  mutate(fg_percent_last_3_yrs=
           ifelse(fga_last_3_yrs==0,0,fg_last_3_yrs/fga_last_3_yrs),
         x3p_percent_last_3_yrs=
           ifelse(x3pa_last_3_yrs==0,0,x3p_last_3_yrs/x3pa_last_3_yrs),
         x2p_percent_last_3_yrs=
           ifelse(x2pa_last_3_yrs==0,0,x2p_last_3_yrs/x2pa_last_3_yrs),
         e_fg_percent_last_3_yrs=
           ifelse(fga_last_3_yrs==0,0,
                  (fg_last_3_yrs+0.5*x3p_last_3_yrs)/fga_last_3_yrs),
         ft_percent_last_3_yrs=
           ifelse(fta_last_3_yrs==0,0,ft_last_3_yrs/fta_last_3_yrs)) %>%
  #remove categories that aren't predictive vars or linear combo of others
  select(-c(lg,pos,birth_year,tm,
            trb,trb_last_3_yrs,
            fg,fga,fg_last_3_yrs,fga_last_3_yrs,
            pts,pts_last_3_yrs)) %>%
  #convert contract year and last 3 year stats to per game (except games)
  mutate(across(c(mp,x3p:x3pa,x2p:x2pa,ft:fta,orb:pf),list(per_game=~./g)),
         .after="gs_percent") %>%
  select(-c(g,mp,x3p:x3pa,x2p:x2pa,ft:fta,orb:pf,ws)) %>% 
  mutate(across(mp_last_3_yrs:pf_last_3_yrs,list(per_game=~./g_last_3_yrs)),
         .after="gs_percent_last_3_yrs") %>%
  select(-c(g_last_3_yrs,mp_last_3_yrs:pf_last_3_yrs,ws_last_3_yrs)) %>% 
  ungroup() %>% 
  #rescale games percentages over 3 years back to 0-1
  mutate(across(g_percent_last_3_yrs:gs_percent_last_3_yrs,~./3)) %>%
  replace_na(list(fg_percent=0,x3p_percent=0,x2p_percent=0,
                  e_fg_percent=0,ft_percent=0))

pbp_last_three_years=pbp_pos_mins %>% group_by(player_id) %>%
  #rolling 3 year sum of minutes played total & position
  mutate(across(mp_summed:c_mp_summed,
                list(three_yrs=~rollapplyr(.,3,sum,partial=TRUE)),
                .names="{col}_last_3_yrs")) %>% ungroup() %>% 
  select(-c(mp_summed:c_mp_summed)) %>%
  #convert totals back to percents
  mutate(across(pg_mp_summed_last_3_yrs:c_mp_summed_last_3_yrs,
                ~./mp_summed_last_3_yrs)) %>%
  rename_with(.fn=~gsub(x = ., pattern = "_mp", replacement = "_percent"),
              .cols=pg_mp_summed_last_3_yrs:c_mp_summed_last_3_yrs) %>% 
  select(-mp_summed_last_3_yrs)

#assign pure positions to players with 75% of time at one position
pure_position=pbp_last_three_years %>% 
  pivot_longer(.,cols=c(pg_percent_summed_last_3_yrs:c_percent_summed_last_3_yrs),
               names_to = "pos",values_to = "percent_at_pos") %>% group_by(seas_id) %>% 
  slice_max(percent_at_pos) %>% ungroup() %>%
  filter(percent_at_pos>0.75) %>% mutate(pos=word(pos,1,sep="_"))

combo_position=
  #remove players with pure positions
  anti_join(pbp_last_three_years,pure_position %>% select(1:4)) %>% 
  #create buckets of in-between positions
  mutate(combo_guard=pg_percent_summed_last_3_yrs+sg_percent_summed_last_3_yrs,
         small_wing=sg_percent_summed_last_3_yrs+sf_percent_summed_last_3_yrs,
         big_wing=sf_percent_summed_last_3_yrs+pf_percent_summed_last_3_yrs,
         big_man=pf_percent_summed_last_3_yrs+c_percent_summed_last_3_yrs) %>%
  select(-c(pg_percent_summed_last_3_yrs:c_percent_summed_last_3_yrs)) %>% 
  pivot_longer(.,cols=c(combo_guard:big_man),
               names_to = "pos",values_to = "percent_at_pos") %>% 
  group_by(seas_id) %>% slice_max(percent_at_pos) %>% ungroup() %>%
  #4 players had 2 separate combo positions listed
  filter(!(player=="Terrance Roberson" & pos=="big_wing") &
           !(player=="Vince Hunter" & pos=="small_wing") &
           !(player=="Ty Jerome" & season==2020 & pos=="small_wing") &
           !(player=="Anthony Gill" & season==2021 & pos=="big_man"))

all_player_pos=bind_rows(pure_position,combo_position) %>% 
  #2 players had 0 MP, so they were lost in both combo & pure
  add_row(seas_id=19914,season=2006,player_id=3589,
          player="Alex Scales",pos="sg",percent_at_pos=1) %>%
  add_row(seas_id=22403,season=2010,player_id=3882,
          player="JamesOn Curry",pos="pg",percent_at_pos=1) %>% 
  filter(season>2009) %>% select(-c(seas_id,percent_at_pos)) %>%
  mutate(pos_group=case_when(pos %in% c("pg","sg","combo_guard")~"guard",
                             pos %in% c("sf","small_wing","big_wing")~"wing",
                             pos %in% c("pf","c","big_man")~"big"))

rm(advanced_and_totals,combo_position,pbp_last_three_years,pbp_pos_mins,pure_position)

train_eval_set_initiations<-function(df,past=TRUE){
  if (past){
    df<-df %>% select(-ws)
  }
  final_df=inner_join(three_year_rolling_stats,df) %>% 
    left_join(.,all_player_pos) %>% group_by(season,pos_group) %>% 
    mutate(position_vorp=sum(vorp_last_3_yrs)) %>% ungroup() %>%
    mutate(percent_of_pos_vorp=vorp_last_3_yrs/position_vorp) %>%
    mutate(contract_yrs=factor(contract_yrs,levels = 0:5)) %>%
    select(-c(pos,pos_group,position_vorp)) %>%
    mutate(across(-c(seas_id:experience,type:contract_yrs),~round(.,digits=4)))
  return(final_df)
}

train_set=train_eval_set_initiations(past_free_agents)
write_csv(train_set,"Data/Train Set.csv")

eval_set=train_eval_set_initiations(current_fa,past=FALSE)
write_csv(eval_set,"Data/Eval Set.csv")

fit_y1_forest=readRDS(file="Data/Input Data/Y1 Forest.rds")
fit_s2_forest=readRDS(file="Data/Input Data/S2 Forest.rds")
fit_s2_svm=readRDS(file="Data/Input Data/S2 SVM.rds")
fit_s1_forest=readRDS(file="Data/Input Data/S1 Forest.rds")
fit_y2_forest=readRDS(file="Data/Input Data/Y2 Forest.rds")

eval_y1_predict=as.numeric(as.character(
  predict(fit_y1_forest,new_data=eval_set) %>% pull()))
eval_s2_predict_forest=predict(fit_s2_forest,
                               new_data=eval_set %>% select(-contract_yrs) %>%
                                 bind_cols(contract_yrs=eval_y1_predict))
eval_s2_predict_svm=predict(fit_s2_svm,
                            new_data=eval_set %>% select(-contract_yrs) %>%
                              bind_cols(contract_yrs=eval_y1_predict))
eval_s2_predict=tibble(forest=eval_s2_predict_forest %>% pull(),
                       svm=eval_s2_predict_svm %>% pull()) %>% rowwise() %>%
  mutate(m=mean(c_across(forest:svm))) %>% ungroup() %>% pull(m) 

eval_s1_predict=predict(fit_s1_forest,new_data=eval_set) %>% pull()
eval_y2_predict=as.numeric(as.character(
  predict(fit_y2_forest,new_data=eval_set %>% select(-first_year_percent_of_cap) %>%
            bind_cols(first_year_percent_of_cap=eval_s1_predict)) %>% 
    pull()))

final_results<-eval_set %>% 
  select(-c(contract_yrs,first_year_percent_of_cap)) %>%
  add_column(yrs_Y1S2=eval_y1_predict,
             yr1_cap_percent_Y1S2=eval_s2_predict,
             yrs_S1Y2=eval_y2_predict,
             yr1_cap_percent_S1Y2=eval_s1_predict) %>% 
  mutate(yrs_Y1S2=ifelse(yr1_cap_percent_Y1S2<=first_yr_min_salary/current_cap,0,yrs_Y1S2)) %>%
  mutate(yr1_cap_percent_Y1S2=ifelse(yrs_Y1S2==0,0,yr1_cap_percent_Y1S2)) %>%
  mutate(yrs_S1Y2=ifelse(yr1_cap_percent_S1Y2<=first_yr_min_salary/current_cap,0,yrs_S1Y2)) %>%
  mutate(yr1_cap_percent_S1Y2=ifelse(yrs_S1Y2==0,0,yr1_cap_percent_S1Y2)) %>% 
  mutate(total_Y1S2=
           round(yr1_cap_percent_Y1S2*current_cap*((1.05)^(yrs_Y1S2)-1)/0.05,digits=-4),
         total_S1Y2=
           round(yr1_cap_percent_S1Y2*current_cap*((1.05)^(yrs_S1Y2)-1)/0.05,digits=-4)) %>%
  mutate(yr1_cap_percent_Y1S2=round(yr1_cap_percent_Y1S2,digits=4),
         yr1_cap_percent_S1Y2=round(yr1_cap_percent_S1Y2,digits=4))

options_decisions=inner_join(final_results,current_fa_options) %>%
  arrange(desc(option_amt)) %>%
  select(player,age,"Y1S2 Cap %"=yr1_cap_percent_Y1S2,yrs_Y1S2,total_Y1S2,"S1Y2 Cap %"=yr1_cap_percent_S1Y2,yrs_S1Y2,
         total_S1Y2,"Option Type"=option_type,"Option Amount"=option_amt)

non_options=anti_join(final_results,current_fa_options %>% select(player)) %>%
  arrange(desc((yr1_cap_percent_Y1S2+yr1_cap_percent_S1Y2)/2)) %>%
  rename("Y1S2 Cap %"=yr1_cap_percent_Y1S2, "S1Y2 Cap %"=yr1_cap_percent_S1Y2) %>%
  select(player,age,type,"Y1S2 Cap %",yrs_Y1S2,total_Y1S2,"S1Y2 Cap %",yrs_S1Y2,total_S1Y2)

write_csv(options_decisions,"Data/Options.csv")
write_csv(non_options,"Data/Non-Option Contracts.csv")

rm(eval_s1_predict,eval_s2_predict,eval_s2_predict_forest,eval_s2_predict_svm,
   eval_y1_predict,eval_y2_predict,
   fit_s1_forest,fit_s2_forest,fit_s2_svm,fit_y1_forest,fit_y2_forest)

train_eval=add_row(train_set,eval_set)

write_csv(train_eval,"Data/Train & Eval Set Combined.csv")

row_numbers=train_eval %>% select(seas_id) %>% mutate(row=row_number())

pre_sim_matrix=train_eval %>% mutate(type=case_when(type=="UFA"~0,type=="RFA"~1)) %>% 
  select(-c(contract_yrs,first_year_percent_of_cap)) %>%
  select(age:percent_of_pos_vorp)

similarity_scores=as_tibble(as.matrix(dist(scale(pre_sim_matrix)))) %>% 
  rowid_to_column() %>% 
  pivot_longer(cols=!rowid,names_to="to_compare",values_to="similarity",names_transform=as.integer) %>%
  mutate(self_compare=(rowid==to_compare),
         similarity=
           scales::rescale(similarity,to=c(1,0))) %>%
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
                              str_detect(player,"Cameron Reynolds")~"Cam Reynolds",
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
