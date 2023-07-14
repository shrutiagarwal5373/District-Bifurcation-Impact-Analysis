library(tidyverse)
library(plyr)
library(stargazer)
library(haven)
### CENSUS 1991
pc91_subd_key <- read_dta("C:/Users/shrut/OneDrive/Desktop/New folder (2)/shrug_pc91_subdistrict_key.dta")
data_91 <- read_dta("C:/Users/shrut/OneDrive/Desktop/New folder (2)/shrug_pc91.dta")
d91<-left_join(data_91,pc91_subd_key)#, by="shrid"
d91<-d91%>%
  select(pc91_state_id, pc91_district_id, pc91_subdistrict_id, pc91_pca_no_hh, pc91_pca_tot_p, pc91_td_area, pc91_pca_p_sc,pc91_pca_p_st, pc91_pca_p_lit, pc91_vd_p_sch, pc91_vd_s_sch, pc91_vd_tar_road, pc91_vd_dirt_road, pc91_vd_power_dom, pc91_vd_power_agr, pc91_vd_power_all)  
names(d91)<-c("stc1991", "dtc1991", "subdt1991", "hh1991", "tot_pop1991", "area1991", "tot_sc1991", "tot_st1991", "lit_pop1991", "pri_school1991", "sec_school1991", "paved_road1991", "nonpaved_road1991", "electricity_dom1991", "electricity_agri1991", "electricity_all1991")#, "shr_lit1991"
d91<-d91 %>% mutate(tot_oth1991=tot_pop1991-tot_sc1991-tot_st1991)
d91$subdt1991<-str_c(str_pad(d91$stc1991,2,side="left","0"),str_pad(d91$dtc1991,3,side="left","0"),str_pad(d91$subdt1991,5,side="left","0"))
length(unique(d91$subdt1991))
##length=6601
group_subdt91<-group_by(d91,subdt1991)
data_1991<-dplyr::summarise(group_subdt91,
                            stc1991=stc1991[1],
                            dtc1991=dtc1991[1],
                            hh1991=sum(hh1991,na.rm = TRUE),
                            tot_pop1991=sum(tot_pop1991,na.rm = TRUE),
                            tot_sc1991=sum(tot_sc1991,na.rm = TRUE),
                            tot_st1991=sum(tot_st1991,na.rm = TRUE),
                            tot_oth1991=sum(tot_oth1991,na.rm = TRUE),
                            area1991=sqrt(sum(as.numeric(area1991),na.rm=TRUE)/100),
                            lit_pop1991=sum(lit_pop1991,na.rm = TRUE),
                            pri_school1991=sum(pri_school1991,na.rm = TRUE),
                            sec_school1991=sum(sec_school1991,na.rm = TRUE)
)
data_1991<-data_1991 %>% mutate(shr_lit1991=lit_pop1991*100/tot_pop1991)
### CENSUS 2001
pc01_subd_key <- read_dta("C:/Users/shrut/OneDrive/Desktop/New folder (2)/shrug_pc01_subdistrict_key.dta")
data_01 <- read_dta("C:/Users/shrut/OneDrive/Desktop/New folder (2)/shrug_pc01.dta")
d01<-left_join(data_01,pc01_subd_key)#, by="shrid"
d01<-d01%>%
  select(pc01_state_id, pc01_district_id, pc01_subdistrict_id, pc01_pca_no_hh, pc01_pca_tot_p, pc01_td_area, pc01_pca_p_sc,pc01_pca_p_st, pc01_pca_p_lit, pc01_vd_p_sch, pc01_vd_s_sch, pc01_vd_tar_road, pc01_vd_dirt_road, pc01_vd_power_dom, pc01_vd_power_agr, pc01_vd_power_all)  
names(d01)<-c("stc2001", "dtc2001", "subdt2001", "hh2001", "tot_pop2001", "area2001", "tot_sc2001", "tot_st2001", "lit_pop2001", "pri_school2001", "sec_school2001", "paved_road2001", "nonpaved_road2001", "electricity_dom2001", "electricity_agri2001", "electricity_all2001")#, "shr_lit2001"
d01<-d01 %>% mutate(tot_oth2001=tot_pop2001-tot_sc2001-tot_st2001)
d01$subdt2001<-str_c(str_pad(d01$stc2001,2,side="left","0"),str_pad(d01$dtc2001,3,side="left","0"),str_pad(d01$subdt2001,5,side="left","0"))
length(unique(d01$subdt2001))
##length=5461
group_subdt01<-group_by(d01,subdt2001)
data_2001<-dplyr::summarise(group_subdt01,
                            stc2001=stc2001[1],
                            dtc2001=dtc2001[1],
                            hh2001=sum(hh2001,na.rm = TRUE),
                            tot_pop2001=sum(tot_pop2001,na.rm = TRUE),
                            tot_sc2001=sum(tot_sc2001,na.rm = TRUE),
                            tot_st2001=sum(tot_st2001,na.rm = TRUE),
                            tot_oth2001=sum(tot_oth2001,na.rm = TRUE),
                            area2001=sqrt(sum(as.numeric(area2001),na.rm=TRUE)/100),
                            lit_pop2001=sum(lit_pop2001,na.rm = TRUE),
                            pri_school2001=sum(pri_school2001,na.rm = TRUE),
                            sec_school2001=sum(sec_school2001,na.rm = TRUE)
)
data_2001<-data_2001 %>% mutate(shr_lit2001=lit_pop2001*100/tot_pop2001)
### CENSUS 2011
pc11_subd_key <- read_dta("C:/Users/shrut/OneDrive/Desktop/New folder (2)/shrug_pc11_subdistrict_key.dta")
data_11 <- read_dta("C:/Users/shrut/OneDrive/Desktop/New folder (2)/shrug_pc11.dta")
d11<-left_join(data_11,pc11_subd_key)#, by="shrid"
d11<-d11%>%
  select(pc11_state_id, pc11_district_id, pc11_subdistrict_id, pc11_pca_no_hh, pc11_pca_tot_p, pc11_td_area, pc11_pca_p_sc,pc11_pca_p_st, pc11_pca_p_lit, pc11_vd_p_sch, pc11_vd_s_sch, pc11_vd_tar_road, pc11_vd_power_dom, pc11_vd_power_agr, pc11_vd_power_all)  
names(d11)<-c("stc2011", "dtc2011", "subdt2011", "hh2011", "tot_pop2011", "area2011", "tot_sc2011", "tot_st2011", "lit_pop2011", "pri_school2011", "sec_school2011", "paved_road2011", "electricity_dom2011", "electricity_agri2011", "electricity_all2011")#, "shr_lit"
d11<-d11 %>% mutate(tot_oth2011=tot_pop2011-tot_sc2011-tot_st2011)
d11$subdt2011<-str_c(str_pad(d11$stc2011,2,side="left","0"),str_pad(d11$dtc2011,3,side="left","0"),str_pad(d11$subdt2011,5,side="left","0"))
length(unique(d11$subdt2011))
##length=5965
group_subdt11<-group_by(d11,subdt2011)
data_2011<-dplyr::summarise(group_subdt11,
                            stc2011=stc2011[1],
                            dtc2011=dtc2011[1],
                            hh2011=sum(hh2011,na.rm = TRUE),
                            tot_pop2011=sum(tot_pop2011,na.rm = TRUE),
                            tot_sc2011=sum(tot_sc2011,na.rm = TRUE),
                            tot_st2011=sum(tot_st2011,na.rm = TRUE),
                            tot_oth2011=sum(tot_oth2011,na.rm = TRUE),
                            area2011=sqrt(sum(as.numeric(area2011),na.rm=TRUE)/100),
                            lit_pop2011=sum(lit_pop2011,na.rm = TRUE),
                            pri_school2011=sum(pri_school2011,na.rm = TRUE),
                            sec_school2011=sum(sec_school2011,na.rm = TRUE)
)
data_2011<-data_2011 %>% mutate(shr_lit2011=lit_pop2011*100/tot_pop2011)
###merged
all<-left_join(data_11,pc11_subd_key)#, by="shrid"
all<-left_join(all,data_01)#, by="shrid"
all<-left_join(all,data_91)#, by="shrid"
all<-all%>%
  select(pc11_state_id, pc11_district_id, pc11_subdistrict_id, pc11_pca_no_hh, pc11_pca_tot_p, pc11_td_area, pc11_pca_p_sc,pc11_pca_p_st, pc11_pca_p_lit, pc11_vd_p_sch, pc11_vd_s_sch, pc11_vd_tar_road, pc11_vd_power_dom, pc11_vd_power_agr, pc11_vd_power_all, pc01_pca_no_hh, pc01_pca_tot_p, pc01_td_area, pc01_pca_p_sc,pc01_pca_p_st, pc01_pca_p_lit, pc01_vd_p_sch, pc01_vd_s_sch, pc01_vd_tar_road, pc01_vd_power_dom, pc01_vd_power_agr, pc01_vd_power_all, pc91_pca_no_hh, pc91_pca_tot_p, pc91_td_area, pc91_pca_p_sc,pc91_pca_p_st, pc91_pca_p_lit, pc91_vd_p_sch, pc91_vd_s_sch, pc91_vd_tar_road, pc91_vd_power_dom, pc91_vd_power_agr, pc91_vd_power_all)
names(all)<-c("stc2011", "dtc2011", "subdt2011", "hh2011", "tot_pop2011", "area2011", "tot_sc2011", "tot_st2011", "lit_pop2011", "pri_school2011", "sec_school2011", "paved_road2011", "electricity_dom2011", "electricity_agri2011", "electricity_all2011", "hh2001", "tot_pop2001", "area2001", "tot_sc2001", "tot_st2001", "lit_pop2001", "pri_school2001", "sec_school2001", "paved_road2001", "electricity_dom2001", "electricity_agri2001", "electricity_all2001", "hh1991", "tot_pop1991", "area1991", "tot_sc1991", "tot_st1991", "lit_pop1991", "pri_school1991", "sec_school1991", "paved_road1991", "electricity_dom1991", "electricity_agri1991", "electricity_all1991")#, "shr_lit"
all<-all %>% mutate(tot_oth2011=tot_pop2011-tot_sc2011-tot_st2011, tot_oth2001=tot_pop2001-tot_sc2001-tot_st2001, tot_oth1991=tot_pop1991-tot_sc1991-tot_st1991)
all$subdt2011<-str_c(str_pad(all$stc2011,2,side="left","0"),str_pad(all$dtc2011,3,side="left","0"),str_pad(all$subdt2011,5,side="left","0"))
length(unique(all$subdt2011))
##length=5965
group_subdtall<-group_by(all, subdt2011)
data_all<-dplyr::summarise(group_subdtall,
                           stc2011=stc2011[1],
                           dtc2011=dtc2011[1],
                           hh2011=sum(hh2011,na.rm = TRUE),
                           tot_pop2011=sum(tot_pop2011,na.rm = TRUE),
                           tot_sc2011=sum(tot_sc2011,na.rm = TRUE),
                           tot_st2011=sum(tot_st2011,na.rm = TRUE),
                           tot_oth2011=sum(tot_oth2011,na.rm = TRUE),
                           area2011=sqrt(sum(as.numeric(area2011),na.rm=TRUE)/100),
                           lit_pop2011=sum(lit_pop2011,na.rm = TRUE),
                           pri_school2011=sum(pri_school2011,na.rm = TRUE),
                           sec_school2011=sum(sec_school2011,na.rm = TRUE),
                           hh2001=sum(hh2001,na.rm = TRUE),
                           tot_pop2001=sum(tot_pop2001,na.rm = TRUE),
                           tot_sc2001=sum(tot_sc2001,na.rm = TRUE),
                           tot_st2001=sum(tot_st2001,na.rm = TRUE),
                           tot_oth2001=sum(tot_oth2001,na.rm = TRUE),
                           area2001=sqrt(sum(as.numeric(area2001),na.rm=TRUE)/100),
                           lit_pop2001=sum(lit_pop2001,na.rm = TRUE),
                           pri_school2001=sum(pri_school2001,na.rm = TRUE),
                           sec_school2001=sum(sec_school2001,na.rm = TRUE),
                           hh1991=sum(hh1991,na.rm = TRUE),
                           tot_pop1991=sum(tot_pop1991,na.rm = TRUE),
                           tot_sc1991=sum(tot_sc1991,na.rm = TRUE),
                           tot_st1991=sum(tot_st1991,na.rm = TRUE),
                           tot_oth1991=sum(tot_oth1991,na.rm = TRUE),
                           area1991=sqrt(sum(as.numeric(area1991),na.rm=TRUE)/100),
                           lit_pop1991=sum(lit_pop1991,na.rm = TRUE),
                           pri_school1991=sum(pri_school1991,na.rm = TRUE),
                           sec_school1991=sum(sec_school1991,na.rm = TRUE)
)
data_all<-data_all %>% mutate(shr_lit2011=lit_pop2011*100/tot_pop2011, shr_lit2001=lit_pop2001*100/tot_pop2001, shr_lit1991=lit_pop1991*100/tot_pop1991)
data_all <- subset(data_all, dtc2011!='' )
data_all <- subset(data_all, stc2011!='01' )
data_1991 <- subset(data_1991,dtc1991!='' )
data_1991 <- subset(data_1991, stc1991!='01' )
data_2001 <- subset(data_2001,dtc2001!='' )
data_2001 <- subset(data_2001,stc2001!='01' )
data_2011 <- subset(data_2011,dtc2011!='' )
data_2011 <- subset(data_2011,stc2011!='01' )
##data_combined<-left_join(pc01_subd_key,pc91_subd_key)#, by="shrid"
##data_combined<-left_join(data_combined,pc11_subd_key)#, by="shrid"
pc91_subd_key<-pc91_subd_key%>%
  select(pc91_state_id, pc91_district_id, pc91_subdistrict_id, shrid)
names(pc91_subd_key)<-c("stc1991", "dtc1991", "subdt1991", "shrid")
pc01_subd_key<-pc01_subd_key%>%
  select(pc01_state_id, pc01_district_id, pc01_subdistrict_id, shrid)
names(pc01_subd_key)<-c("stc2001", "dtc2001", "subdt2001", "shrid")
pc11_subd_key<-pc11_subd_key%>%
  select(pc11_state_id, pc11_district_id, pc11_subdistrict_id, shrid)
names(pc11_subd_key)<-c("stc2011", "dtc2011", "subdt2011", "shrid")
pc91_subd_key$subdt1991<-str_c(str_pad(pc91_subd_key$stc1991,2,side="left","0"),str_pad(pc91_subd_key$dtc1991,3,side="left","0"),str_pad(pc91_subd_key$subdt1991,5,side="left","0"))
pc01_subd_key$subdt2001<-str_c(str_pad(pc01_subd_key$stc2001,2,side="left","0"),str_pad(pc01_subd_key$dtc2001,3,side="left","0"),str_pad(pc01_subd_key$subdt2001,5,side="left","0"))
pc11_subd_key$subdt2011<-str_c(str_pad(pc11_subd_key$stc2011,2,side="left","0"),str_pad(pc11_subd_key$dtc2011,3,side="left","0"),str_pad(pc11_subd_key$subdt2011,5,side="left","0"))
data_combined_11<-left_join(pc11_subd_key, data_2011)#, by="subdt2011"
data_combined_91<-left_join(pc91_subd_key, data_1991)#, by="subdt1991"
data_combined_01<-left_join(pc01_subd_key, data_2001)#, by="subdt2001"
data_combined<-left_join(data_combined_01, data_combined_91)#, by="shrid"
data_combined<-left_join(data_combined, data_combined_11)#, by="shrid"
data_combined <- subset(data_combined, subdt2011!='NA' )
data_combined <- subset(data_combined, stc2011!='01' )
data_combined <- data_combined[,-4]
data_combined  <-data_combined  %>% distinct()
data_bifurcation <- read_excel("C:/Users/shrut/Downloads/bifurcation.xlsx")
data_bifurcation<-data_bifurcation %>% mutate(split=split2001|split2011)
data_bifurcation$split <- as.integer(as.logical(data_bifurcation$split))