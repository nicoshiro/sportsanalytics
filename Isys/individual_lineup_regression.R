library(factoextra)
library(descr)
library(dplyr)
library(tidyr)
library(FactoMineR)
library(igraph)

setwd("/Users/isysjo/Documents/Summer REU 2018/SportsAnalytics/R")

source("../Isys/cluster_id_model.R")

# get lineups
all_lineups <- read.csv('../Data/2017_2018/train_for_lineups_full.csv')
all_lineups<- rbind(all_lineups,read.csv("test_for_lineups_full.csv"))

numeric_lin <- all_lineups %>% dplyr::select(TOTAL_MIN:TOV, -TOTAL_MIN, -LINEUP, -TOT_PT_DIFF)
numeric_lin_scaled <- scale(numeric_lin)

# get individual player data and scale it
clean_individual_player_data <- function(player_table) {
  player_table <- player_table %>%
    filter(MIN > 0)
  return(player_table)
}


#cluster and get lineup ids
model_clust <- Mclust(filtered_scaled, G=1:9, modelNames = mclust.options("emModelNames"))
clusters_added_EM <- cbind(aggregate_training, cluster = model_clust$classification)

# get lineup types for all lineups
all_mlineup_types <- sapply(all_lineups$LINEUP %>% as.character, function(x) get_lineup_type(x, clusters_added_EM))


# give each lineup a efficiency rating
all_eff <- c()
for(w in 1:374) {
  all_eff <- sum(numeric_lin_scaled[w,1:6]) - numeric_lin_scaled[w,7]
}

# get X3P stats for each player for each lineup
x3ps <- sapply(all_lineups$LINEUP,
               function(x) get_individual_stats(x, "FG3_PCT", aggregate_training) %>% as.double) %>% t

# get FG% stats for each player for each lineup
fgs <- sapply(all_lineups$LINEUP,
              function(x) get_individual_stats(x, "FG_PCT", aggregate_training) %>% as.double) %>% t

# get REB stats for each player for each lineup
ors <- sapply(all_lineups$LINEUP, function(x) get_individual_stats(x, "OREB", aggregate_training) %>% as.double)
drs <- sapply(all_lineups$LINEUP, function(x) get_individual_stats(x, "DREB", aggregate_training) %>% as.double)
trs <- t(ors+drs)

# get AST stats for each player for each lineup
asts <- sapply(all_lineups$LINEUP, function(x) get_individual_stats(x, "AST", aggregate_training) %>% as.double) %>% t

#combine into data frame
ndf_full <- cbind(fgs,x3ps,trs,asts,all_lineups$TOT_PT_DIFF,all_eff)
colnames(ndf_full) <- c("FG1","FG2","FG3","FG4","FG5","3P1","3P2","3P3","3P4","3P5","R1","R2","R3","R4","R5","A1"
                    ,"A2","A3","A4","A5","PT_DIFF","EFF")


# predicting lineup efficiency
#predicted <- get_predicted_stats() %>% dplyr::select(-LINEUP) %>% scale
#ndf_p <- cbind(ndf,all_lineups[1:302,]$TOT_PT_DIFF,V(j.graph.training)[1:302]$EST_EFF) %>% as.data.frame()

#predicted_eff <- sum(predicted[,1:6]) - predicted[,7]

# with estimated efficiency: 0.6887977 training data
plot(predict(lm(PT_DIFF ~ ., ndf_full[1:302,]),ndf_full[1:302,]),ndf_full$PT_DIFF[1:302])
# without estimated efficiency: 0.3384578 training data
plot(predict(lm(PT_DIFF ~ ., ndf_full[1:302,]%>%dplyr::select(-EFF)),ndf_full[1:302,]),ndf_full$PT_DIFF[1:302])


# problem: what is the best way to predict estimated efficiency?
#ideas : modify predicted stats? is there a better model than LM? how do i find it
#possibly use LINEUP ids and try to find a contribution factor? there would have
# to be some way to introduce the interaction factors
# best case: be able to have some reasonable estimate for every possible lineup type
#which is why the contribution factors + trends are so valuable


#introduce interaction terms and such so count how many of each type is in each cluster?
matrix_clusters <- matrix(nrow = 374, ncol=length(unique(clusters_added_EM$cluster)))
for(v in 1:length(all_mlineup_types)) {
  clusters <- strsplit(all_mlineup_types[v], '') %>% unlist %>% as.numeric
  matrix_clusters[v,clusters %>% table %>% names %>% as.numeric] <- clusters %>% table
}
matrix_clusters[is.na(matrix_clusters)] <- 0
colnames(matrix_clusters) <- c("C1","C2","C3","C4","C5","C6","C7","C8")


ndf_clusters <- cbind(ndf_full,matrix_clusters)
#plot(predict(lm(PT_DIFF ~ ., ndf_p%>%dplyr::select(-EFF)),ndf_p),ndf_p$PT_DIFF)
#ndf_ppp <- cbind(matrix_clusters[1:302,], EFF= ndf_p$EFF) %>% as.data.frame

#eff_lm <- lm(EFF ~  poly(C1,2) + poly(C2,2) + poly(C3,2) +poly(C4,2)
#             + poly(C5,2) + poly(C6,2)+ poly(C7,2) + poly(C8,2) +.^2, data=ndf_ppp)
#plot(ndf_ppp$EFF,predict(eff_lm,ndf_ppp))

#ndf_pppp <- cbind(ndf_p, PEFF=predict(eff_lm,ndf_ppp))
#plot(predict(lm(PT_DIFF ~ ., ndf_pppp%>%dplyr::select(-EFF)),ndf_pppp),ndf_p$PT_DIFF)

testing_data <- all_lineups
predicted_stats <- get_predicted_stats()
ndf_cps <- cbind(ndf_clusters,predicted_stats%>%dplyr::select(-LINEUP))


# do linear regression including interaction terms  + gradratic terms 
#of the number of player types seen in the lineup
ano_lm <- lm(PT_DIFF ~ . + poly(C1, 2) + poly(C2, 2) + poly(C3, 2) + poly(C4, 2) +
               poly(C5, 2) + poly(C6, 2) + poly(C7, 2) + poly(C8, 2) +
               C1:C2 + C1:C3 +  C1:C4 + C1:C5  + C1:C6 + C1:C7 + C1:C8 + C2:C3 + 
               C2:C4 + C2:C5 + C2:C6 + C2:C7 + C2:C8 + C3:C4 + C3:C5 + C3:C6 +
               C3:C7 + C3:C8 +  C4:C5 + C4:C6 + C4:C7 + C4:C8 + C5:C6 +  
               C5:C7 + C5:C8 + C6:C7 + C6:C8 + C7:C8, ndf_clusters%>%dplyr::select(-EFF))
#training co & MSE ||
#.50 and .099 or .1 

#red_ano_lm <- (lm(PT_DIFF ~ . + poly(C1, 2) + poly(C2, 2) + poly(C3, 2) + poly(C4, 2) +
#                    poly(C5, 2) + poly(C6, 2) + poly(C7, 2) + poly(C8, 2) +
#                    C1:C3 +  C1:C4  + C1:C6 + C1:C7 + C1:C8 + C3:C5 + C3:C6 +
#                    C3:C8 + C4:C8 + C5:C6 +  
#                    C5:C7 + C5:C8 + C6:C7 + C6:C8 + C7:C8, ndf_pp%>%dplyr::select(-EFF)))
#plot(predict(red_ano_lm,ndf_full[303:374,]),ndf_full$PT_DIFF[303:374])
#-0.1046816, mse = 0.2058921


# do linear regression including interaction terms  + gradratic terms + predicted statistics
#of the number of player types seen in the lineup
ano_lm2 <- lm(PT_DIFF ~ . + poly(C1, 2) + poly(C2, 2) + poly(C3, 2) + poly(C4, 2) +
                poly(C5, 2) + poly(C6, 2) + poly(C7, 2) + poly(C8, 2) +
                C1:C2 + C1:C3 +  C1:C4 + C1:C5  + C1:C6 + C1:C7 + C1:C8 + C2:C3 + 
                C2:C4 + C2:C5 + C2:C6 + C2:C7 + C2:C8 + C3:C4 + C3:C5 + C3:C6 +
                C3:C7 + C3:C8 +  C4:C5 + C4:C6 + C4:C7 + C4:C8 + C5:C6 +  
                C5:C7 + C5:C8 + C6:C7 + C6:C8 + C7:C8, ndf_cps%>%dplyr::select(-EFF))
#training co & MSE   ||
#.47 and .104


#now trying on testing data
plot(predict(ano_lm,ndf_clusters[303:374,]),ndf_full$PT_DIFF[303:374])
#testing co and MSE
# -.113 and .219     
plot(predict(ano_lm2,ndf_cps[303:374,]),ndf_full$PT_DIFF[303:374])
#testing co and MSE || now when predicting stats
# .262 and 0.1441072





# in testing data, lineup eff still good indicator -> anything that can predict lineup eff 
plot(ndf_full$EFF[303:374],ndf_full$PT_DIFF[303:374])



#things to consider for future groups: outlier treatments
#possibly reduce noise by analyzing differences in TEAM style

# to finish: documentation and packaging and write up results
