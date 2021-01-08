

# for analyzing the data from the DDW dataset



DDWData <- read.csv("D:\\DDW Twitter Data\\20190719_TOTAL_DATA_FILE_w_class_LeftOnly.csv")

DDWData$HarmTotal <- DDWData$HarmVice + DDWData$HarmVirtue
DDWData$IngroupTotal <- DDWData$IngroupVice + DDWData$IngroupVirtue
DDWData$FairnessTotal <- DDWData$FairnessVice + DDWData$FairnessVirtue
DDWData$PurityTotal <- DDWData$PurityVice + DDWData$PurityVirtue
DDWData$AuthorityTotal <- DDWData$AuthorityVice + DDWData$AuthorityVirtue


DDWData$HarmTotal_adj <- DDWData$HarmVirtue - DDWData$HarmVice
DDWData$IngroupTotal_adj <- DDWData$IngroupVirtue - DDWData$IngroupVice
DDWData$FairnessTotal_adj <- DDWData$FairnessVirtue - DDWData$FairnessVice
DDWData$PurityTotal_adj <- DDWData$PurityVirtue - DDWData$PurityVice
DDWData$AuthorityTotal_adj <- DDWData$AuthorityVirtue - DDWData$AuthorityVice



###################################################
# Test for correlations between the morality variables
library(dplyr)
DDWData_cor <- data.frame(DDWData$HarmVirtue,
                          DDWData$HarmVice,
                          DDWData$IngroupVirtue,
                          DDWData$IngroupVice,
                          DDWData$FairnessVirtue,
                          DDWData$FairnessVice,
                          DDWData$Morality.General,
                          DDWData$PurityVirtue,
                          DDWData$PurityVice,
                          DDWData$AuthorityVirtue,
                          DDWData$AuthorityVice,
                          DDWData$posemo,
                          DDWData$negemo,
                          DDWData$power,
                          DDWData$retweet_count,
                          DDWData$followers_count,
                          DDWData$i,
                          DDWData$we,
                          DDWData$you,
                          DDWData$anx,
                          DDWData$anger,
                          DDWData$sad,
                          DDWData$affiliation,
                          DDWData$risk,
                          DDWData$reward,
                          DDWData$Security,
                          DDWData$spectrum)

DDWData_cor_auth <- data.frame(DDWData$HarmVirtue,
                          DDWData$HarmVice,
                          DDWData$IngroupVirtue,
                          DDWData$IngroupVice,
                          DDWData$FairnessVirtue,
                          DDWData$FairnessVice,
                          DDWData$Morality.General,
                          DDWData$PurityVirtue,
                          DDWData$PurityVice,
                          DDWData$AuthorityVirtue,
                          DDWData$AuthorityVice,
                          DDWData$power,
                          DDWData$anx,
                          DDWData$anger,
                          DDWData$sad,
                          DDWData$affiliation,
                          DDWData$negemo)

DDWData_cor_auth_adj <- data.frame(DDWData$HarmTotal_adj,
                                   DDWData$IngroupTotal_adj,
                                   DDWData$FairnessTotal_adj,
                                   DDWData$Morality.General,
                                   DDWData$PurityTotal_adj,
                                   DDWData$AuthorityTotal_adj,
                                   DDWData$power,
                                   DDWData$anx,
                                   DDWData$anger,
                                   DDWData$sad,
                                   DDWData$negemo,
                                   DDWData$affiliation,
                                   DDWData$spectrum)


library(Hmisc)
res_ddw_adj <- rcorr(as.matrix(DDWData_cor_auth_adj))
res_ddw <- rcorr(as.matrix(DDWData_cor_auth))
res_ddw


# do correlation plot heatmap stype
library(corrplot)
corrplot(res_ddw$r, type="upper", order="hclust", 
         p.mat = res_ddw$P, sig.level = 0.05, insig = "blank",tl.col = "black", tl.srt = 45)







library(rcompanion)

Sum = groupwiseMean(Steps ~ Sex,
                    data   = Data,
                    conf   = 0.95,
                    digits = 3)



boxplot(DDWData$HarmTotal,
        ylab="Harm Total",
        xlab="")
boxplot(DDWData$IngroupTotal,
        ylab="Ingroup Total",
        xlab="")







# Compute the analysis of variance for harm
res.aov.harm <- aov(HarmTotal ~ search_phrase, data = DDWData)
# Summary of the analysis
summary(res.aov.harm)
TukeyHSD(res.aov.harm)
# Compute the analysis of variance for harm
res.aov.ingroup <- aov(IngroupTotal ~ search_phrase, data = DDWData)
# Summary of the analysis
summary(res.aov.ingroup)
# Compute the analysis of variance for harm
res.aov.fairness <- aov(FairnessTotal ~ search_phrase, data = DDWData)
# Summary of the analysis
summary(res.aov.fairness)
# Compute the analysis of variance for harm
res.aov.purity <- aov(PurityTotal ~ search_phrase, data = DDWData)
# Summary of the analysis
summary(res.aov.purity)
# Compute the analysis of variance for harm
res.aov.authority <- aov(AuthorityTotal ~ search_phrase, data = DDWData)
# Summary of the analysis
summary(res.aov.authority)



library(ggplot2)
library(plyr)
library(reshape2)



melted <- melt(DDWData, id.vars=c("search_phrase", "HarmTotal"))
means <- ddply(melted, c("search_phrase"), summarise,
               mean=mean(HarmTotal))

barplot(means$mean, names.arg = means$search_phrase)

mean(DDWData$HarmTotal)
mean(DDWData$IngroupTotal)
mean(DDWData$FairnessTotal)
mean(DDWData$AuthorityTotal)
mean(DDWData$PurityTotal)

mean(DDWData$HarmTotal_adj)
mean(DDWData$IngroupTotal_adj)
mean(DDWData$FairnessTotal_adj)
mean(DDWData$AuthorityTotal_adj)
mean(DDWData$PurityTotal_adj)




Moral_domains_df <- data.frame(DDWData$HarmTotal,
                               DDWData$IngroupTotal,
                               DDWData$FairnessTotal,
                               DDWData$AuthorityTotal,
                               DDWData$PurityTotal)
md_stack <- stack(Moral_domains_df)

md_aov <- aov( values ~ ind, md_stack)
summary(md_aov)
TukeyHSD(md_aov)


barplot(colMeans(Moral_domains_df))




Moral_domains_df_adj <- data.frame(DDWData$HarmTotal_adj,
                               DDWData$IngroupTotal_adj,
                               DDWData$FairnessTotal_adj,
                               DDWData$AuthorityTotal_adj,
                               DDWData$PurityTotal_adj)


md_stack_adj <- stack(Moral_domains_df_adj)

md_aov_adj <- aov( values ~ ind, md_stack_adj)
summary(md_aov_adj)
TukeyHSD(md_aov_adj)


barplot(colMeans(Moral_domains_df_adj))




library(dplyr)
df <- DDWData %>% group_by(search_phrase) %>% summarise(me <- mean(HarmTotal))

ggplot(aes(x = search_phrase, y = HarmTotal), data = DDWData) + stat_summary(fun.y = "mean", geom = "bar")

ggplot(aes(x = search_phrase, y = HarmTotal_adj), data = DDWData) + stat_summary(fun.y = "mean", geom = "bar")



library(psych)
describeBy(DDWData$HarmTotal_adj, DDWData$search_phrase)
describeBy(DDWData$IngroupTotal_adj, DDWData$search_phrase)
describeBy(DDWData$FairnessTotal_adj, DDWData$search_phrase)
describeBy(DDWData$AuthorityTotal_adj, DDWData$search_phrase)
describeBy(DDWData$PurityTotal_adj, DDWData$search_phrase)
