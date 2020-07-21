# create data twitter survey data
survey_twitter_esp <- data.frame(date = c("2020/03/14", "2020/03/16", "2020/03/18"), 
                                 survey_twitter = c((374.05/(762*150))* 46754778, (66.13/(85*150))*46754778,
                                                    (116.16/(120*150))*46754778), stringsAsFactors = F)

# create data twitter survey data
survey_twitter_pt <- data.frame(date = c("2020/03/18", "2020/03/20"), 
                                survey_twitter = c((11/(63*150))*10261075, 15/(45*150)*10261075),
                                stringsAsFactors = F)