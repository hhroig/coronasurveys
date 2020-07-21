
forwardToFriends <- function(surveyRecipients, popSize, myId, knownPeople, fwdProb, fwdFanout){
  
  #sample fwdFanout candidates from reach
  rcpSample <- sample(knownPeople, min(fwdFanout,knownPeople))
  for (v in rcpSample){
    candidate=((myId + v - 1)  %% popSize ) + 1;# myId starts from 1, v starts from 1 and goes up to reach-1 (see calls), v=1 means first friend. 
    if (! is.element(candidate, surveyRecipients)) {# we do not process two invitations
      surveyRecipients <- c(surveyRecipients, candidate)
    }
  }
  
  return (surveyRecipients)
}

correlatedSampling <- function(popSize, requiredSize, reach, numSeeds, ansProb, fwdProb, fwdFanout){
  surveyRecipients <- vector()
  surveyRespondents <- vector()
  # select seeds randomly
  firstProbes <- sample (popSize, min(popSize,numSeeds)) 
  surveyRespondents <- c(surveyRespondents, firstProbes)
  if (reach > 1){
    for (p in firstProbes){
      #seeds always forward the survey
      surveyRecipients <- forwardToFriends(surveyRecipients, popSize, p, reach - 1, fwdProb, fwdFanout)
    }
  }
  
  # print("1-length surveyRecipients ")
  # print(length(surveyRecipients))
  # print("1-length surveyRespondents ")
  # print(length(surveyRespondents))
  # print("1-length firstProbes ")
  # print(length(firstProbes))
  
  i=length(firstProbes)+1;
  while (i <= length(surveyRecipients) && length(surveyRespondents) < requiredSize){
    myId=surveyRecipients[i];
    if (runif(1) <= ansProb){
      surveyRespondents <-c(surveyRespondents, myId)
      if (reach > 1 && runif(1) <= fwdProb){# this reach>1 condition is not necessary because we will never get here if reach=1 because of line 22
        surveyRecipients <- forwardToFriends(surveyRecipients, popSize, myId, reach - 1 , fwdProb, fwdFanout)
      }
    }
    i=i+1 #go to the next recipient
  }
  
  # print("2-length surveyRecipients ")
  # print(length(surveyRecipients))
  # print("2-length surveyRespondents ")
  # print(length(surveyRespondents))
  # print("2-length firstProbes ")
  # print(length(firstProbes))
  
  
#  print("recipients")
#  print(surveyRecipients)
#  print("respondents")
#  print(surveyRespondents)
  return (surveyRespondents)
}


selectProbes <- function (popSize, nResp, replace){
  if (uniform){
    return (sample(popSize, nResp,replace=FALSE))
  } else {
    return(correlatedSampling(popSize, nResp, reach, numSeeds, ansProb, fwdProb, fwdFanout))
  }
}

#default parameter values
ansProb=0.3 # probability of answering a survey you receive from a friend
fwdProb=0.1 # probability of forwarding the survey to some friends once you have answered it. 
fwdFanout=20 # number of people each respondent forwards the survey to
numSeeds=20 # number of people that share the survey to begin with. These always respond to and forward the survey with probability 1
