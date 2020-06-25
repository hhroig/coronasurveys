
forwardToFriends <- function(surveyRecipients, popSize, myId, reach, fwdProb, fwdFanout){
  
  #sample fwdFanout candidates from reach
  rcpSample <- sample(reach, fwdFanout)
  for (v in rcpSample){
    candidate=((myId + v)  %% popSize ) + 1;# TODO check this
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
  firstProbes <- sample (popSize, numSeeds) 
  surveyRespondents <- c(surveyRespondents, firstProbes)
  
  for (p in firstProbes){
    #seeds always forward the survey
    surveyRecipients <- forwardToFriends(surveyRecipients, popSize, p, reach, fwdProb, fwdFanout)
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
      if (runif(1) <= fwdProb){
        surveyRecipients <- forwardToFriends(surveyRecipients, popSize, myId, reach, fwdProb, fwdFanout)
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
fwdProb=0.05 # probability of forwarding the survey to some friends once you have answered it. 
fwdFanout=20 # number of people each respondent forwards the survey to
numSeeds=20 # number of people that share the survey to begin with. These always respond to and forward the survey with probability 1
