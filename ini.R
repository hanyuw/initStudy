## Case Study: Hero's initiative
## by moral and initiatives of creatures
## Euler Sauce
## Aug.2nd 2015


## Calcluates for hero's atb gaining from his troops by empathy,
## given his moral (and artificer knowledge of wizards), and spell casted on creatures(haste and benediction)
## Ignoring the influence of artifacts.
## Study on heroes who have the empathy skill (wizards and knights).

## Select whether to report the details for each creature's acting summary.
showTable = FALSE

heroIniReport <- function(faction = "academy", hero = "This hero", moral = 3, knowledge = 34, haste = FALSE, benediction = FALSE, turns = 4){
        print("Please wait for a while...")

        ## The worst case for the moral of a hero with Expert Leadership is -2
        ## (Ring of the Broken Will, Cloak of Death`s Shadow and handicap for academy troops)
        moral = max(moral, -2)  
                
        if(tolower(faction) == "academy"){
                troopSummary = academyTroops(moral, knowledge, haste, turns)
        } else if (tolower(faction) == "aidingacademy"){
                troopSummary = aidingAcademyTroops(moral, knowledge, haste, benediction, turns)
        } else if (tolower(faction) == "heaven"){
                troopSummary = heavenTroops(hero, moral, haste, benediction, turns)
        } else {
                print(paste("Algorithm for", faction, "has not been developed yet", sep = ""))
        }
        
        atbGainedTotal = sum(troopSummary$GoodMoralMean) / 10
        atbGainedAvg = atbGainedTotal / turns
        if (tolower(hero) == "jhora"){
                ## Windspeaker
                heroInitiative = 11.4 + atbGainedAvg * 10
        } else {
                heroInitiative = 10 + atbGainedAvg * 10
        }
        atbGainedSUmmary = paste(hero, " will gain ", digit(atbGainedAvg), " atb bonus through empathy per turn on average,", sep = "")
        heroiniSummary = paste("which is similar to a unit with initiative  = ", digit(heroInitiative), ".", sep = "")
        heroSummary = print(paste(atbGainedSUmmary, heroiniSummary), sep = "")
        invisible (heroInitiative)
}



## Initializing all initiative for academy creatures by assigning heroes's knowledge and moral
academyTroops <- function(moral, knowledge, haste, turns){
        ## maximum artificer increament for initiative is 50%
        if (knowledge > 50){
                knowledge = 50
        }
        
        ## initialize the cases for casting haste
        bonus = 1
        if (haste == TRUE){
                bonus = bonus * 1.4
        }
        
        ## Initialize creatures' initiatives and morals, flying units assumed by affected by stormwind.
        creatureList = data.frame(Name = character(), Initiative = numeric(), Moral = integer(), Turns = numeric(), stringsAsFactors = FALSE)
        creatureList[nrow(creatureList)+1, ] = c("Gremlin", 12*(1+knowledge/100)*bonus, moral+2, turns)
        creatureList[nrow(creatureList)+1, ] = c("Gargoyle", 11*(1+knowledge/100)*0.8*bonus, moral+2, turns)
        creatureList[nrow(creatureList)+1, ] = c("Golem", 7*(1+knowledge/100)*bonus, max(0,moral+2), turns)
        creatureList[nrow(creatureList)+1, ] = c("Mage", 10*(1+knowledge/100)*bonus, moral+2, turns)
        creatureList[nrow(creatureList)+1, ] = c("Djinn ", 12*(1+knowledge/100)*0.8*bonus, moral+2, turns)
        creatureList[nrow(creatureList)+1, ] = c("Rakshasa ", 16*(1+knowledge/100)*bonus, moral+2, turns)
        creatureList[nrow(creatureList)+1, ] = c("Titan", 10*(1+knowledge/100)*bonus, moral+2, turns)
        creatureList[nrow(creatureList)+1, ] = c("Ballista", 10, max(0,moral), turns)
        creatureList[nrow(creatureList)+1, ] = c("Tent", 10, max(0,moral), turns)

        creatureList$Name = as.character(creatureList$Name)
        creatureList$Initiative = as.numeric(creatureList$Initiative)
        creatureList$Moral = as.integer(creatureList$Moral)
        creatureList$Turns = as.numeric(creatureList$Turns)

        actSummary = armyAct(creatureList)
        invisible (actSummary)
}

## Initializing all initiative for academy creatures by assigning heroes's knowledge and Godrics's moral.
## Notice that war machines and golems are not affected by morals (moral is 0).
aidingAcademyTroops <- function(moral, knowledge, haste, benediction, turns){
        ## maximum artificer increament for initiative is 50%
        if (knowledge > 50){
                knowledge = 50
        }
        
        ## initialize the cases for casting haste and benediction
        bonus = 1
        if (haste == TRUE){
                bonus = bonus * 1.4
        }
        
        if (benediction == TRUE){
                bonus = bonus * 1.2
                moral = moral + 2
        }
      
        ## Initialize creatures' initiatives and morals, flying units assumed by affected by stormwind.
        creatureList = data.frame(Name = character(), Initiative = numeric(), Moral = integer(), Turns = numeric(), stringsAsFactors = FALSE)
        creatureList[nrow(creatureList)+1, ] = c("Gremlin", 12*(1+knowledge/100)*bonus, moral+1, turns)
        creatureList[nrow(creatureList)+1, ] = c("Gargoyle", 11*(1+knowledge/100)*0.8*bonus, moral+1, turns)
        creatureList[nrow(creatureList)+1, ] = c("Golem", 7*(1+knowledge/100)*bonus, 0, turns)
        creatureList[nrow(creatureList)+1, ] = c("Mage", 10*(1+knowledge/100)*bonus, moral+1, turns)
        creatureList[nrow(creatureList)+1, ] = c("Djinn ", 12*(1+knowledge/100)*0.8*bonus, moral+1, turns)
        creatureList[nrow(creatureList)+1, ] = c("Rakshasa ", 16*(1+knowledge/100)*bonus, moral+1, turns)
        creatureList[nrow(creatureList)+1, ] = c("Titan", 10*(1+knowledge/100)*bonus, moral+1, turns)
        
        creatureList$Name = as.character(creatureList$Name)
        creatureList$Initiative = as.numeric(creatureList$Initiative)
        creatureList$Moral = as.integer(creatureList$Moral)
        creatureList$Turns = as.numeric(creatureList$Turns)
        
        actSummary = armyAct(creatureList)
        invisible (actSummary)
}

## Initializing all initiative for heaven creatures by assigning heroes's moral.
heavenTroops <- function(hero, moral, haste, benediction, turns){
        ## initialize the cases for casting haste and benediction
        bonus = 1
        if (haste == TRUE){
                if ((tolower(hero) == "meave") ||  (tolower(hero) == "alaric")) {
                        ## Windrider
                        bonus = bonus * 1.68
                } else {
                        bonus = bonus * 1.4
                }
        }
        
        if (benediction == TRUE){
                if (tolower(hero) == "godric"){
                        ## Paragon Knight
                        bonus = bonus * 1.2
                        moral = moral + 2
                } else {
                        bonus = bonus * 1.1
                        moral = moral + 1
                }
        }
        ## Initialize creatures' initiatives and morals, flying units assumed by affected by stormwind.
        creatureList = data.frame(Name = character(), Initiative = numeric(), Moral = integer(), Turns = numeric(), stringsAsFactors = FALSE)
        creatureList[nrow(creatureList)+1, ] = c("Peasant", 8*bonus, moral+2, turns)
        creatureList[nrow(creatureList)+1, ] = c("Archer", 8*bonus, moral+2, turns)
        creatureList[nrow(creatureList)+1, ] = c("Footman", 8*bonus, moral+2, turns)
        creatureList[nrow(creatureList)+1, ] = c("Griffin", 15**0.8*bonus, moral+2, turns)
        creatureList[nrow(creatureList)+1, ] = c("Priest ", 10*bonus, moral+2, turns)
        creatureList[nrow(creatureList)+1, ] = c("Champion ", 12*bonus, moral+2, turns)
        creatureList[nrow(creatureList)+1, ] = c("Angel", 11**0.8*bonus, moral+2, turns) 
        
        creatureList$Name = as.character(creatureList$Name)
        creatureList$Initiative = as.numeric(creatureList$Initiative)
        creatureList$Moral = as.integer(creatureList$Moral)
        creatureList$Turns = as.numeric(creatureList$Turns)
        
        actSummary = armyAct(creatureList)
        invisible (actSummary)
}

## Converting the creatures' list to individual ones,  
## then stimulate them indirectly.
armyAct <- function(creaturesList) {
        actSummary = data.frame(Name = character(), GoodMoral = numeric(), Acts = numeric())
        for (i in 1:nrow(creaturesList)){
                creatureMoral = creaturesList[i,]$Moral
                creatureInitiative = creaturesList[i,]$Initiative
                creatureName = creaturesList[i,]$Name
                turns = creaturesList[i,]$Turns
                creatureReport = actMean(creatureName, creatureInitiative, creatureMoral, turns, showTable) 
                actSummary = rbind(actSummary, creatureReport) 
        }
        invisible (actSummary)
}


## Stimulate 1000 times to get the mean and standard deviation 
## for the count of the acts and good morals of a single creature.
actMean <- function(name = "This creature ", initiative, moral, totalTurns = 3.5, summary = TRUE){
        MAcount = data.frame(CreatureName = character(), GoodMoralCount = numeric(), TotalActs = numeric())
        for (i in 1:1000){
                MAsingle = moralCount(moral, initiative, totalTurns)
                MAcount = rbind(MAcount, MAsingle)
        }
        goodMoralMeanTotal = mean(MAcount$GoodMoralCount)
        goodMoralMeanEachTurn = goodMoralMeanTotal / totalTurns
        actsMeanTotal = mean(MAcount$TotalActs)
        actsMeanEachTurn = actsMeanTotal / totalTurns
        creatureini = actsMeanEachTurn * 10

        ## Print the summary for each unit.        
        actSummary = paste(name, " can act ", digit(actsMeanTotal), " times in ", totalTurns, " turns on average", sep = "")
        moralSummary = paste(", and ", digit(goodMoralMeanTotal), " times for good morals, respectively.", sep = "")
        meanSummary = paste(" Gerenally speaking, ", name, " can act ", digit(actsMeanEachTurn), " times and ", digit(goodMoralMeanEachTurn), " good morals per turn.", sep = "")
        iniSummary = paste(" Its actual initiative is ", digit(creatureini), ".")
        conditionSummary = paste(" Under the condition that moral is ", moral, " ,initiative is ", initiative, ".", sep = "")
        if (summary){
                print(paste(actSummary, moralSummary, conditionSummary, meanSummary, iniSummary, sep = ""))
        }
        MoralMeanSummary = data.frame(Name = name, GoodMoralMean = goodMoralMeanTotal, ActsMean = actsMeanTotal)
        
        ## For reference of statistical use of getting standard deviation:
        ## MASDSummary = data.frame(goodMoralSD = sd(MAcount$goodMoralCount), totalActsSD = sd(MAcount$totalActs))
        
        invisible (MoralMeanSummary)
}

## Simulating a creature's acting track under given turns
moralCount <- function(moral, initiative, totalTurns){
        moralBar = c()
        acts = 0
        atb = runif(1,0,0.25)                          ## inital atb that was assigned to the creature
        turns = atb                                    ## total turns passed
        atbConsume = (1 - atb) * 10 / initiative       ## first round Counting
        turns = turns + atbConsume                     ## first round Counting
        
        while(turns <= totalTurns){
                ## For monitoring the entire approach:
                ## print(paste("current atb:", atb, "atb Consumed to 1:", atbConsume, "Turns:", turns, "acts:", acts))
                ## print(moralBar)
                
                acts = acts + 1
                
                ## moral incident
                if (goodMoral(moral) == TRUE){
                        atb = 0.5
                        moralBar = c(moralBar, TRUE)
                } else {
                        atb = 0
                        moralBar = c(moralBar, FALSE)
                }
                atbConsume = (1 - atb) * 10 / initiative
                turns = turns + atbConsume
                
        }
        
        ## counting for the good moral
        morals = moralBar[1:length(moralBar)-1]
        posMoral = morals[morals == TRUE]
        posMoralCount = length(posMoral)
        
        ## For monitoring the summary:
        ## print(paste("total good morals is:", posMoralCount))
        ## print(paste("total acts is:", acts))
        ## print(paste("total turns is:", turns))
        
        ## Returns the count for the acts and good morals
        moralSummary = data.frame(GoodMoralCount = posMoralCount, TotalActs = acts)
        invisible (moralSummary)
}


## Generates a moral incident by given moral
## return true when good moral is triggered, false otherwise
goodMoral <- function (moral){
        moral = min(moral/10, 0.5)
        ## maximum possibility for good moral is 0.5
        
        x = runif(1)
        ## print(x)
        if (x > moral) {
                return (FALSE)
        } else {
                return (TRUE)
        }
}

## Rounding the given figure to 2 digits.
digit <- function (x){
        return (round(x, digit = 2))
}