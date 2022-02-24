rm(list=ls())
#The next 3 lines open the 3 important dataframes for this project
AC_Pops <- read.csv("AC_Species_Populations.csv") #This dataframe contains the population of each species at each major "Animal Crossing" installment
AC_Pred_Chart <- read.csv("AC_Predation_Chart.csv") #This dataframe records the predatory relationships of the columns with respect to the rows (the columns eat rows labelled "1")
AC_Comp_Matrix <- read.csv("AC_Competition_Matrix.csv") #This dataframe records the competitive interactions between species. A 1 indicates competition between the two species
AC_Pops_fixed <- AC_Pops[-38] #An extra column of only NAs was added for some reason so I dropped that
AC_Pops_repaired <- AC_Pops_fixed[-c(2),] #Removes the first game of the series (the second game is also from 2001 and has near identical population counts)
AC_Pops_finalized <- AC_Pops_repaired[,-c(1,2,12,20,26)] #Removed year/total columns for species parameter calculations, and dropped "Deer," "Hamster," and "Monkey" because the model is not equipped to deal with immigration/speciation

#The following code assigns the initial population for each species to a variable (N0), a vector in which the resulting values for all time steps will be placed (N), and a working vector in which the value for each time step is to be stored (Nnow)
#There probably is a more efficient way to do this in which row 1 is pulled from the "AC_Pops" dataframe, but time prevented me from learning how to do it
N0_alligator <- 4
N_alligator <- N0_alligator
Nnow_alligator <- N0_alligator
Nnow2_alligator <- N0_alligator
N0_anteater <- 5
N_anteater <- N0_anteater
Nnow_anteater <- N0_anteater
Nnow2_anteater <- N0_anteater
N0_bear <- 9
N_bear <- N0_bear
Nnow_bear <- N0_bear
Nnow2_bear <- N0_bear
N0_bird <- 10
N_bird <- N0_bird
Nnow_bird <- N0_bird
Nnow2_bird <- N0_bird
N0_bull <- 4
N_bull <- N0_bull
Nnow_bull <- N0_bull
Nnow2_bull <- N0_bull
N0_cat <- 13
N_cat <- N0_cat
Nnow_cat <- N0_cat
Nnow2_cat <- N0_cat
N0_chicken <- 8
N_chicken <- N0_chicken
Nnow_chicken <- N0_chicken
Nnow2_chicken <- N0_chicken
N0_cow <- 4
N_cow <- N0_cow
Nnow_cow <- N0_cow
Nnow2_cow <- N0_cow
N0_cub <- 9
N_cub <- N0_cub
Nnow_cub <- N0_cub
Nnow2_cub <- N0_cub
N0_dog <- 10
N_dog <- N0_dog
Nnow_dog <- N0_dog
Nnow2_dog <- N0_dog
N0_duck <- 10
N_duck <- N0_duck
Nnow_duck <- N0_duck
Nnow2_duck <- N0_duck
N0_eagle <- 5
N_eagle <- N0_eagle
Nnow_eagle <- N0_eagle
Nnow2_eagle <- N0_eagle
N0_elephant <- 6
N_elephant <- N0_elephant
Nnow_elephant <- N0_elephant
Nnow2_elephant <- N0_elephant
N0_frog <- 12
N_frog <- N0_frog
Nnow_frog <- N0_frog
Nnow2_frog <- N0_frog
N0_goat <- 6
N_goat <- N0_goat
Nnow_goat <- N0_goat
Nnow2_goat <- N0_goat
N0_gorilla <- 4
N_gorilla <- N0_gorilla
Nnow_gorilla <- N0_gorilla
Nnow2_gorilla <- N0_gorilla
N0_hippo <- 7
N_hippo <- N0_hippo
Nnow_hippo <- N0_hippo
Nnow2_hippo <- N0_hippo
N0_horse <- 7
N_horse <- N0_horse
Nnow_horse <- N0_horse
Nnow2_horse <- N0_horse
N0_kangaroo <- 6
N_kangaroo <- N0_kangaroo
Nnow_kangaroo <- N0_kangaroo
Nnow2_kangaroo <- N0_kangaroo
N0_koala <- 5
N_koala <- N0_koala
Nnow_koala <- N0_koala
Nnow2_koala <- N0_koala
N0_lion <- 3
N_lion <- N0_lion
Nnow_lion <- N0_lion
Nnow2_lion <- N0_lion
N0_mouse <- 9
N_mouse <- N0_mouse
Nnow_mouse <- N0_mouse
Nnow2_mouse <- N0_mouse
N0_octopus <- 1
N_octopus <- N0_octopus
Nnow_octopus <- N0_octopus
Nnow2_octopus <- N0_octopus
N0_ostrich <- 4
N_ostrich <- N0_ostrich
Nnow_ostrich <- N0_ostrich
Nnow2_ostrich <- N0_ostrich
N0_penguin <- 7
N_penguin <- N0_penguin
Nnow_penguin <- N0_penguin
Nnow2_penguin <- N0_penguin
N0_pig <- 10
N_pig <- N0_pig
Nnow_pig <- N0_pig
Nnow2_pig <- N0_pig
N0_rabbit <- 10
N_rabbit <- N0_rabbit
Nnow_rabbit <- N0_rabbit
Nnow2_rabbit <- N0_rabbit
N0_rhino <- 4
N_rhino <- N0_rhino
Nnow_rhino <- N0_rhino
Nnow2_rhino <- N0_rhino
N0_sheep <- 6
N_sheep <- N0_sheep
Nnow_sheep <- N0_sheep
Nnow2_sheep <- N0_sheep
N0_squirrel <- 10
N_squirrel <- N0_squirrel
Nnow_squirrel <- N0_squirrel
Nnow2_squirrel <- N0_squirrel
N0_tiger <- 3
N_tiger <- N0_tiger
Nnow_tiger <- N0_tiger
Nnow2_tiger <- N0_tiger
N0_wolf <- 5
N_wolf <- N0_wolf
Nnow_wolf <- N0_wolf
Nnow2_wolf <- N0_wolf

#The next line subtracts calculates the birthrate for each species by subtracting each species' 2020 population from their initial population, dividing this by the initial population, and then dividing this by the amount of time passed
#Ideally birth rates would be available before the initial population counts, as these birth rates are calculated from the period in which I'm testing my model
AC_Birth_Rates <- (((AC_Pops_finalized[7,]-AC_Pops_finalized[1,])/AC_Pops_finalized[1,])/19) + 1

#The next line of code assigns a proportionality factor of predation to all species. This factor represents the relative resource gain when a hunt is successful
#3 values were decided based on approximate species body mass, 0.1 (high gain for the predator), 0.05 (moderate gain for the predator), and 0.01 (low gain for the predator)
m <- c(0.1, 0.05, 0.1, 0.01, 0.1, 0.01, 0.01, 0.1, 0.01, 0.05, 0.01, 0.01, 0.1, 0.01, 0.05, 0.1, 0.1, 0.1, 0.05, 0.01, 0.1, 0.01, 0.01, 0.1, 0.01, 0.05, 0.01, 0.1, 0.05, 0.01, 0.1, 0.1)

#The next line of code assigns a proportionality factor of competition to all species. This factor represents the proportional effect of resource loss when a species competes with others
#2 values were decided based on general knowledge of whether species are specialists (high impact from competition, c = 0.05) or generalists (low impact from competition, c = 0.01)
c <- c(0.05, 0.05, 0.01, 0.01, 0.05, 0.05, 0.01, 0.05, 0.01, 0.01, 0.05, 0.05, 0.01, 0.05, 0.01, 0.01, 0.05, 0.05, 0.01, 0.05, 0.05, 0.01, 0.05, 0.01, 0.05, 0.01, 0.01, 0.05, 0.05, 0.01, 0.05, 0.01)

#The next line represents the likelihood of a hunt being successful.
#The values are based on a general knowledge of success in large mammal hunts, and the rates vary based on a defense score assigned to each species (also based on general knowlege)
b <- c(0.05, 0.25, 0.05, 0.25, 0.05, 0.25, 0.25, 0.05, 0.25, 0.15, 0.25, 0.15, 0.05, 0.25, 0.15, 0.15, 0.05, 0.05, 0.15, 0.25, 0.05, 0.25, 0.25, 0.15, 0.25, 0.15, 0.25, 0.05, 0.15, 0.25, 0.05, 0.05)

#The next variable is the final parameter for the model, alpha. It represents the chance that a resource being competed for will be lost in when two competing animals meet.
#This was calcuated to be 0.08 from the rate of resource generation in the game's data files. It is assumed that all competing animals have an equal chance of aquiring a resource when competing.
a = 0.08

#Set the time of the simulation to 20 because I want to compare my model to the 20 years for which I have data
t=20

#This for loop runs the simulation. For each species there are 3 lines:
#Line 1: Save the population at time step t+1 as Nnow_animal. This line involves adding/subtracting all terms that represent population growth/loss. Note that Nnow2_animal is used within this line so that later animals that interact with the given species don't have their t+1 population counted with the other animal's t+1 population
#Line 2: Ensure that no species' populations can drop below 0
#Line 3: Add the newly calculated population vector to the end of the vector holding all population values
#After these 3 lines are run for every species Nnow2_animal's value is replaced with the newly calculated population value for every species
for(i in 1:t) {
  Nnow_alligator <- (AC_Birth_Rates$Alligator*Nnow2_alligator) + (m[4]*b[4]*Nnow2_alligator*Nnow2_bird) + (m[6]*b[6]*Nnow2_alligator*Nnow2_cat) + (m[7]*b[7]*Nnow2_alligator*Nnow2_chicken) + (m[9]*b[9]*Nnow2_alligator*Nnow2_cub) + (m[10]*b[10]*Nnow2_alligator*Nnow2_dog) + (m[11]*b[11]*Nnow2_alligator*Nnow2_duck) + (m[12]*b[12]*Nnow2_alligator*Nnow2_eagle) + (m[14]*b[14]*Nnow2_alligator*Nnow2_frog) + (m[15]*b[15]*Nnow2_alligator*Nnow2_goat) + (m[19]*b[19]*Nnow2_alligator*Nnow2_kangaroo) + (m[20]*b[20]*Nnow2_alligator*Nnow2_koala) + (m[22]*b[22]*Nnow2_alligator*Nnow2_mouse) + (m[23]*b[23]*Nnow2_alligator*Nnow2_octopus) + (m[25]*b[25]*Nnow2_alligator*Nnow2_penguin) + (m[26]*b[26]*Nnow2_alligator*Nnow2_pig) + (m[27]*b[27]*Nnow2_alligator*Nnow2_rabbit) + (m[29]*b[29]*Nnow2_alligator*Nnow2_sheep) + (m[30]*b[30]*Nnow2_alligator*Nnow2_squirrel) - (c[3]*a*Nnow2_alligator*Nnow2_bear) - (c[6]*a*Nnow2_alligator*Nnow2_cat) - (c[10]*a*Nnow2_alligator*Nnow2_dog) - (c[12]*a*Nnow2_alligator*Nnow2_eagle) - (c[21]*a*Nnow2_alligator*Nnow2_lion) - (c[25]*a*Nnow2_alligator*Nnow2_penguin) - (c[31]*a*Nnow2_alligator*Nnow2_tiger) - (c[32]*a*Nnow2_alligator*Nnow2_wolf)
  if(Nnow_alligator < 0) {Nnow_alligator <- 0}
  N_alligator <- c(N_alligator, Nnow_alligator)
  Nnow_anteater <- (AC_Birth_Rates$Anteater*Nnow2_anteater) - (c[4]*a*Nnow2_anteater*Nnow2_bird) - (c[7]*a*Nnow2_anteater*Nnow2_chicken) - (c[9]*a*Nnow2_anteater*Nnow2_cub) - (c[14]*a*Nnow2_anteater*Nnow2_frog) - (m[2]*b[2]*Nnow2_anteater*Nnow2_bear) - (m[2]*b[2]*Nnow2_anteater*Nnow2_lion) - (m[2]*b[2]*Nnow2_anteater*Nnow2_tiger) - (m[2]*b[2]*Nnow2_anteater*Nnow2_wolf)
  if(Nnow_anteater < 0) {Nnow_anteater <- 0}
  N_anteater <- c(N_anteater, Nnow_anteater)
  Nnow_bear <- (AC_Birth_Rates$Bear*Nnow2_bear) + (m[2]*b[2]*Nnow2_bear*Nnow2_anteater) + (m[4]*b[4]*Nnow2_bear*Nnow2_bird) + (m[6]*b[6]*Nnow2_bear*Nnow2_cat) + (m[7]*b[7]*Nnow2_bear*Nnow2_chicken) + (m[9]*b[9]*Nnow2_bear*Nnow2_cub) + (m[10]*b[10]*Nnow2_bear*Nnow2_dog) + (m[11]*b[11]*Nnow2_bear*Nnow2_duck) + (m[12]*b[12]*Nnow2_bear*Nnow2_eagle) + (m[14]*b[14]*Nnow2_bear*Nnow2_frog) + (m[15]*b[15]*Nnow2_bear*Nnow2_goat) + (m[19]*b[19]*Nnow2_kangaroo*Nnow2_koala) + (m[22]*b[22]*Nnow2_bear*Nnow2_mouse) + (m[23]*b[23]*Nnow2_octopus*Nnow2_bear) + (m[24]*b[24]*Nnow2_bear*Nnow2_ostrich) + (m[25]*b[25]*Nnow2_bear*Nnow2_penguin) + (m[26]*b[26]*Nnow2_bear*Nnow2_pig) + (m[27]*b[27]*Nnow2_bear*Nnow2_rabbit) + (m[29]*b[29]*Nnow2_bear*Nnow2_sheep) + (m[30]*b[30]*Nnow2_bear*Nnow2_squirrel) - (c[1]*a*Nnow2_alligator*Nnow2_bear) - (c[6]*a*Nnow2_bear*Nnow2_cat) - (c[9]*a*Nnow2_bear*Nnow2_cub) - (c[10]*a*Nnow2_bear*Nnow2_dog) - (c[12]*a*Nnow2_bear*Nnow2_eagle) - (c[16]*a*Nnow2_bear*Nnow2_gorilla) - (c[21]*a*Nnow2_bear*Nnow2_lion) - (c[25]*a*Nnow2_bear*Nnow2_penguin) - (c[26]*a*Nnow2_pig*Nnow2_bear) - (c[31]*a*Nnow2_tiger*Nnow2_bear) - (c[32]*a*Nnow2_bear*Nnow2_wolf)
  if(Nnow_bear < 0) {Nnow_bear <- 0}
  N_bear <- c(N_bear, Nnow_bear)
  Nnow_bird <- (AC_Birth_Rates$Bird*Nnow2_bird) - (c[2]*a*Nnow2_bird*Nnow2_anteater) - (c[7]*a*Nnow2_bird*Nnow2_chicken) - (c[9]*a*Nnow2_bird*Nnow2_cub) - (c[14]*a*Nnow2_bird*Nnow2_frog) - (c[22]*a*Nnow2_bird*Nnow2_mouse) - (c[27]*a*Nnow2_bird*Nnow2_rabbit) - (c[30]*a*Nnow2_bird*Nnow2_squirrel) - (m[4]*b[4]*Nnow2_bird*Nnow2_alligator) - (m[4]*b[4]*Nnow2_bird*Nnow2_bear) - (m[4]*b[4]*Nnow2_bird*Nnow2_cat) - (m[4]*b[4]*Nnow2_bird*Nnow2_cub) - (m[4]*b[4]*Nnow2_bird*Nnow2_dog) - (m[4]*b[4]*Nnow2_bird*Nnow2_eagle) - (m[4]*b[4]*Nnow2_bird*Nnow2_lion) - (m[4]*b[4]*Nnow2_bird*Nnow2_tiger) - (m[4]*b[4]*Nnow2_bird*Nnow2_wolf)
  if(Nnow_bird < 0) {Nnow_bird <- 0}
  N_bird <- c(N_bird, Nnow_bird)
  Nnow_bull <- (AC_Birth_Rates$Bull*Nnow2_bull) - (c[8]*a*Nnow2_bull*Nnow2_cow) - (c[13]*a*Nnow2_bull*Nnow2_elephant) - (c[15]*a*Nnow2_bull*Nnow2_goat) - (c[17]*a*Nnow2_bull*Nnow2_hippo) - (c[18]*a*Nnow2_horse*Nnow2_bull) - (c[19]*a*Nnow2_kangaroo*Nnow2_bull) - (c[24]*a*Nnow2_bull*Nnow2_ostrich) - (c[26]*a*Nnow2_bull*Nnow2_pig) - (c[27]*a*Nnow2_bull*Nnow2_rabbit) - (c[28]*a*Nnow2_bull*Nnow2_rhino) - (c[29]*Nnow2_bull*Nnow2_sheep) - (m[5]*b[5]*Nnow2_bull*Nnow2_wolf)
  if(Nnow_bull < 0) {Nnow_bull <- 0}
  N_bull <- c(N_bull, Nnow_bull)
  Nnow_cat <- (AC_Birth_Rates$Cat*Nnow2_cat) + (m[4]*b[4]*Nnow2_cat*Nnow2_bird) + (m[14]*b[14]*Nnow2_cat*Nnow2_frog) + (m[22]*b[22]*Nnow2_cat*Nnow2_mouse) + (m[23]*b[23]*Nnow2_octopus*Nnow2_cat) + (m[27]*b[27]*Nnow2_cat*Nnow2_rabbit) + (m[30]*b[30]*Nnow2_cat*Nnow2_squirrel) - (m[6]*b[6]*Nnow2_cat*Nnow2_alligator) - (m[6]*b[6]*Nnow2_cat*Nnow2_bear) - (m[6]*b[6]*Nnow2_cat*Nnow2_dog) - (m[6]*b[6]*Nnow2_cat*Nnow2_eagle) - (m[6]*b[6]*Nnow2_cat*Nnow2_lion) - (m[6]*b[6]*Nnow2_cat*Nnow2_tiger) - (m[6]*b[6]*Nnow2_cat*Nnow2_wolf) - (c[1]*a*Nnow2_cat*Nnow2_alligator) - (c[3]*a*Nnow2_cat*Nnow2_bear) - (c[9]*a*Nnow2_cub*Nnow2_cat) - (c[10]*a*Nnow2_dog*Nnow2_cat) - (c[12]*a*Nnow2_cat*Nnow2_eagle) - (c[21]*a*Nnow2_cat*Nnow2_lion) - (c[25]*a*Nnow2_cat*Nnow2_penguin) - (c[31]*a*Nnow2_cat*Nnow2_tiger) - (c[32]*a*Nnow2_cat*Nnow2_wolf)
  if(Nnow_cat < 0) {Nnow_cat <- 0}
  N_cat <- c(N_cat, Nnow_cat)
  Nnow_chicken <- (AC_Birth_Rates$Chicken*Nnow2_chicken) - (c[2]*a*Nnow2_chicken*Nnow2_anteater) - (c[4]*a*Nnow2_chicken*Nnow2_bird) - (c[14]*a*Nnow2_chicken*Nnow2_frog) - (c[22]*a*Nnow2_chicken*Nnow2_mouse) - (c[27]*a*Nnow2_chicken*Nnow2_rabbit) - (c[30]*a*Nnow2_chicken*Nnow2_squirrel) - (m[7]*b[7]*Nnow2_chicken*Nnow2_alligator) - (m[7]*b[7]*Nnow2_chicken**Nnow2_bear) - (m[7]*b[7]*Nnow2_chicken*Nnow2_dog) - (m[7]*b[7]*Nnow2_chicken*Nnow2_eagle) - (m[7]*b[7]*Nnow2_chicken*Nnow2_lion) - (m[7]*b[7]*Nnow2_chicken*Nnow2_tiger) - (m[7]*b[7]*Nnow2_chicken*Nnow2_wolf)
  if(Nnow_chicken < 0) {Nnow_chicken <- 0}
  N_chicken <- c(N_chicken, Nnow_chicken)
  Nnow_cow <- (AC_Birth_Rates$Cow*Nnow2_cow) - (c[5]*a*Nnow2_cow*Nnow2_bull) - (c[13]*a*Nnow2_cow*Nnow2_elephant) - (c[15]*a*Nnow2_goat*Nnow2_cow) - (c[17]*a*Nnow2_cow*Nnow2_goat) - (c[18]*a*Nnow2_cow*Nnow2_horse) - (c[19]*a*Nnow2_cow*Nnow2_kangaroo) - (c[24]*a*Nnow2_cow*Nnow2_ostrich) - (c[26]*a*Nnow2_pig*Nnow2_cow) - (c[27]*a*Nnow2_rabbit*Nnow2_cow) - (c[28]*a*Nnow2_cow*Nnow2_rhino) - (c[29]*a*Nnow2_cow*Nnow2_sheep) - (m[8]*b[8]*Nnow2_cow*Nnow2_wolf)
  if(Nnow_cow < 0) {Nnow_cow <- 0}
  N_cow <- c(N_cow, Nnow_cow)
  Nnow_cub <- (AC_Birth_Rates$Cub*Nnow2_cub) + (m[4]*b[4]*Nnow2_cub*Nnow2_bird) + (m[14]*b[14]*Nnow2_cub*Nnow2_frog) + (m[22]*b[22]*Nnow2_cub*Nnow2_mouse) + (m[23]*b[23]*Nnow2_octopus*Nnow2_cub) + (m[27]*b[27]*Nnow2_rabbit*Nnow2_cub) + (m[30]*b[30]*Nnow2_cub*Nnow2_squirrel) - (m[9]*b[9]*Nnow2_cub*Nnow2_alligator) - (m[9]*b[9]*Nnow2_cub*Nnow2_bear) - (m[9]*b[9]*Nnow2_cub*Nnow2_dog) - (m[9]*b[9]*Nnow2_cub*Nnow2_eagle) - (m[9]*b[9]*Nnow2_cub*Nnow2_lion) - (m[9]*b[9]*Nnow2_cub*Nnow2_tiger) - (m[9]*b[9]*Nnow2_cub*Nnow2_wolf) - (c[1]*a*Nnow2_cub*Nnow2_alligator) - (c[2]*a*Nnow2_cub*Nnow2_anteater) - (c[3]*a*Nnow2_cub*Nnow2_bear) - (c[4]*a*Nnow2_cub*Nnow2_bird) - (c[6]*a*Nnow2_cub*Nnow2_cat) - (c[10]*a*Nnow2_cub*Nnow2_eagle) - (c[16]*a*Nnow2_cub*Nnow2_gorilla) - (c[25]*a*Nnow2_cub*Nnow2_penguin) - (c[26]*a*Nnow2_cub*Nnow2_pig) - (c[31]*a*Nnow2_cub*Nnow2_tiger) - (c[32]*a*Nnow2_wolf*Nnow2_cub)
  if(Nnow_cub < 0) {Nnow_cub <- 0}
  N_cub <- c(N_cub, Nnow_cub)
  Nnow_dog <- (AC_Birth_Rates$Dog*Nnow2_dog) + (m[4]*b[4]*Nnow2_dog*Nnow2_bird) + (m[6]*b[6]*Nnow2_dog*Nnow2_cat) + (m[7]*b[7]*Nnow2_dog*Nnow2_chicken) + (m[9]*b[9]*Nnow2_dog*Nnow2_cub) + (m[11]*b[11]*Nnow2_dog*Nnow2_duck) + (m[14]*b[14]*Nnow2_dog*Nnow2_frog) + (m[20]*b[20]*Nnow2_dog*Nnow2_koala) + (m[22]*b[22]*Nnow2_dog*Nnow2_mouse) + (m[23]*b[23]*Nnow2_octopus*Nnow2_dog) + (m[25]*b[25]*Nnow2_penguin*Nnow2_dog) + (m[27]*b[27]*Nnow2_dog*Nnow2_rabbit) + (m[30]*b[30]*Nnow2_dog*Nnow2_squirrel) - (m[10]*b[10]*Nnow2_dog*Nnow2_alligator) - (m[10]*b[10]*Nnow2_dog*Nnow2_bear) - (m[10]*b[10]*Nnow2_dog*Nnow2_lion) - (m[10]*b[10]*Nnow2_dog*Nnow2_tiger) - (m[10]*b[10]*Nnow2_dog*Nnow2_wolf) - (c[1]*a*Nnow2_dog*Nnow2_alligator) - (c[3]*a*Nnow2_dog*Nnow2_cat) - (c[6]*a*Nnow2_dog*Nnow2_cat) - (c[9]*a*Nnow2_dog*Nnow2_cub) - (c[12]*a*Nnow2_dog*Nnow2_eagle) - (c[21]*a*Nnow2_dog*Nnow2_lion) - (c[25]*a*Nnow2_dog*Nnow2_penguin) - (c[26]*a*Nnow2_dog*Nnow2_pig) - (c[31]*a*Nnow2_dog*Nnow2_tiger) - (c[32]*a*Nnow2_dog*Nnow2_wolf)
  if(Nnow_dog < 0) {Nnow_dog <- 0}
  N_dog <- c(N_dog, Nnow_dog)
  Nnow_duck <- (AC_Birth_Rates$Duck*Nnow2_duck) - (c[17]*a*Nnow2_duck*Nnow2_hippo) - (m[11]*b[11]*Nnow2_duck*Nnow2_alligator) - (m[11]*b[11]*Nnow2_duck*Nnow2_bear) - (m[11]*b[11]*Nnow2_duck*Nnow2_dog) - (m[11]*b[11]*Nnow2_duck*Nnow2_eagle) - (m[11]*b[11]*Nnow2_duck*Nnow2_lion) - (m[11]*b[11]*Nnow2_duck*Nnow2_tiger) - (m[11]*b[11]*Nnow2_duck*Nnow2_wolf)
  if(Nnow_duck < 0) {Nnow_duck <- 0}
  N_duck <- c(N_duck, Nnow_duck)
  Nnow_eagle <- (AC_Birth_Rates$Eagle*Nnow2_eagle) + (m[4]*b[4]*Nnow2_eagle*Nnow2_bird) + (m[6]*b[6]*Nnow2_eagle*Nnow2_cat) + (m[7]*b[7]*Nnow2_eagle*Nnow2_chicken) + (m[9]*b[9]*Nnow2_eagle*Nnow2_cub) + (m[11]*b[11]*Nnow2_eagle*Nnow2_duck) + (m[14]*b[14]*Nnow2_eagle*Nnow2_frog) + (m[20]*b[20]*Nnow2_eagle*Nnow2_koala) + (m[22]*b[22]*Nnow2_eagle*Nnow2_mouse) + (m[23]*b[23]*Nnow2_eagle*Nnow2_octopus) + (m[27]*b[27]*Nnow2_eagle*Nnow2_rabbit) + (m[30]*b[30]*Nnow2_eagle*Nnow2_squirrel) - (m[12]*b[12]*Nnow2_eagle*Nnow2_alligator) - (m[12]*b[12]*Nnow2_eagle*Nnow2_bear) - (m[12]*b[12]*Nnow2_eagle*Nnow2_lion) - (m[12]*b[12]*Nnow2_eagle*Nnow2_tiger) - (m[12]*b[12]*Nnow2_eagle*Nnow2_wolf) - (c[1]*a*Nnow2_eagle*Nnow2_alligator) - (c[3]*a*Nnow2_eagle*Nnow2_bear) - (c[6]*a*Nnow2_eagle*Nnow2_cat) - (c[9]*a*Nnow2_eagle*Nnow2_cub) - (c[10]*a*Nnow2_eagle*Nnow2_dog) - (c[21]*a*Nnow2_eagle*Nnow2_lion) - (c[25]*a*Nnow2_eagle*Nnow2_penguin) - (c[31]*a*Nnow2_eagle*Nnow2_tiger) - (c[32]*a*Nnow2_eagle*Nnow2_wolf)
  if(Nnow_eagle < 0) {Nnow_eagle <- 0}
  N_eagle <- c(N_eagle, Nnow_eagle)
  Nnow_elephant <- (AC_Birth_Rates$Elephant*Nnow2_elephant) - (c[5]*a*Nnow2_elephant*Nnow2_bull) - (c[8]*a*Nnow2_elephant*Nnow2_cow) - (c[15]*a*Nnow2_elephant*Nnow2_goat) - (c[17]*a*Nnow2_elephant*Nnow2_hippo) - (c[18]*a*Nnow2_elephant*Nnow2_horse) - (c[19]*a*Nnow2_elephant*Nnow2_kangaroo) - (c[24]*a*Nnow2_ostrich*Nnow2_elephant) - (c[26]*a*Nnow2_pig*Nnow2_elephant) - (c[27]*a*Nnow2_elephant*Nnow2_rabbit) - (c[28]*a*Nnow2_elephant*Nnow2_rhino) - (c[29]*a*Nnow2_elephant*Nnow2_rhino)
  if(Nnow_elephant < 0) {Nnow_elephant <- 0}
  N_elephant <- c(N_elephant, Nnow_elephant)
  Nnow_frog <- (AC_Birth_Rates$Frog*Nnow2_frog) - (c[2]*a*Nnow2_anteater*Nnow2_frog) - (c[4]*a*Nnow2_frog*Nnow2_bird) - (c[7]*a*Nnow2_frog*Nnow2_chicken) - (m[14]*b[14]*Nnow2_frog*Nnow_alligator) - (m[14]*b[14]*Nnow2_frog*Nnow2_bear) - (m[14]*b[14]*Nnow2_frog**Nnow2_cat) - (m[14]*b[14]*Nnow2_frog*Nnow2_cub) - (m[14]*b[14]*Nnow2_frog*Nnow2_dog) - (m[14]*b[14]*Nnow2_frog*Nnow2_eagle) - (m[14]*b[14]*Nnow2_frog*Nnow2_lion) - (m[14]*b[14]*Nnow2_frog*Nnow2_tiger) - (m[14]*b[14]*Nnow2_frog*Nnow2_wolf)
  if(Nnow_frog < 0) {Nnow_frog <- 0}
  N_frog <- c(N_frog, Nnow_frog)
  Nnow_goat <- (AC_Birth_Rates$Goat*Nnow2_goat) - (c[5]*a*Nnow2_goat*Nnow2_bull) - (c[8]*a*Nnow2_goat*Nnow2_cow) - (c[13]*a*Nnow2_goat*Nnow2_elephant) - (c[17]*a*Nnow2_goat*Nnow2_hippo) - (c[18]*a*Nnow2_goat*Nnow2_horse) - (c[19]*a*Nnow2_goat*Nnow2_kangaroo) - (c[24]*a*Nnow2_goat*Nnow2_ostrich) - (c[26]*a*Nnow2_goat*Nnow2_pig) - (c[27]*a*Nnow2_goat*Nnow2_rabbit) - (c[28]*a*Nnow2_goat*Nnow2_rhino) - (c[29]*a*Nnow2_goat*Nnow2_sheep) - (m[15]*b[15]*Nnow2_goat*Nnow2_alligator) - (m[15]*b[15]*Nnow2_goat**Nnow2_bear) - (m[15]*b[15]*Nnow2_goat*Nnow2_lion) - (m[15]*b[15]*Nnow2_goat*Nnow2_tiger) - (m[15]*b[15]*Nnow2_goat*Nnow2_wolf)
  if(Nnow_goat < 0) {Nnow_goat <- 0}
  N_goat <- c(N_goat, Nnow_goat)
  Nnow_gorilla <- (AC_Birth_Rates$Gorilla*Nnow2_gorilla) - (c[9]*a*Nnow2_gorilla*Nnow2_cub) - (c[26]*a*Nnow2_gorilla*Nnow2_pig)
  if(Nnow_gorilla < 0) {Nnow_gorilla <- 0}
  N_gorilla <- c(N_gorilla, Nnow_gorilla)
  Nnow_hippo <- (AC_Birth_Rates$Hippo*Nnow2_hippo) - (c[5]*a*Nnow2_bull*Nnow2_hippo) - (c[8]*a*Nnow2_hippo*Nnow2_cow) - (c[11]*a*Nnow2_duck*Nnow2_hippo) - (c[13]*a*Nnow2_hippo*Nnow2_elephant) - (c[15]*a*Nnow2_hippo*Nnow2_goat) - (c[18]*a*Nnow2_hippo*Nnow2_horse) - (c[19]*a*Nnow2_hippo*Nnow2_kangaroo) - (c[24]*a*Nnow2_hippo*Nnow2_ostrich) - (c[26]*a*Nnow2_hippo*Nnow2_pig) - (c[27]*a*Nnow2_hippo*Nnow2_rabbit) - (c[28]*a*Nnow2_hippo*Nnow2_rhino) - (c[29]*a*Nnow2_sheep*Nnow2_hippo)
  if(Nnow_hippo < 0) {Nnow_hippo <- 0}
  N_hippo <- c(N_hippo, Nnow_hippo)
  Nnow_horse <- (AC_Birth_Rates$Horse*Nnow2_horse) - (c[5]*a*Nnow2_bull*Nnow2_horse) - (c[8]*a*Nnow2_horse*Nnow2_cow) - (c[13]*a*Nnow2_horse*Nnow2_elephant) - (c[15]*a*Nnow2_horse*Nnow2_goat) - (c[17]*a*Nnow2_horse*Nnow2_hippo) - (c[19]*a*Nnow2_horse*Nnow2_kangaroo) - (c[24]*a*Nnow2_horse*Nnow2_ostrich) - (c[26]*a*Nnow2_horse*Nnow2_pig) - (c[27]*a*Nnow2_horse*Nnow2_rabbit) - (c[28]*a*Nnow2_horse*Nnow2_rhino) - (c[29]*a*Nnow2_horse*Nnow2_sheep)
  if(Nnow_horse < 0) {Nnow_horse <- 0}
  N_horse <- c(N_horse, Nnow_horse)
  Nnow_kangaroo <- (AC_Birth_Rates$Kangaroo*Nnow2_kangaroo) - (c[5]*a*Nnow2_kangaroo*Nnow2_bull) - (c[8]*a*Nnow2_kangaroo*Nnow2_cow) - (c[13]*a*Nnow2_kangaroo*Nnow2_elephant) - (c[15]*a*Nnow2_kangaroo*Nnow2_goat) - (c[17]*a*Nnow2_kangaroo*Nnow2_hippo) - (c[18]*a*Nnow2_kangaroo*Nnow2_horse) - (c[24]*a*Nnow2_kangaroo*Nnow2_ostrich) - (c[26]*a*Nnow2_kangaroo*Nnow2_pig) - (c[27]*a*Nnow2_kangaroo*Nnow2_rabbit) - (c[28]*a*Nnow2_kangaroo*Nnow2_rhino) - (c[29]*a*Nnow2_kangaroo*Nnow2_sheep) - (m[19]*b[19]*Nnow2_kangaroo*Nnow2_alligator) - (m[19]*b[19]*Nnow2_kangaroo*Nnow2_bear) - (m[19]*b[19]*Nnow2_kangaroo*Nnow2_lion) - (m[19]*b[19]*Nnow2_kangaroo*Nnow2_tiger) - (m[19]*b[19]*Nnow2_kangaroo*Nnow2_wolf)
  if(Nnow_kangaroo < 0) {Nnow_kangaroo <- 0}
  N_kangaroo <- c(N_kangaroo, Nnow_kangaroo)
  Nnow_koala <- (AC_Birth_Rates$Koala*Nnow2_koala) - (m[20]*b[20]*Nnow2_koala*Nnow2_alligator) - (m[20]*b[20]*Nnow2_koala*Nnow2_bear) - (m[20]*b[20]*Nnow2_koala*Nnow2_dog) - (m[20]*b[20]*Nnow2_koala*Nnow2_eagle) - (m[20]*b[20]*Nnow2_koala*Nnow2_lion) - (m[20]*b[20]*Nnow2_koala*Nnow2_tiger) - (m[20]*b[20]*Nnow2_koala*Nnow2_wolf)
  if(Nnow_koala < 0) {Nnow_koala <- 0}
  N_koala <- c(N_koala, Nnow_koala)
  Nnow_lion <- (AC_Birth_Rates$Lion*Nnow2_lion) + (m[2]*b[2]*Nnow2_lion*Nnow2_anteater) + (m[6]*b[6]*Nnow2_lion*Nnow2_cat) + (m[7]*b[7]*Nnow2_lion*Nnow2_chicken) + (m[9]*b[9]*Nnow2_lion*Nnow_cub) + (m[10]*b[10]*Nnow2_lion*Nnow2_dog) + (m[11]*b[11]*Nnow2_lion*Nnow2_duck) + (m[12]*b[12]*Nnow2_lion*Nnow2_eagle) + (m[14]*b[14]*Nnow2_lion*Nnow2_frog) + (m[15]*b[15]*Nnow2_lion*Nnow2_goat) + (m[19]*b[19]*Nnow2_kangaroo*Nnow2_lion) + (m[22]*b[22]*Nnow2_mouse*Nnow2_lion) + (m[23]*b[23]*Nnow2_lion*Nnow2_octopus) + (m[24]*b[24]*Nnow2_ostrich*Nnow2_lion) + (m[25]*b[25]*Nnow2_penguin*Nnow2_lion) + (m[26]*b[26]*Nnow2_pig*Nnow2_lion) + (m[27]*b[27]*Nnow2_lion*Nnow2_rabbit) + (m[29]*m[29]*Nnow2_lion*Nnow2_sheep) + (m[30]*b[30]*Nnow2_lion*Nnow2_squirrel) - (c[1]*a*Nnow2_lion*Nnow2_alligator) - (c[3]*a*Nnow2_lion*Nnow2_bear) - (c[6]*a*Nnow2_lion*Nnow2_cat) - (c[9]*a*Nnow2_lion*Nnow2_cub) - (c[10]*a*Nnow2_lion*Nnow2_dog) - (c[12]*a*Nnow2_lion*Nnow2_eagle) - (c[25]*a*Nnow2_lion*Nnow2_penguin) - (c[31]*a*Nnow2_lion*Nnow2_tiger) - (c[32]*a*Nnow2_lion*Nnow2_wolf)
  if(Nnow_lion < 0) {Nnow_lion <- 0}
  N_lion <- c(N_lion, Nnow_lion)
  Nnow_mouse <- (AC_Birth_Rates$Mouse*Nnow2_mouse) - (c[4]*a*Nnow2_mouse*Nnow2_bird) - (c[7]*a*Nnow2_mouse*Nnow2_chicken) - (c[27]*a*Nnow2_mouse*Nnow2_rabbit) - (c[30]*a*Nnow2_mouse*Nnow2_squirrel) - (m[22]*b[22]*Nnow2_mouse*Nnow2_alligator) - (m[22]*b[22]*Nnow2_mouse**Nnow2_bear) - (m[22]*b[22]*Nnow2_mouse*Nnow2_cat) - (m[22]*b[22]*Nnow2_mouse*Nnow2_cub) - (m[22]*b[22]*Nnow2_mouse*Nnow2_dog) - (m[22]*b[22]*Nnow2_mouse*Nnow2_eagle) - (m[22]*b[22]*Nnow2_mouse*Nnow2_lion) - (m[22]*b[22]*Nnow2_mouse*Nnow2_tiger) - (m[22]*b[22]*Nnow2_mouse*Nnow2_wolf)
  if(Nnow_mouse < 0) {Nnow_mouse <- 0}
  N_mouse <- c(N_mouse, Nnow_mouse)
  Nnow_octopus <- (AC_Birth_Rates$Octopus*Nnow2_octopus) - (m[23]*b[23]*Nnow2_octopus*Nnow2_alligator) - (m[23]*b[23]*Nnow2_octopus*Nnow2_bear) - (m[23]*b[23]*Nnow2_octopus*Nnow2_cat) - (m[23]*b[23]*Nnow2_octopus*Nnow2_cub) - (m[23]*b[23]*Nnow2_octopus*Nnow2_dog) - (m[23]*b[23]*Nnow2_octopus*Nnow2_eagle) - (m[23]*b[23]*Nnow2_octopus*Nnow2_lion) - (m[23]*b[23]*Nnow2_octopus*Nnow2_penguin) - (m[23]*b[23]*Nnow2_octopus*Nnow2_tiger) - (m[23]*b[23]*Nnow2_octopus*Nnow2_wolf)
  if(Nnow_octopus < 0) {Nnow_octopus <- 0}
  N_octopus <- c(N_octopus, Nnow_octopus)
  Nnow_ostrich <- (AC_Birth_Rates$Ostrich*Nnow2_ostrich) - (c[5]*a*Nnow2_ostrich*Nnow2_bull) - (c[8]*a*Nnow2_ostrich*Nnow2_cow) - (c[13]*a*Nnow2_ostrich*Nnow2_elephant) - (c[15]*a*Nnow2_ostrich*Nnow2_goat) - (c[17]*a*Nnow2_ostrich*Nnow2_hippo) - (c[18]*a*Nnow2_ostrich*Nnow2_horse) - (c[19]*a*Nnow2_ostrich*Nnow2_kangaroo) - (c[26]*a*Nnow2_ostrich*Nnow2_pig) - (c[27]*a*Nnow2_ostrich*Nnow2_rabbit) - (c[28]*a*Nnow2_ostrich*Nnow2_rhino) - (c[29]*a*Nnow2_ostrich*Nnow2_sheep) - (m[24]*b[24]*Nnow2_ostrich*Nnow2_bear) - (m[24]*b[24]*Nnow2_ostrich*Nnow2_lion) - (m[24]*b[24]*Nnow2_ostrich*Nnow2_tiger) - (m[24]*b[24]*Nnow2_ostrich*Nnow2_wolf)
  if(Nnow_ostrich < 0) {Nnow_ostrich <- 0}
  N_ostrich <- c(N_ostrich, Nnow_ostrich)
  Nnow_penguin <- (AC_Birth_Rates$Penguin*Nnow2_penguin) + (m[23]*b[23]*Nnow2_penguin*Nnow2_octopus) - (c[1]*a*Nnow2_penguin*Nnow2_alligator) - (c[3]*a*Nnow2_penguin*Nnow2_bear) - (c[6]*a*Nnow2_penguin*Nnow2_cat) - (c[9]*a*Nnow2_cub*Nnow2_penguin) - (c[10]*a*Nnow2_dog*Nnow2_penguin) - (c[12]*a*Nnow2_penguin*Nnow2_eagle) - (c[21]*a*Nnow2_penguin*Nnow2_lion) - (c[31]*a*Nnow2_penguin*Nnow2_tiger) - (c[32]*a*Nnow2_penguin*Nnow2_wolf) - (m[25]*b[25]*Nnow2_penguin*Nnow2_alligator) - (m[25]*b[25]*Nnow2_penguin*Nnow2_bear) - (m[25]*b[25]*Nnow2_penguin*Nnow2_dog) - (m[25]*b[25]*Nnow2_penguin*Nnow2_lion) - (m[25]*b[25]*Nnow2_penguin*Nnow2_tiger) - (m[25]*b[25]*Nnow2_penguin*Nnow2_wolf)
  if(Nnow_penguin < 0) {Nnow_penguin <- 0}
  N_penguin <- c(N_penguin, Nnow_penguin)
  Nnow_pig <- (AC_Birth_Rates$Pig*Nnow2_pig) - (c[5]*a*Nnow2_pig*Nnow2_bull) - (c[8]*a*Nnow2_pig*Nnow2_cow) - (c[9]*a*Nnow2_pig*Nnow2_cub) - (c[10]*a*Nnow2_pig*Nnow2_dog) - (c[13]*a*Nnow2_elephant*Nnow2_pig) - (c[15]*a*Nnow2_pig*Nnow2_goat) - (c[16]*a*Nnow2_pig*Nnow2_gorilla) - (c[17]*a*Nnow2_pig*Nnow2_hippo) - (c[18]*a*Nnow2_pig*Nnow2_horse) - (c[19]*a*Nnow2_pig*Nnow2_kangaroo) - (c[24]*a*Nnow2_pig*Nnow2_ostrich) - (c[27]*a*Nnow2_pig*Nnow2_rabbit) - (c[28]*a*Nnow2_pig*Nnow2_rhino) - (c[29]*a*Nnow2_pig*Nnow2_sheep) - (m[26]*b[26]*Nnow2_pig*Nnow2_alligator) - (m[26]*b[26]*Nnow2_pig*Nnow2_bear) - (m[26]*b[26]*Nnow2_pig*Nnow2_lion) - (m[26]*b[26]*Nnow2_pig*Nnow2_tiger) - (m[26]*b[26]*Nnow2_pig*Nnow2_wolf)
  if(Nnow_pig < 0) {Nnow_pig <- 0}
  N_pig <- c(N_pig, Nnow_pig)
  Nnow_rabbit <- (AC_Birth_Rates$Rabbit*Nnow2_rabbit) - (c[4]*a*Nnow2_rabbit*Nnow2_bird) - (c[5]*a*Nnow2_rabbit*Nnow2_bull) - (c[7]*a*Nnow2_rabbit*Nnow2_chicken) - (c[8]*a*Nnow2_rabbit*Nnow2_cow) - (c[13]*a*Nnow2_rabbit*Nnow2_elephant) - (c[15]*a*Nnow2_rabbit*Nnow2_goat) - (c[17]*a*Nnow2_rabbit*Nnow2_hippo) - (c[18]*a*Nnow2_rabbit*Nnow2_horse) - (c[19]*a*Nnow2_rabbit*Nnow2_kangaroo) - (c[22]*a*Nnow2_rabbit*Nnow2_mouse) - (c[24]*a*Nnow2_rabbit*Nnow2_ostrich) - (c[26]*a*Nnow2_rabbit*Nnow2_pig) - (c[28]*a*Nnow2_rabbit*Nnow2_rhino) - (c[29]*a*Nnow2_sheep*Nnow2_rabbit) - (c[30]*a*Nnow2_rabbit*Nnow2_squirrel) - (m[27]*b[27]*Nnow2_rabbit*Nnow2_alligator) - (m[27]*b[27]*Nnow2_rabbit*Nnow2_bear) - (m[27]*b[27]*Nnow2_rabbit*Nnow2_cat) - (m[27]*b[27]*Nnow2_rabbit*Nnow2_cub) - (m[27]*b[27]*Nnow2_rabbit*Nnow2_dog) - (m[27]*b[27]*Nnow2_rabbit*Nnow2_eagle) - (m[27]*b[27]*Nnow2_rabbit*Nnow2_lion) - (m[27]*b[27]*Nnow2_rabbit*Nnow2_tiger) - (m[27]*b[27]*Nnow2_rabbit*Nnow2_wolf)
  if(Nnow_rabbit < 0) {Nnow_rabbit <- 0}
  N_rabbit <- c(N_rabbit, Nnow_rabbit)
  Nnow_rhino <- (AC_Birth_Rates$Rhino*Nnow2_rhino) - (c[5]*a*Nnow2_rhino*Nnow2_bull) - (c[8]*a*Nnow2_rhino*Nnow2_cow) - (c[13]*a*Nnow2_rhino*Nnow2_elephant) - (c[15]*a*Nnow2_rhino*Nnow2_goat) - (c[17]*a*Nnow2_rhino*Nnow2_hippo) - (c[18]*a*Nnow2_rhino*Nnow2_horse) - (c[19]*a*Nnow2_rhino*Nnow2_kangaroo) - (c[24]*a*Nnow2_rhino*Nnow2_ostrich) - (c[26]*a*Nnow2_rhino*Nnow2_pig) - (c[27]*a*Nnow2_rhino*Nnow2_rabbit) - (c[29]*a*Nnow2_rhino*Nnow2_sheep)
  if(Nnow_rhino < 0) {Nnow_rhino <- 0}
  N_rhino <- c(N_rhino, Nnow_rhino)
  Nnow_sheep <- (AC_Birth_Rates$Sheep*Nnow2_sheep) - (c[5]*a*Nnow2_sheep*Nnow2_bull) - (c[8]*a*Nnow2_sheep*Nnow2_cow) - (c[17]*a*Nnow2_sheep*Nnow2_hippo) - (c[18]*a*Nnow2_sheep*Nnow2_horse) - (c[19]*a*Nnow2_sheep*Nnow2_kangaroo) - (c[24]*a*Nnow2_sheep*Nnow2_ostrich) - (c[26]*a*Nnow2_sheep*Nnow2_pig) - (c[27]*a*Nnow2_sheep*Nnow2_rabbit) - (c[28]*a*Nnow2_sheep*Nnow2_rhino) - (m[29]*b[29]*Nnow2_sheep*Nnow2_alligator) - (m[29]*b[29]*Nnow2_sheep*Nnow2_bear) - (m[29]*b[29]*Nnow2_sheep*Nnow2_lion) - (m[29]*b[29]*Nnow2_sheep*Nnow2_tiger) - (m[29]*b[29]*Nnow2_sheep*Nnow2_wolf)
  if(Nnow_sheep < 0) {Nnow_sheep <- 0}
  N_sheep <- c(N_sheep, Nnow_sheep)
  Nnow_squirrel <- (AC_Birth_Rates$Squirrel*Nnow2_squirrel) - (c[4]*a*Nnow2_squirrel*Nnow2_bird) - (c[7]*a*Nnow2_squirrel*Nnow2_chicken) - (c[22]*a*Nnow2_squirrel*Nnow2_mouse) - (c[27]*a*Nnow2_squirrel*Nnow2_rabbit) - (m[30]*b[30]*Nnow2_squirrel*Nnow2_alligator) - (m[30]*b[30]*Nnow2_squirrel*Nnow2_bear) - (m[30]*b[30]*Nnow2_squirrel*Nnow2_cat) - (m[30]*b[30]*Nnow2_squirrel*Nnow2_cub) - (m[30]*b[30]*Nnow2_squirrel*Nnow2_dog) - (m[30]*b[30]*Nnow2_squirrel*Nnow2_eagle) - (m[30]*b[30]*Nnow2_squirrel*Nnow2_lion) - (m[30]*b[30]*Nnow2_squirrel*Nnow2_tiger) - (m[30]*b[30]*Nnow2_squirrel*Nnow2_wolf)
  if(Nnow_squirrel < 0) {Nnow_squirrel <- 0}
  N_squirrel <- c(N_squirrel, Nnow_squirrel)
  Nnow_tiger <- (AC_Birth_Rates$Tiger*Nnow2_tiger) + (m[2]*b[2]*Nnow2_tiger*Nnow2_anteater) + (m[4]*b[4]*Nnow2_tiger*Nnow2_bird) + (m[6]*b[6]*Nnow2_tiger*Nnow2_cat) + (m[7]*b[7]*Nnow2_tiger*Nnow2_chicken) + (m[9]*b[9]*Nnow2_tiger*Nnow2_cub) + (m[10]*b[10]*Nnow2_tiger*Nnow2_dog) + (m[11]*b[11]*Nnow2_tiger*Nnow2_duck) + (m[12]*b[12]*Nnow2_tiger*Nnow2_eagle) + (m[14]*b[14]*Nnow2_tiger*Nnow2_frog) + (m[15]*b[15]*Nnow2_tiger*Nnow2_goat) + (m[19]*b[19]*Nnow2_tiger*Nnow2_kangaroo) + (m[20]*b[20]*Nnow2_tiger*Nnow2_koala) +(m[22]*b[22]*Nnow2_tiger*Nnow2_mouse) + (m[23]*b[23]*Nnow2_tiger*Nnow2_octopus) + (m[24]*b[24]*Nnow2_tiger*Nnow2_ostrich) + (m[25]*b[25]*Nnow2_tiger*Nnow2_penguin) + (m[26]*b[26]*Nnow2_tiger*Nnow2_pig) + (m[27]*b[27]*Nnow2_tiger*Nnow2_rabbit) + (m[29]*b[29]*Nnow2_tiger*Nnow2_sheep) + (m[30]*b[30]*Nnow2_tiger*Nnow2_squirrel) - (c[1]*a*Nnow2_tiger*Nnow2_alligator) - (c[3]*a*Nnow2_tiger*Nnow2_bear) - (c[6]*a*Nnow2_tiger*Nnow2_cat) - (c[9]*a*Nnow2_tiger*Nnow2_cub) - (c[10]*a*Nnow2_tiger*Nnow2_dog) - (c[12]*a*Nnow2_tiger*Nnow2_eagle) - (c[21]*a*Nnow2_tiger*Nnow2_lion) - (c[25]*a*Nnow2_tiger*Nnow2_penguin) - (c[32]*a*Nnow2_tiger*Nnow2_wolf)
  if(Nnow_tiger < 0) {Nnow_tiger <- 0}
  N_tiger <- c(N_tiger, Nnow_tiger)
  Nnow_wolf <- (AC_Birth_Rates$Wolf*Nnow2_wolf) + (m[2]*b[2]*Nnow2_wolf*Nnow2_anteater) + (m[4]*b[4]*Nnow2_wolf*Nnow2_bird) + (m[5]*b[5]*Nnow2_wolf*Nnow2_bull) + (m[6]*b[6]*Nnow2_wolf*Nnow2_cat) + (m[7]*b[7]*Nnow2_wolf*Nnow2_chicken) + (m[8]*b[8]*Nnow2_wolf*Nnow2_cow) + (m[9]*b[9]*Nnow2_wolf*Nnow2_cub) + (m[10]*b[10]*Nnow2_wolf*Nnow2_dog) + (m[11]*b[11]*Nnow2_wolf*Nnow2_duck) + (m[12]*b[12]*Nnow2_wolf*Nnow2_eagle) + (m[14]*b[14]*Nnow2_wolf*Nnow2_frog) + (m[15]+b[15]*Nnow2_wolf*Nnow2_goat) + (m[19]*b[19]*Nnow2_wolf*Nnow2_kangaroo) + (m[20]*b[20]*Nnow2_wolf*Nnow2_koala) + (m[22]*b[22]*Nnow2_wolf*Nnow2_mouse) + (m[23]*b[23]*Nnow2_wolf*Nnow2_octopus) + (m[24]*b[24]*Nnow2_wolf*Nnow2_ostrich) + (m[25]*b[25]*Nnow2_wolf*Nnow2_penguin) + (m[26]*b[26]*Nnow2_wolf*Nnow2_pig) + (m[27]*b[27]*Nnow2_wolf*Nnow2_rabbit) + (m[29]*b[29]*Nnow2_wolf*Nnow2_sheep) + (m[30]*b[30]*Nnow2_wolf*Nnow2_squirrel) - (c[1]*a*Nnow2_wolf*Nnow2_alligator) - (c[3]*a*Nnow2_wolf*Nnow2_bear) - (c[6]*a*Nnow2_wolf*Nnow2_cat) - (c[9]*a*Nnow2_wolf*Nnow2_cub) - (c[10]*a*Nnow2_wolf*Nnow2_dog) - (c[12]*a*Nnow2_wolf*Nnow2_eagle) - (c[21]*a*Nnow2_wolf*Nnow2_lion) - (c[25]*a*Nnow2_wolf*Nnow2_penguin) - (c[31]*a*Nnow2_wolf*Nnow2_tiger)
  if(Nnow_wolf < 0) {Nnow_wolf <- 0}
  N_wolf <- c(N_wolf, Nnow_wolf)
  Nnow2_alligator <- Nnow_alligator
  Nnow2_anteater <- Nnow_anteater
  Nnow2_bear <- Nnow_bear
  Nnow2_bird <- Nnow_bird
  Nnow2_bull <- Nnow_bull
  NNow2_cat <- Nnow_cat
  Nnow2_chicken <- Nnow_chicken
  NNow2_cow <- Nnow_cow
  Nnow2_cub <- Nnow_cub
  Nnow2_dog <- Nnow_dog
  Nnow2_duck <- Nnow_duck
  Nnow2_eagle <- Nnow_eagle
  Nnow2_elephant <- Nnow_elephant
  Nnow2_frog <- Nnow_frog
  Nnow2_goat <- Nnow_goat
  Nnow2_gorilla <- Nnow_gorilla
  Nnow2_hippo <- Nnow_hippo
  Nnow2_horse <- Nnow_horse
  Nnow2_kangaroo <- Nnow_kangaroo
  Nnow2_koala <- Nnow_koala
  Nnow2_lion <- Nnow_lion
  Nnow2_mouse <- Nnow_mouse
  Nnow2_octopus <- Nnow_octopus
  Nnow2_ostrich <- Nnow_ostrich
  Nnow2_penguin <- Nnow_penguin
  Nnow2_penguin <- Nnow_penguin
  Nnow2_pig <- Nnow_pig
  Nnow2_rabbit <- Nnow_rabbit
  Nnow2_rhino <- Nnow_rhino
  Nnow2_sheep <- Nnow_sheep
  Nnow2_squirrel <- Nnow_squirrel
  Nnow2_tiger <- Nnow_tiger
  Nnow2_wolf <- Nnow_wolf
}

#The next lines of code plots every species' population over time for 20 years (the length for which the simulation was run)
#Due to space limitations the majority of these graphs were not presented in the report, only the most interesting patterns among them were subset. All 32 graphs can be found on GitHub
par(mfrow = c(3,3))
EndofSim <- 2001+length(N_alligator) #This line sets the simulation time period as 2001 to 2020
tplot <- 2001:EndofSim #This line saves the x-axis as the values 2001 to 2020
plot(tplot[1:t], N_alligator[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Alligator")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Alligator, col="red")
plot(tplot[1:t], N_anteater[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Anteater")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Anteater, col="red")
plot(tplot[1:t], N_bear[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Bear")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Bear, col="red")
plot(tplot[1:t], N_bird[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Bird")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Bird, col="red")
plot(tplot[1:t], N_bull[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Bull")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Bull, col="red")
plot(tplot[1:t], N_cat[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Cat")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Cat, col="red")
plot(tplot[1:t], N_chicken[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Chicken")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Chicken, col="red")
plot(tplot[1:t], N_cow[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Cow")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Cow, col="red")
plot(tplot[1:t], N_cub[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Cub")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Cub, col="red")

plot(tplot[1:t], N_dog[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Dog")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Dog, col="red")
plot(tplot[1:t], N_duck[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Duck")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Duck, col="red")
plot(tplot[1:t], N_eagle[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Eagle")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Eagle, col="red")
plot(tplot[1:t], N_elephant[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Elephant")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Elephant, col="red")
plot(tplot[1:t], N_frog[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Frog")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Frog, col="red")
plot(tplot[1:t], N_goat[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Goat")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Goat, col="red")
plot(tplot[1:t], N_gorilla[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Gorilla")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Gorilla, col="red")
plot(tplot[1:t], N_hippo[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Hippo")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Hippo, col="red")
plot(tplot[1:t], N_horse[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Horse")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Horse, col="red")

plot(tplot[1:t], N_kangaroo[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Kangaroo")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Kangaroo, col="red")
plot(tplot[1:t], N_koala[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Koala")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Koala, col="red")
plot(tplot[1:t], N_lion[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Lion")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Lion, col="red")
plot(tplot[1:t], N_mouse[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Mouse")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Mouse, col="red")
plot(tplot[1:t], N_octopus[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Octopus")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Octopus, col="red")
plot(tplot[1:t], N_ostrich[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Ostrich")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Ostrich, col="red")
plot(tplot[1:t], N_penguin[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Penguin")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Penguin, col="red")
plot(tplot[1:t], N_pig[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Pig")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Pig, col="red")
plot(tplot[1:t], N_rabbit[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Rabbit")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Rabbit, col="red")

plot(tplot[1:t], N_rhino[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Rhino")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Rhino, col="red")
plot(tplot[1:t], N_sheep[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Sheep")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Sheep, col="red")
plot(tplot[1:t], N_squirrel[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Squirrel")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Squirrel, col="red")
plot(tplot[1:t], N_tiger[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Tiger")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Tiger, col="red")
plot(tplot[1:t], N_wolf[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Wolf")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Wolf, col="red")

#This group of code was an attempt to create a vectors for a dataframe with the actual and simulated values paired
#Disregard the lines for vectors "Years" and "Year"; this ended up being unnecessary for the t-test
#The line to create the "Actual" vector simply makes a vector out of each column of the "AC_Pops_finalized" dataframe (and thus the output lists each species' populations chronologically species by species)
#The line to create the "Simulated" vector takes the values for the years of interest for each species' simulation
Years <- c(2001, 2002, 2003, 2005, 2006, 2012, 2020)
Actual <- c(AC_Pops_finalized$Alligator, AC_Pops_finalized$Anteater, AC_Pops_finalized$Bear, AC_Pops_finalized$Bird, AC_Pops_finalized$Bull, AC_Pops_finalized$Cat, AC_Pops_finalized$Chicken, AC_Pops_finalized$Cow, AC_Pops_finalized$Cub, AC_Pops_finalized$Dog, AC_Pops_finalized$Duck, AC_Pops_finalized$Eagle, AC_Pops_finalized$Elephant, AC_Pops_finalized$Frog, AC_Pops_finalized$Goat, AC_Pops_finalized$Gorilla, AC_Pops_finalized$Hippo, AC_Pops_finalized$Horse, AC_Pops_finalized$Kangaroo, AC_Pops_finalized$Koala, AC_Pops_finalized$Lion, AC_Pops_finalized$Mouse, AC_Pops_finalized$Octopus, AC_Pops_finalized$Ostrich, AC_Pops_finalized$Penguin, AC_Pops_finalized$Pig, AC_Pops_finalized$Rabbit, AC_Pops_finalized$Rhino, AC_Pops_finalized$Sheep, AC_Pops_finalized$Squirrel, AC_Pops_finalized$Tiger, AC_Pops_finalized$Wolf)
Simulated <- c(N_alligator[c(1,2,3,5,6,12,20)], N_anteater[c(1,2,3,5,6,12,20)], N_bear[c(1,2,3,5,6,12,20)], N_bird[c(1,2,3,5,6,12,20)], N_bull[c(1,2,3,5,6,12,20)], N_cat[c(1,2,3,5,6,12,20)], N_chicken[c(1,2,3,5,6,12,20)], N_cow[c(1,2,3,5,6,12,20)], N_cub[c(1,2,3,5,6,12,20)], N_dog[c(1,2,3,5,6,12,20)], N_duck[c(1,2,3,5,6,12,20)], N_eagle[c(1,2,3,5,6,12,20)], N_elephant[c(1,2,3,5,6,12,20)], N_frog[c(1,2,3,5,6,12,20)], N_goat[c(1,2,3,5,6,12,20)], N_gorilla[c(1,2,3,5,6,12,20)], N_hippo[c(1,2,3,5,6,12,20)], N_horse[c(1,2,3,5,6,12,20)], N_kangaroo[c(1,2,3,5,6,12,20)], N_koala[c(1,2,3,5,6,12,20)], N_lion[c(1,2,3,5,6,12,20)], N_mouse[c(1,2,3,5,6,12,20)], N_octopus[c(1,2,3,5,6,12,20)], N_ostrich[c(1,2,3,5,6,12,20)], N_penguin[c(1,2,3,5,6,12,20)], N_pig[c(1,2,3,5,6,12,20)], N_rabbit[c(1,2,3,5,6,12,20)], N_rhino[c(1,2,3,5,6,12,20)], N_sheep[c(1,2,3,5,6,12,20)], N_squirrel[c(1,2,3,5,6,12,20)], N_tiger[c(1,2,3,5,6,12,20)], N_wolf[c(1,2,3,5,6,12,20)])
Year <- c(Years, Years, Years, Years, Years, Years, Years, Years, Years, Years, Years, Years, Years, Years, Years, Years, Years, Years, Years, Years, Years, Years, Years, Years, Years, Years, Years, Years, Years, Years, Years, Years)

#Disregard the "SimvActual" dataframe. It's not set up in a useful way (but I left it in case it someday becomes useful)
SimvActual <- data.frame(Year, Actual, Simulated)

#"SimandActual" is the dataframe set up so that a paired t-test can be run (comparing the groups "Actual" and "Simulated")
SimandAcutal <- data.frame(Value_Type=rep(c("Actual", "Simulated"), each = 224), Population = c(Actual, Simulated))

#
model_eval <- t.test(Population ~ Value_Type, data = SimandAcutal, paired = TRUE)
model_eval

#These lines of code create the graph subset used in the paper and presentation. The code is identical to the graph code found above
par(mfrow=c(3,3))
plot(tplot[1:t], N_alligator[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Alligator")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Alligator, col="red")
legend(1, 1, legend = c("Actual", "Simulated"), col=c("Red", "Black"), lty = (1:1), cex = 0.5)
plot(tplot[1:t], N_bear[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Bear")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Bear, col="red")
plot(tplot[1:t], N_wolf[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Wolf")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Wolf, col="red")
plot(tplot[1:t], N_chicken[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Chicken")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Chicken, col="red")
plot(tplot[1:t], N_frog[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Frog")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Frog, col="red")
plot(tplot[1:t], N_mouse[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Mouse")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Mouse, col="red")
plot(tplot[1:t], N_anteater[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Anteater")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Anteater, col="red")
plot(tplot[1:t], N_duck[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Duck")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Duck, col="red")
plot(tplot[1:t], N_hippo[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Hippo")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Hippo, col="red")

#These last rows repeat the graph code above for wolf and mouse, only this time spaced so that the full graph is zoomed in (one graph per page instead of 9)
par(mfrow=c(1,1))
plot(tplot[1:t], N_wolf[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Wolf")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Wolf, col="red")
plot(tplot[1:t], N_mouse[1:t], type='l', xlab ="Year", ylab="Population", cex = 0.5, ylim = c(0,25), main="Mouse")
lines(AC_Pops_fixed$Year, AC_Pops_fixed$Mouse, col="red")