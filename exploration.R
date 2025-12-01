#this script explores VALOR 3.1 data

#read data----
dat <- read.csv("data/data_fruit.csv")
head(dat)

#load some functions
fit_data <- function(seedset, visitation, a_start = 0, b_start = 10, c_start = 0.5, 
                     control = nls.control(maxiter = 1000), algorithm = "default"){
  nlmod3 <- nls(seedset ~  ((b*a)/100) + (b-((b*a)/100)) * (1-exp(-c*visitation)), 
                start = list(a = a_start, b = b_start,c = c_start),
                control= control, algorithm = algorithm)
  #summary(nlmod3)
  c <- coef(nlmod3)
  c
}


plot_visits <- function(a, b, c, from_ = 0, to_ = 1, add_ = FALSE, col_ = 2){
  a2 <- (b*a)/100
  b2 <- b-a2
  curve(a2+b2*(1-exp(-c*x)), from = from_, to = to_, add = add_, col = col_, ylim = c(0, b), las = 1)
}



#Explor visually----
unique(dat$plant_species)
#Fix mispelling
dat$plant_species <- ifelse(dat$plant_species == "Cynara_humillus", "Cynara_humillis", dat$plant_species)
dat$plant_species <- ifelse(dat$plant_species == "Armeria_sp", "Armeria_gaditana", dat$plant_species)
dat$plant_species <- ifelse(dat$plant_species == "Narcisus_paperaceus ", "Narcisus_paperaceus", dat$plant_species)

#Select the species to explore:-----
species <- "Anchusa_calcarata"

datsp <- subset(dat, plant_species == species)
head(datsp)

datsp$n_fruits[which(datsp$n_fruits == "Abortion")] <- 0
datsp$n_fruits <- as.numeric(datsp$n_fruits)
hist(datsp$n_fruits)

datsp$total_visits <- as.numeric(datsp$total_visits)

#Check Data with MJ
scatter.smooth(datsp$n_fruits ~ jitter(datsp$total_visits), las = 1)
#Clear binary pattern
boxplot(datsp$n_fruits ~ as.character(datsp$Treatment), las = 1, xlab ="", ylab = "Fruit set", main = "Anchusa calcarata")
#Check non-linear fit
m <- fit_data(seedset = datsp$n_fruits, visitation = datsp$total_visits, 
              a_start = 0, b_start = 250, c_start = 1, control = nls.control(maxiter = 1000))
plot(datsp$n_fruits ~ jitter(datsp$total_visits), las = 1, 
     xlab = "Visits", ylab = "Fruit set", main = "Anchusa_calcarata")
plot_visits(a = m[[1]], b = m[[2]], c = m[[3]], from_ = 0, to_ = 20, add_ = T, col_ = "red")

datsp[which(datsp$total_visits > 2),]
unique(datsp$plant_number)

#Select the species to explore:-----
species <- "Narcisus_paperaceus"

datsp <- subset(dat, plant_species == species)
head(datsp)
unique(datsp$plant_number)

datsp$n_fruits <- as.numeric(datsp$n_fruits)
hist(datsp$n_fruits)

datsp$total_visits <- as.numeric(datsp$total_visits)

#Check Data with MJ
plot(datsp$n_fruits ~ jitter(datsp$total_visits), las = 1, xlab = "Visits", 
     ylab = "Fruit set", main = "Narcisus paperacius")
#Clear binary pattern
boxplot(datsp$n_fruits ~ as.character(datsp$Treatment), las = 1,
        xlab ="", ylab = "Fruit set", main = "Narcisus paperacius")


#Select the species to explore:-----
species <- "Armeria_gaditana"

datsp <- subset(dat, plant_species == species)
head(datsp)
unique(datsp$plant_number)

datsp$n_fruits <- as.numeric(datsp$n_fruits)
hist(datsp$n_fruits)

datsp$total_visits <- as.numeric(datsp$total_visits)

#Check Data with MJ
scatter.smooth(datsp$n_fruits ~ datsp$total_visits, las = 1) #Nice!
#Clear binary pattern
boxplot(datsp$n_fruits ~ as.character(datsp$Treatment), las = 1,
        xlab ="", ylab = "Fruit set", main = "Armeria gaditana")
#Check non-linear fit
fit_data(seedset = datsp$n_fruits, visitation = datsp$total_visits, 
         a_start = 0, b_start = 250, c_start = 1, control = nls.control(maxiter = 1000))
#Check non-linear fit
m <- fit_data(seedset = datsp$n_fruits, visitation = datsp$total_visits, 
              a_start = 0, b_start = 250, c_start = 1, control = nls.control(maxiter = 1000))
plot(datsp$n_fruits ~ datsp$total_visits, las = 1,  xlab = "Visits", 
     ylab = "Fruit set", main = "Armeria gaditana")
plot_visits(a = m[[1]], b = m[[2]], c = m[[3]], from_ = 0, to_ = 20, add_ = T, col_ = "red")



#Select the species to explore:-----
species <- "Cynara_humillis"

datsp <- subset(dat, plant_species == species)
head(datsp)
unique(datsp$plant_number)

datsp$n_fruits <- as.numeric(datsp$n_fruits)
hist(datsp$n_fruits)

datsp$total_visits <- as.numeric(datsp$total_visits)

#Check Data with MJ
scatter.smooth(datsp$n_fruits ~ datsp$total_visits, las = 1) 
#Clear binary pattern
boxplot(datsp$n_fruits ~ as.character(datsp$Treatment), las = 1,
        xlab ="", ylab = "Fruit set", main = "Cynara humillis")

#Check non-linear fit
#datsp <- subset(datsp, total_visits != 1) #to avoid convergence issues
scatter.smooth(datsp$n_fruits ~ datsp$total_visits, las = 1) 
m <- fit_data(seedset = datsp$n_fruits, visitation = datsp$total_visits, 
         a_start = 0, b_start = 200, c_start = 0.1, 
         control = nls.control(maxiter = 1000, scaleOffset = 1, warnOnly = TRUE))
plot(datsp$n_fruits ~ datsp$total_visits, las = 1, xlab = "Visits", 
     ylab = "Fruit set", main = "Cynara humillis") 
plot_visits(a = m[[1]], b = m[[2]], c = m[[3]], from_ = 0, to_ = 20, add_ = T, col_ = "red")

