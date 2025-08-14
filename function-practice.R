# adds up the number of birds and dogs

# defined function
birddog_sum <- function(bird, dog) {
  pets <- bird + dog # bird and dog are arbitrary, no actual data. placeholder variables like i or j
  return(pets)
}

# use the function
total_pets <- birddog_sum(bird = 2, dog = 5)
total_pets <- birddog_sum(2, 5) # same as above

# create a function to double values
double_it <- function(x) {
  print(2 * x)
}

double_it(91083029)

# write a function with conditionals
# example is converting animals ages
animal_age <- function(animal, age) {
  if (animal == "dog") {
  print(age * 7)
} else if (animal == "goat") {
  print(age * 4.7)
}
}

# try using for an 8 year old dog
animal_age(animal = "dog", age = 8)

# try using for a cow
animal_age(animal = "cow", age = 8)

# write an updated version of animal_age function with error/warning messages

animal_age_stop <- function(animal, age) {
  if (!animal %in% c("dog", "goat")) { # ! is a "not". so it means "if an animal is not a dog or goat"
    stop("oops! animal must be a dog or goat.")
  } 
  
  if (is.numeric(age) == FALSE) {
    stop("age must be a number.")
  }
  
  if (age <= 0 | age > 50) {
    warning("are you sure about your animal's age?")
  }
  
  if (animal == "dog") {
    print(age * 7)
  } else if (animal == "goat") {
    print(age * 4.7)
  }
}

animal_age_stop("elephant", 100)





