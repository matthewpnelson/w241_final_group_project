set.seed(20)

d = read.csv("./subjects.csv")

# change column names so easier to type
colnames(d) <- c("timestamp", "email", "phone_use", 
                 "number_phones", "age", "gender", "os", 
                 "contact_method", "relationship")

# add ID
d$ID = seq.int(nrow(d))

# create new dataframe with encoded values for blocking
df <- data.frame(ID = seq.int(nrow(d)),
                 email = d$email)

# add column to identify pilot participants
df$pilot = 0 # default 0 
df$pilot[d$ID > 1 & d$ID < 4 | d$ID > 6 & d$ID < 11] = 1

# Encode variables we are blocking on for easier randomizing
# encode gender (F = 1, Male & Other = 2)
df$gender = 1
df$gender[d$gender == "Male" | d$gender == "Prefer not to say"] = 2

# encode age (<24 = 1, 25-54 = 2, 55+ = 3)
df$age_code = 1
df$age_code[d$age == "25-34" | d$age == "35-44" | 
             d$age == "35-44" | d$age == "45-54"] = 2
df$age_code[d$age == "55-64"] = 3

# encode OS (Apple = 1, Android = 2, Other = 3)
df$os = 1
df$os[grep("Android", d$os)] = 2 # regex, if contains 'Android', encode as 2
df$os[d$os == "Blackberry"] = 3

# encode number_phones
df$num_phones = 1
df$num_phones[grep("only", d$number_phones)] = 2

# remove pilot people
df = df[df$pilot == 0,]

# all combinations of gender, age, os, num_phones
gender = unique(df$gender)
age = unique(df$age)
os = unique(df$os)
phones = unique(df$num_phones)

comb = expand.grid(gender = gender, # creates dataframe with all combos
            age = age, 
            os = os, 
            phones = phones) 

# label blocks 1-36
comb$block = seq.int(nrow(comb))

# add block column in randomization df
df$block = 0

# assign block to individuals
for (i in 1:nrow(df)) {
  row = df[i, 4:7]                      # get blocking values in df
  for(j in 1:nrow(comb)) {
    combination = comb[j, 1:4]          # get blocking values in comb
    block = comb[j, 5]
    if (sum(row == combination) == 4) { # if blocking values are equal
      df[i, 8] = block                  # assign block to df
    }
  }
}

# create treatment/control assignment
df$treat = 0

# assign based on block
for (h in 1:36) {
  size = nrow(df[df$block == h,])                # get size of block
  zero = round(size/2)                           # take size, divide by 2 and round down
  treatment = sample(c(rep(0, zero), 
                       rep(1, size-zero)), size) # randomly assign 1 with 0.5 prob  
  df$treat[df$block == h] = treatment            # assign treatment to block
}

# check proportion treatment in each block                
for (g in 1:36) {
  c = sum(df$treat[df$block == g] == 0)
  t = sum(df$treat[df$block == g] == 1)
  p = t/(c+t)
  print(cat(g,p))
}

# Blocks with 1 person will get assigned to treatment by default
# Group blocks with 1 person and randomize within
one_person = c()
for (c in 1:36) {
  size = nrow(df[df$block == c,])
  if (size == 1) {
    one_person = c(one_person, c)
  }
}

# Randomly assign within one-person blocks
df[df$block %in% one_person,]                # view blocks with one person
size = nrow(df[df$block %in% one_person,])   # size of blocks with one person    
zero = round(size/2)                         # calculate number in control group
treatment = sample(c(rep(0, zero),           # randomize
                     rep(1, size-zero)), size) 
df$treat[df$block %in% one_person] = treatment # assign 
df[df$block %in% one_person,] # check to see if randomized

# check proportion treatment in df
mean(df$treat)

# view people in treatment and control
treat = df$email[df$treat == 1]
control = df$email[df$treat == 0]

# merge dataframes 
assignments = merge(d, df, by.x = "email", by.y = "email", all.x = TRUE)
assignments = assignments[order(assignments$ID.x),]

# examine fully merged dataset
assignments


# write to csv
write.csv(treat, "email_treatment.csv")
write.csv(control, "email_control.csv")
write.csv(assignments, "assignments.csv")

