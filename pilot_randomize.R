set.seed(20)

d = read.csv("./pilot.csv")

# change column names so easier to read
colnames(d) <- c("ID", "Timestamp", "email", "phone_use", 
                 "number_phones", "age", "gender", "OS")

# subset on phone use, operating system, and number of phones for pilot
pilot = subset(d, phone_use == "Multiple times per hour" & OS == "iPhone - iOS 10.X"
               & number_phones == "One phone - used both for work and personal",
                  select = c(ID, email, phone_use, number_phones, age, gender, OS))

# assign individuals to control or treatment at random
control_treatment = sample(rep(c(0,1), each=3))

# add assignment to data frame
pilot$control_treatment = control_treatment

# create control and treatment dataframes
control = pilot[pilot$control_treatment == 0,]
treatment = pilot[pilot$control_treatment == 1,]

# show control and treatment group ID and emails
control[,c(1,2)]
treatment[,c(1,2)]
