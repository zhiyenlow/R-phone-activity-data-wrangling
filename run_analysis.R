# Loading data into R
x_train <- read.table('x_train.txt')
y_train <- read.table('y_train.txt')
sub_train <- read.table("subject_train.txt")
x_test <- read.table('x_test.txt')
y_test <- read.table('y_test.txt')
sub_test <- read.table('subject_test.txt')
activity <- read.table("activity_labels.txt")
features <- read.table("features.txt")

# Cleaning up column names
colnames(x_train) <- features[, 2]
colnames(x_test) <- features[, 2]
names(y_train)[1] <- "act_num"
names(y_test)[1] <- "act_num"
names(activity) <- c("act_num", "act_label")
names(sub_train) <- "subject"
names(sub_test) <- "subject"

# Adding a dummy column to join all training data frames together
sub_train$dummy <- seq(1, 7352, by = 1)
x_train$dummy <- seq(1, 7352, by = 1)
y_train$dummy <- seq(1, 7352, by = 1)

sub_test$dummy <- seq(1, 2947, by = 1)
x_test$dummy <- seq(1, 2947, by = 1)
y_test$dummy <- seq(1, 2947, by = 1)


# Joining the data frames together
# Joining training set together
train1 <- inner_join(sub_train, y_train, by = "dummy")
train1 <- inner_join(train1, x_train, by = "dummy")
train1 <- inner_join(activity, train1, by = "act_num")

# Joining test set together
test1 <- inner_join(sub_test, y_test, by = "dummy")
test1 <- inner_join(test1, x_test, by = "dummy")
test1 <- inner_join(activity, test1, by = "act_num")

# Joining training and test sets together and removing "dummy" variable
total1 <- rbind(train1, test1)
total1$dummy <- NULL

# Making column names a valid string, and then extracting mean and standard deviation values
valid_column_names <- make.names(names=names(total1), unique=TRUE, allow_ = TRUE)
names(total1) <- valid_column_names

total_extract <- select(.data = total1, 
                       act_num, act_label, subject, 
                       contains("mean"), contains("std"))

# Finally, we summarize each variable by its average for each subject and activity
total_extract_sum <- total_extract %>% 
  group_by(act_label, subject) %>% 
  summarise_each(funs(mean))

# Saving our tidy dataset to our drive
write.table(total_extract_sum, "summary.txt", sep="\t")

