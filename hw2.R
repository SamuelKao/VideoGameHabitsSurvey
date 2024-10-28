# q1
# read data
videodata <- read.table("videodata.txt", header = TRUE)
# read data
videoMultiple <- read.table("videoMultiple.txt", header = TRUE)
# find the point estimator
point_estimate_fraction <- sum(videodata$time > 0) / length(videodata$time)
confidence_level <- 0.95
z <- qnorm((confidence_level + 1) / 2)
# find proportion standard error
se <- sqrt(point_estimate_fraction * (1 - point_estimate_fraction)) / sqrt(length(videodata))
# find lower and upper confidence interval
lower_interval_estimate_fraction <- point_estimate_fraction - 2 * se
lower_interval_estimate_fraction
higher_interval_estimate_fraction <- point_estimate_fraction + 2 * se
higher_interval_estimate_fraction

# q2
# Filter out rows
videodata_clean <- videodata[!is.na(videodata$freq) & videodata$freq != 99, ]
avg_time <- tapply(videodata_clean$time, videodata_clean$freq, mean, na.rm = TRUE)
sd_time <- tapply(videodata_clean$time, videodata_clean$freq, sd, na.rm = TRUE)
count <- table(videodata_clean$freq)

# Summary statistics
summary_stats <- data.frame(
    freq = names(avg_time),
    avg_time = as.numeric(avg_time),
    sd_time = as.numeric(sd_time),
    count = as.numeric(count)
)
# Barplot of average time spent per frequency category
barplot(summary_stats$avg_time,
    names.arg = summary_stats$freq,
    main = "Average Time Spent by Reported Frequency",
    xlab = "Reported Frequency of Play",
    ylab = "Average Time Spent (hours)",
    ylim = c(0, max(summary_stats$avg_time, na.rm = TRUE) + 2),
    cex.names = 0.8,
    cex.lab = 0.8,
    cex.main = 0.9
)
# Boxplot of time spent by frequency category
boxplot(time ~ freq,
    data = videodata_clean,
    main = "Time Spent Playing Video Games by Reported Frequency",
    xlab = "Time Spent (hours)",
    ylab = "Reported Frequency of Play",
    horizontal = TRUE,
    cex.lab = 0.8,
    cex.main = 0.9
)

# q3
# find the point estimator
point_estimate_average <- mean(videodata$time)
confidence_leve <- 0.95
z <- qnorm((1 + confidence_level) / 2)
# get the variance of data
variance <- 1 / length(videodata$time) * sum((videodata$time - point_estimate_average)**2)
# find average standard error
se <- sqrt(variance) / sqrt(length(videodata$time))
# lower and upper 95% confidence interval
lower_interval_estimate_average <- point_estimate_average - 2 * se
higher_interval_estimate_average <- point_estimate_average + 2 * se

lower_interval_estimate_average
higher_interval_estimate_average

# q4
like_to_play <- videoMultiple[, c("relax", "coord", "challenge", "graphic", "master", "bored")]
like_to_play[] <- lapply(like_to_play, function(x) as.numeric(as.character(x)))
# Count the number of 1s in each column
count_ones <- sapply(like_to_play, function(x) sum(x == 1, na.rm = TRUE))
# Sort and get the names of the top 2 columns with the most 1s
top_2_like <- names(sort(count_ones, decreasing = TRUE)[1:2])
not_like_to_play <- videoMultiple[, c("time", "frust", "lonely", "rules", "cost", "boring", "friends", "point")]
not_like_to_play[] <- lapply(not_like_to_play, function(x) as.numeric(as.character(x)))
count_ones <- sapply(not_like_to_play, function(x) sum(x == 1, na.rm = TRUE))
top_2_dislike <- names(sort(count_ones, decreasing = TRUE)[1:2])
reasons_for_likeliness <- "Like: relax, master. Dislike: time, cost"

# q5
# Cleaning data
video_data_like_cleaned <- videodata[videodata$like != 99, ]
video_data_work_cleaned <- video_data_like_cleaned[video_data_like_cleaned$work != 99, ]
video_data_work_cleaned$work[video_data_work_cleaned$work != 0] <- 1

# Prop for each category
gender_vs_games <- table(video_data_like_cleaned$sex, video_data_like_cleaned$like)
gender_vs_games_prop <- prop.table(gender_vs_games, margin = 1)

work_vs_games <- table(video_data_work_cleaned$work, video_data_work_cleaned$like)
work_vs_games_prop <- prop.table(work_vs_games, margin = 1)

computer_vs_games <- table(video_data_like_cleaned$own, video_data_like_cleaned$like)
computer_vs_games_prop <- prop.table(computer_vs_games, margin = 1)
# plots + formatting
par(mar = c(5, 10, 4, 2), cex.lab = 0.8, cex.axis = 0.8, cex.main = 0.9)

# Plot for Gender vs. Likes Video Games
max_x <- max(gender_vs_games_prop) * 1.1

barplot(gender_vs_games_prop,
    beside = TRUE, horiz = TRUE, col = c("blue", "red"),
    main = "Gender vs. Likes Video Games (Proportion)",
    xlab = "Proportion",
    ylab = "",
    xlim = c(0, max_x),
    names.arg = c("Never Played", "Very Much", "Somewhat", "Not Really", "Not At All"),
    las = 1
)
legend("topright", legend = c("Female", "Male"), fill = c("blue", "red"), cex = 0.7)
mtext("Like to play", side = 2, line = 8, cex = 0.8)

# Plot for Work vs. Likes Video Games
max_x <- max(work_vs_games_prop) * 1.2

barplot(work_vs_games_prop,
    beside = TRUE, horiz = TRUE, col = c("blue", "red"),
    main = "Work vs. Likes Video Games (Proportion)",
    xlab = "Proportion",
    ylab = "",
    xlim = c(0, max_x),
    names.arg = c("Never Played", "Very Much", "Somewhat", "Not Really", "Not At All"),
    las = 1
)
legend("topright", legend = c("No Work", "Work"), fill = c("blue", "red"), cex = 0.7)
mtext("Like to play", side = 2, line = 8, cex = 0.8)
# Plot for Owns Computer vs. Likes Video Games
max_x <- max(computer_vs_games_prop) * 1.1

barplot(computer_vs_games_prop,
    beside = TRUE, horiz = TRUE, col = c("blue", "red"),
    main = "Owns Computer vs. Likes Video Games (Proportion)",
    xlab = "Proportion",
    ylab = "",
    xlim = c(0, max_x),
    names.arg = c("Never Played", "Very Much", "Somewhat", "Not Really", "Not At All"),
    las = 1
)
legend("topright", legend = c("No PC", "Have PC"), fill = c("blue", "red"), cex = 0.7)
mtext("Like to play", side = 2, line = 8, cex = 0.8)

# Reset par settings
par(mar = c(5, 4, 4, 2) + 0.1)

# q6
videodata <- read.table("videodata.txt", header = TRUE)

total_students <- 95
# Count valid respondents based on available grade data
respondents <- sum(!is.na(videodata$grade))
# Calculate the number of non-respondents
non_respondents <- total_students - respondents
# Convert 'grade' to numeric before mapping
videodata$grade <- as.numeric(videodata$grade)
# Properly map numeric grades to letter grades
videodata$grade <- factor(videodata$grade, levels = c(4, 3, 2), labels = c("A", "B", "C"))
# Recalculate the raw counts after mapping to ensure accuracy
raw_counts <- table(videodata$grade)
# Calculate the adjusted proportions with non-respondents included
adjusted_counts <- c(raw_counts, DF = non_respondents)
adjusted_proportions <- adjusted_counts / sum(adjusted_counts)

grade_labels <- c("A", "B", "C", "DF")
adjusted_proportions_for_plot <- adjusted_proportions[grade_labels]
# target grade distribution
target_distribution <- c(A = 0.2, B = 0.3, C = 0.4, DF = 0.1)

# Visual comparison of expected and target distributions
barplot(rbind(adjusted_proportions_for_plot, target_distribution[grade_labels]),
    beside = TRUE,
    horiz = TRUE,
    col = c("blue", "green"),
    main = "Grade Distribution Comparison (Expected vs. Target)",
    xlab = "Proportion",
    ylab = "Grade",
    xlim = c(0, 0.6),
    names.arg = grade_labels,
    cex.names = .8
)
legend("topright", legend = c("Expected", "Target"), fill = c("blue", "green"), cex = 0.7)

# advanced analysis
# gaming preference binary
video_data_like_cleaned$preference <- ifelse(video_data_like_cleaned$like %in% c(2, 3), "Likes", "Dislikes")

# Creating a contingency table for gender and gaming preferences
gender_vs_preference <- table(video_data_like_cleaned$sex, video_data_like_cleaned$preference)
colnames(gender_vs_preference) <- c("Dislikes", "Likes")
rownames(gender_vs_preference) <- c("Female", "Male")
gender_vs_preference

chi_square_test <- chisq.test(gender_vs_preference)
chi_square_test