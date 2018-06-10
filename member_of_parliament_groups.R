### MP Groups 26 May 2018 ###


### Load Libraries
library(rtweet)
library(dplyr)
library(cluster)


### Load MP Profile Data ####
mps_twitter_profile <- dget(file = "C:/Users/DAVID/Documents/members_of_parliament_followers/mps_twitter_profile.txt")

#mps_twitter_handle <- c("konkrumah", 
 #                       "S_OkudzetoAblak", 
  #                      "nenyi_issiw", 
   #                     "SUHUYINI1", 
    #                    "RasMubarak", 
     #                   "karbo_anthony", 
      #                  "samgeorgegh", 
       #                 "pyboamah", 
        #                "UrsulaOw",
         #               "4jour")

#mps_twitter_profile <- lookup_users(mps_twitter_handle)

### Select variables relevant to clustering ####
mps_twitter_profile$name <- recode(mps_twitter_profile$name, "Kojo Oppong Nkrumah"="Kojo Oppong Nkrumah","Sam Okudzeto Ablakwa"="Samuel Okudzeto Ablakwa","Nenyi G. Andah"="George Andah","SUHUYINI ALHASSAN"="Alhassan Suhuyini","Ras Mubarak MP" = "Ras Mubarak","Anthony A. Karbo MP"="Anthony Karbo","Sam 'Dzata' George"="Samuel George","Patrick Yaw Boamah"="Patrick Yaw Boamah","Ursula Owusu"="Ursula Owusu-Ekuful","John Ntim Fordjour" = "John Ntim Fordjour")


mps_twitter_profile <- mps_twitter_profile %>% select(name, followers_count, friends_count, statuses_count, favourites_count, account_created_at)


# Find number of days between account creation and today
mps_twitter_profile$days_since_account_created <- difftime(time1 = as.Date("2018-05-27"), time2 = mps_twitter_profile$account_created_at)

# Change days difference into numberic objects
mps_twitter_profile$days_since_account_created <- as.numeric(mps_twitter_profile$days_since_account_created)

# Compute units per day 
mps_twitter_profile <- mps_twitter_profile %>% mutate(followers_count = followers_count/days_since_account_created,
                                                      friends_count = friends_count/days_since_account_created, 
                                                      statuses_count = statuses_count/days_since_account_created, 
                                                      favourites_count = favourites_count/days_since_account_created)
#dput(mps_twitter_profile, file = "C:/Users/DAVID/Documents/members_of_parliament_followers/mps_twitter_profile.txt")


# Rename rows into names of MPs
row.names(mps_twitter_profile) <- mps_twitter_profile$name

# Remove "name" column to aid standardization computation & Remove uneccesary columns
mps_twitter_profile <- mps_twitter_profile %>% select(-name, -account_created_at, -days_since_account_created)


# Standardize variables so that can compare accross different units
scale_mps <- scale(mps_twitter_profile)

# Calculate distances to determine (dis)similarity
dist_matrix <- dist(scale_mps, method = "euclidean")

### Use Ageratative Hierachical Clustering to cluster MPs into groups
agnes <- agnes(dist_matrix, method = "ward")

# Plot dendenogram to visualise clusters
plot(agnes, main = "", xlab = "", sub= "")
title(main = "10 of Ghana's Members of Parliament grouped \n according to how they use twitter (Likes/ Tweets/ Followers/ Following)", 
      sub = "Source: Twitter(rtweet) \n Date: 27/05/2018")

# Colour clusters
rect.hclust(agnes, k = 3, border = 2:4)


########### Test to see most robust method #####

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(dist_matrix, method = x)$ac
}

purrr::map_dbl(m, ac)


fviz_cluster(list(data = dist_matrix, cluster = cutree(agnes, 4)))



