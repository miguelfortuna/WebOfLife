
###########################################################
######################## R kernel #########################
###########################################################

###########################
# load required libraries #
###########################
#install.packages("tidyverse") # data manipulation, exploration and visualization that share a common design philosophy
#install.packages("rjson") # read json files
#install.packages("data.table") # large data sets 
#install.packages("bipartite") # analyses and visualization of bipartite networks
#install.packages("repr") # to change plot size
library("tidyverse")
library("rjson")
library("data.table")
library("bipartite")
library("repr")

################################################################
# download a single ecological network from www.web-of-life.es #
################################################################
networkName <- "M_SD_002"
speciesName <- "yes"
url <- paste("http://www.web-of-life.es/download/",
             networkName,
             "_",
             speciesName,
             ".csv",
             sep = "")
network_adjacency_matrix <- as.matrix(read.csv(file = url, sep = ",",
                                               header = TRUE,
                                               row.names = 1,
                                               check.names = "FALSE"))
# print the network_adjacency_matrix (plants as rows; seed dispersers as columns)
network_adjacency_matrix

#############################################################
# print the list of pairwise interactions from that network #
#############################################################
network_list_edges <- as.data.frame(network_adjacency_matrix) %>%
    rownames_to_column %>%
    gather(key = "seed_disperser", value="interaction", -rowname) %>%
    rename(plant = rowname) %>%
    # remove pairs of species that do not interact (interaction = 0)
    filter(interaction > 0)
network_list_edges

####################
# plot the network #
####################
plotweb(network_adjacency_matrix) # seed dispersers (top) and plants species (bottom)

########################################################################################
# save the adjacency matrix as a txt file (plants as rows; seed dispersers as columns) #
########################################################################################
network_adjacency_matrix[network_adjacency_matrix > 0] <- 1 # from quantitative to binary matrix
write.table(network_adjacency_matrix,
            file = "example_network_M_SD_002.txt",
            sep = " ",
            row.names = FALSE,
            col.names = FALSE)

##################################################################################
# download networks by specifying the type of interaction (plant-seed disperser) #
##################################################################################
# create an empty list to store the networks
network_list <- list()

# type of interaction
    # mutualistic
        # "3" (plant-ant)
        # "5" (plant-pollinator)
        # "6" (plant-seed disperser)
        # "11" (anemone-fish)
    # antagonistic
        # "8" (host-parasite)
        # "10" (plant-herbivore)
        # "7" (food webs)
type_id <- "6"

# create a file (json_networks) with the names of the networks we would like to download
json_file <- paste("http://www.web-of-life.es/networkslist.php?type=",
                   type_id,
                   "&data=All",
                   sep = "")
json_networks <- fromJSON(paste(readLines(json_file),
                                collapse = ""))

# would you like to include the names of the species? ("yes" or "no")
speciesName <- "yes"

# download the networks
for(i in 1: length(json_networks)){
    
    # identifying the network
    #if(json_networks[[i]]$countSpecies > 0) { # we get networks and subnetworks
    if(json_networks[[i]]$root == 0 & is.null(json_networks[[i]]$parentNetworkId)){ # we get networks without subnetworks
        networkName <- json_networks[[i]]$networkName
        print(networkName)

        # building the URL
        url <- paste("http://www.web-of-life.es/download/",
                     networkName,
                     "_",
                     speciesName, ".csv",
                     sep = "")

        # download the network from www.web-of-life.es
        data <- fread(url)

        # storing the networks as a data table
        assign(networkName,data)

        # storing the networks as a list
        network_list[[networkName]] <- (data)
    }
}

# check the number of plant-seed disperser networks stored in the list
number_downloaded_networks <- length(network_list)
number_downloaded_networks

######################################################################
# the following code is illustrates the process for a single network #
######################################################################

########################################
# print one of the downloaded networks #
########################################
# get it from the data table
#M_SD_002

# get it from the list
#network_list[[2]]
network_list[["M_SD_002"]]

###########################################################################################
# convert the values in the first column of one of the downloaded networks into row names #
###########################################################################################
# plant-pollinator (do it only once because it overwrites the network: otherwise, download again the list of networks)
M_SD_002 <- M_SD_002 %>%
    remove_rownames %>%
    column_to_rownames(var="V1")   
M_SD_002

#########################################################################################
# convert network from quantitative to qualitative (i.e., as a binary adjacency matrix) #
#########################################################################################
M_SD_002[M_SD_002 > 0] <- 1
M_SD_002

####################################
# compute some network descriptors #
####################################

# plant-pollinator
plants <- nrow(M_SD_002) # number of rows
plants

animals <- ncol(M_SD_002) # number of columns
animals

interactions <- sum(M_SD_002) # number of interactions
interactions

connectance <- interactions / (plants * animals) # connectance
connectance

##############################################################################
# download networks by specifying the type of interaction (plant-pollinator) #
##############################################################################
# create an empty list to store the networks
network_list_PL <- list()

# type of interaction
    # mutualistic
        # "3" (plant-ant)
        # "5" (plant-pollinator)
        # "6" (plant-seed disperser)
        # "11" (anemone-fish)
    # antagonistic
        # "8" (host-parasite)
        # "10" (plant-herbivore)
        # "7" (food webs)
type_id <- "5"

# create a file (json_networks) with the names of the networks we would like to download
json_file <- paste("http://www.web-of-life.es/networkslist.php?type=",
                   type_id,
                   "&data=All",
                   sep = "")
json_networks <- fromJSON(paste(readLines(json_file),
                                collapse = ""))

# would you like to include the names of the species? ("yes" or "no")
speciesName <- "yes"

# download the networks
for(i in 1: length(json_networks)){
    
    # identifying the network
    #if(json_networks[[i]]$countSpecies > 0) { # we get networks and subnetworks
    if(json_networks[[i]]$root == 0 & is.null(json_networks[[i]]$parentNetworkId)){ # we get networks without subnetworks
        networkName <- json_networks[[i]]$networkName
        print(networkName)

        # building the URL
        url <- paste("http://www.web-of-life.es/download/",
                     networkName,
                     "_",
                     speciesName, ".csv",
                     sep = "")

        # download the network from www.web-of-life.es
        data <- fread(url)

        # storing the networks as a data table
        assign(networkName,data)

        # storing the networks as a list
        network_list_PL[[networkName]] <- (data)
    }
}

# check the number of plant-pollinator networks stored on the list
number_downloaded_PL_networks <- length(network_list_PL)
number_downloaded_PL_networks

PL_size <- vector() # initialize vector to store network size
PL_connectance <- vector() # initialize vector to store network connectance

# please, note that network_list_PL[[61]] has an additional column ("Number of flowers"): we have to remove that column
for (i in 1: length(network_list_PL)) {
    
    if(i == 61) {
        current_network <- network_list_PL[[i]] %>%
            remove_rownames %>%
            column_to_rownames(var="V1") %>%
            select(-1)
    }
    else {
        # convert the values in the first column of one of the downloaded network into row names
        current_network <- network_list_PL[[i]] %>%
            remove_rownames %>%
            column_to_rownames(var="V1")   
    }
    # convert network from quantitative to qualitative (i.e., as a binary adjacency matrix)
    current_network[current_network > 0] <- 1

    # compute some network descriptors
    plants <- nrow(current_network) # number of rows
    animals <- ncol(current_network) # number of columns
    interactions <- sum(current_network) # number of interactions
    connectance <- interactions / (plants * animals) # connectance

    # store network size and connectance as vectors
    PL_size[i] <- plants * animals # network size
    PL_connectance[i] <- connectance # connectance
}

# print the data
PL_data <- data.frame(PL_size, PL_connectance)
names(PL_data) <- c("size", "connectance")
PL_data

#####################
### host-parasite ###
#####################

###########################################################################
# download networks by specifying the type of interaction (host-parasite) #
###########################################################################
# create an empty list to store the networks
network_list_HP <- list()

# type of interaction
    # mutualistic
        # "3" (plant-ant)
        # "5" (plant-pollinator)
        # "6" (plant-seed disperser)
        # "11" (anemone-fish)
    # antagonistic
        # "8" (host-parasite)
        # "10" (plant-herbivore)
        # "7" (food webs)
type_id <- "8"

# create a file (json_networks) with the names of the networks we would like to download
json_file <- paste("http://www.web-of-life.es/networkslist.php?type=",
                   type_id,
                   "&data=All",
                   sep = "")
json_networks <- fromJSON(paste(readLines(json_file),
                                collapse = ""))

# would you like to include the names of the species? ("yes" or "no")
speciesName <- "yes"

# download the networks
for(i in 1: length(json_networks)){
    
    # identifying the network
    #if(json_networks[[i]]$countSpecies > 0){ # we get networks and subnetworks
    if(json_networks[[i]]$root == 0 & is.null(json_networks[[i]]$parentNetworkId)) { # we get networks without subnetworks
        networkName <- json_networks[[i]]$networkName
        print(networkName)

        # building the URL
        url <- paste("http://www.web-of-life.es/download/",
                     networkName,
                     "_",
                     speciesName, ".csv",
                     sep = "")

        # download the network from www.web-of-life.es
        data <- fread(url)

        # storing the networks as a data table
        assign(networkName,data)

        # storing the networks as a list
        network_list_HP[[networkName]] <- (data)
    }
}

# check the number of host-parasite networks stored on the list
number_downloaded_HP_networks <- length(network_list_HP)
number_downloaded_HP_networks

HP_size <- vector() # initialize vector to store network size
HP_connectance <- vector() # initialize vector to store network connectance

for (i in 1: length(network_list_HP)) {
    
    # convert the values in the first column of one of the downloaded network into row names
    # please, be careful! first column in HP networks indicates sample size; second column contains the name of the hosts
    current_network <- network_list_HP[[i]] %>%
        remove_rownames %>%
        column_to_rownames(var="V1") %>%
        select(-1) # remove what is now the first column (it was the second column containing sample size)  
    # convert network from quantitative to qualitative (i.e., as a binary adjacency matrix)
    current_network[current_network > 0] <- 1

    # compute some network descriptors
    hosts <- nrow(current_network) # number of rows
    parasites <- ncol(current_network) # number of columns
    interactions <- sum(current_network) # number of interactions
    connectance <- interactions / (hosts * parasites) # connectance

    # store network size and connectance as vectors
    HP_size[i] <- hosts * parasites # network size
    HP_connectance[i] <- connectance # connectance
}

# print the data
HP_data <- data.frame(HP_size, HP_connectance)
names(HP_data) <- c("size", "connectance")
HP_data

#############
### plots ###
#############

# plot relationship between plant-pollinator network size and connectance
options(repr.plot.width=5, repr.plot.height=4)
ggplot() +
    ggtitle("Plant - Pollinator \n Networks") +
    geom_point(data = PL_data, aes(log(size), connectance), color = "blue") +
    geom_smooth(data = PL_data, aes(log(size), connectance), method = "lm", se = FALSE, color = "blue") +
    labs(x = "Network Size", 
         y = "Network Connectance") +
theme_classic()

# plot relationship between host-parasite network size and connectance
options(repr.plot.width=5, repr.plot.height=4)
ggplot() +
    ggtitle("Host - Parasite \n Networks") +
    geom_point(data = HP_data, aes(log(size), connectance), color = "red") +
    geom_smooth(data = HP_data, aes(log(size), connectance), method = "lm", se = FALSE, color = "red") +
    labs(x = "Network Size", 
         y = "Network Connectance") +
theme_classic()

# plot relationship between size and connectance for both plant-pollinator and host-parasite networks
options(repr.plot.width=5, repr.plot.height=4)
ggplot() +
    ggtitle("Plant-Pollinator (blue) and Host-Parasite (red)") +
    geom_point(data = PL_data, aes(log(size), connectance), color = "blue") +
    geom_smooth(data = PL_data, aes(log(size), connectance), method = "lm", se = FALSE, color = "blue") +
    geom_point(data = HP_data, aes(log(size), connectance), color = "red") +
    geom_smooth(data = HP_data, aes(log(size), connectance), method = "lm", se = FALSE, color = "red") +
    labs(x = "Network Size", 
         y = "Network Connectance") +
theme_classic()
