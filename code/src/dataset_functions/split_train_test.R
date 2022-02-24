convert_train_dataset <- function (trainset_df) {
  trainset_df$threatened <- as.character(trainset_df$threatened)
  
  trainset_df[trainset_df$threatened == "TRUE",] <- trainset_df %>%
    filter(threatened == "TRUE") %>%
    mutate(threatened = "threatened")
  
  trainset_df[trainset_df$threatened == "FALSE",] <- trainset_df %>%
    filter(threatened == "FALSE") %>%
    mutate(threatened = "non_threatened")
  
  trainset_df$threatened <- as.factor(trainset_df$threatened)
  trainset_df$threatened <- factor(trainset_df$threatened, levels=c("threatened", "non_threatened"))
  
  train_no_species <- trainset_df %>% dplyr::select(-c("species"))
  return(train_no_species)
}

split_train_test <- function (trainset_df, group_name) {
  
  if (file.exists(paste0("data/processed/trainset_",group_name,".rds")) && file.exists(paste0("data/processed/testset_",group_name,".rds"))){ # Datasets exist
    train_no_species <- readRDS(paste0("data/processed/trainset_",group_name,".rds"))
    test_no_species <- readRDS(paste0("data/processed/testset_",group_name,".rds"))

    return(list("train"= train_no_species, "test"=test_no_species))
  }
 
  trainset_df$threatened <- as.character(trainset_df$threatened)
  
  trainset_df[trainset_df$threatened == "TRUE",] <- trainset_df %>%
    filter(threatened == "TRUE") %>%
    mutate(threatened = "threatened")
  
  trainset_df[trainset_df$threatened == "FALSE",] <- trainset_df %>%
    filter(threatened == "FALSE") %>%
    mutate(threatened = "non_threatened")
  
  trainset_df$threatened <- as.factor(trainset_df$threatened)
  trainset_df$threatened <- factor(trainset_df$threatened, levels=c("threatened", "non_threatened"))
  
  
  train_ind = sample(seq_len(nrow(trainset_df)),size = floor(0.75*nrow(trainset_df)))
  
  train <- trainset_df[train_ind,]
  test <- trainset_df[-train_ind,]
  
  table(train$threatened)
  table(test$threatened)
  
  saveRDS(train, paste0("data/processed/trainset_",group_name,"_with_species.rds"))
  saveRDS(test, paste0("data/processed/testset_",group_name,"_with_species.rds"))
  
  train_no_species <- train %>% dplyr::select(-c("species"))
  test_no_species <- test %>% dplyr::select(-c("species"))
  
  saveRDS(train_no_species, paste0("data/processed/trainset_",group_name,".rds"))
  saveRDS(test_no_species, paste0("data/processed/testset_",group_name,".rds"))

  return(list("train"= train_no_species, "test"=test_no_species))
}



split_train_test_by_existing <- function (trainset_df, group_name, existing_group) {
  
  if (file.exists(paste0("data/processed/trainset_",group_name,".rds")) && file.exists(paste0("data/processed/testset_",group_name,".rds"))){ # Datasets exist
    train_no_species <- readRDS(paste0("data/processed/trainset_",group_name,".rds"))
    test_no_species <- readRDS(paste0("data/processed/testset_",group_name,".rds"))
    
    return(list("train"= train_no_species, "test"=test_no_species))
  }
  
  if (!file.exists(paste0("data/processed/trainset_",existing_group,".rds")) && !file.exists(paste0("data/processed/testset_",existing_group,".rds"))){
    print(paste0("Dataset for existing group, ", existing_group, ", does not exist."))
    return()
  }
  
  trainset_df$threatened <- as.character(trainset_df$threatened)
  
  trainset_df[trainset_df$threatened == "TRUE",] <- trainset_df %>%
    filter(threatened == "TRUE") %>%
    mutate(threatened = "threatened")
  
  trainset_df[trainset_df$threatened == "FALSE",] <- trainset_df %>%
    filter(threatened == "FALSE") %>%
    mutate(threatened = "non_threatened")
  
  trainset_df$threatened <- as.factor(trainset_df$threatened)
  trainset_df$threatened <- factor(trainset_df$threatened, levels=c("threatened", "non_threatened"))
  
  # Get indexes used on existing dataset
  train_no_species_existing_group <- readRDS(paste0("data/processed/trainset_",existing_group,".rds"))
  test_no_species_existing_group <- readRDS(paste0("data/processed/testset_",existing_group,".rds"))
  
  species_train_no_species_existing_group <- train_no_species_existing_group %>%
    left_join(trainset_df, by=NULL) %>%
    dplyr::select("species")

  if (length(complete.cases(species_train_no_species_existing_group$species)) != nrow(train_no_species_existing_group)) {
    print("Error on join step of training set")
    return
  }

  species_test_no_species_existing_group <- test_no_species_existing_group %>%
    left_join(trainset_df, by=NULL) %>%
    dplyr::select("species")

  if (length(complete.cases(species_test_no_species_existing_group$species)) != nrow(test_no_species_existing_group)) {
    print("Error on join step of testing set")
    return
  }

  train <- trainset_df %>% filter(species %in% species_train_no_species_existing_group$species)
  test <- trainset_df %>% filter(species %in% species_test_no_species_existing_group$species)

  train_no_species <- train %>% dplyr::select(-c("species"))
  test_no_species <- test %>% dplyr::select(-c("species"))
  
  saveRDS(train_no_species, paste0("data/processed/trainset_",group_name,".rds"))
  saveRDS(test_no_species, paste0("data/processed/testset_",group_name,".rds"))
  
  return(list("train"= train_no_species, "test"=test_no_species))
}