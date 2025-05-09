# Read in and preprocess random dot data

read_randomdot <- function(path){
    d_randomdot <- read_csv(path) |>
        filter(!is.na(test_trials.thisN)) |>
        mutate(trial = test_trials.thisN + 1) |>
        select(id = participant,
               trial,
               direction,
               condition = randomInstr,
               corrAns,
               resp = respDots.keys,
               corr = respDots.corr,
               rt = respDots.rt)
    d_randomdot
}

d <- list.files(path = 'data/raw/', pattern = 'random') %>%
    paste('data/raw/', ., sep = '') |>
    map_dfr(read_randomdot)

d |> write.csv(file = "data/clean/dataset_random_dot_clean.csv", row.names = FALSE) # neuer Datensatz in anderen Ordner speichern um Verdoppelung zu vermeiden
