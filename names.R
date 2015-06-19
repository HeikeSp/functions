
names_treatment_sample_time <- c("control \n early", 
                                 "drought stress \n early", 
                                 "control \n late", 
                                 "drought stress \n late")

names_treatment_sample_time_2 <- c("control \n early/before", "drought stress \n early/before", 
                                 "control \n early/after", "drought stress \n early/after", 
                                 "control \n late/before", "drought stress \n late/before", 
                                 "control \n late/after", "drought stress \n late/after")

names_treatment_sample_time_jki <- c("control \n 18.05.2011", 
                                 "drought stress \n 18.05.2011", 
                                 "control \n 25.05.2011", 
                                 "drought stress \n 25.05.2011",
                                 "control \n 14.06.2011", 
                                 "drought stress \n 14.06.2011",
                                 "control \n 20.06.2011", 
                                 "drought stress \n 20.06.2011",
                                 "control \n 07.07.2011", 
                                 "drought stress \n 07.07.2011",
                                 "control \n 15.07.2011", 
                                 "drought stress \n 15.07.2011",
                                 "control \n 02.08.2011", 
                                 "drought stress \n 02.08.2011")

names_treatment_cultivar <- c("control \n Alegria", "drought stress \n Alegria", 
                              "control \n Milva", "drought stress \n Milva",  
							                "control \n Desiree", "drought stress \n Desiree", 
							                "control \n Saturna", "drought stress \n Saturna")

names_treatment_cultivar_reordered <- c("control \n Milva", "drought stress \n Milva",
                                        "control \n Alegria", "drought stress \n Alegria",
                                        "control \n Desiree", "drought stress \n Desiree",
                                        "control \n Saturna", "drought stress \n Saturna")

names_treatment_sample_time_cultivar <- c("control \n early, Alegria", 
                                          "drought stress \n early, Alegria", 
                                          "control \n late, Alegria", 
                                          "drought stress \n late, Alegria", 
                                          "control \n early, Desiree", 
                                          "drought stress \n early, Desiree", 
                                          "control \n late, Desiree", 
                                          "drought stress \n late, Desiree", 
                                          "control \n early, Milva", 
                                          "drought stress \n early, Milva", 
                                          "control \n late, Milva", 
                                          "drought stress \n late, Milva", 
                                          "control \n early, Saturna", 
                                          "drought stress \n early, Saturna", 
                                          "control \n late, Saturna", 
                                          "drought stress \n late, Saturna")

names_treatment_sample_time_cultivar_reordered <- c("control \n early, Milva", 
                                          "drought stress \n early, Milva", 
                                          "control \n late, Milva", 
                                          "drought stress \n late, Milva",
                                          "control \n early, Alegria", 
                                          "drought stress \n early, Alegria", 
                                          "control \n late, Alegria", 
                                          "drought stress \n late, Alegria", 
                                          "control \n early, Desiree", 
                                          "drought stress \n early, Desiree", 
                                          "control \n late, Desiree", 
                                          "drought stress \n late, Desiree", 
                                          "control \n early, Saturna", 
                                          "drought stress \n early, Saturna", 
                                          "control \n late, Saturna", 
                                          "drought stress \n late, Saturna")

names_treatment_cultivar_all <- c("control \n Albatros", "drought stress \n Albatros",
                                  "control \n Alegria", "drought stress \n Alegria",
                                  "control \n Burana", "drought stress \n Burana",
                                  "control \n Desiree", "drought stress \n Desiree",
                                  "control \n Eldena", "drought stress \n Eldena",
                                  "control \n Eurobravo", "drought stress \n Eurobravo",
                                  "control \n Euroflora", "drought stress \n Euroflora",
                                  "control \n Euronova", "drought stress \n Euronova",
                                  "control \n Euroresa", "drought stress \n Euroresa",
                                  "control \n Eurostarch", "drought stress \n Eurostarch",
                                  "control \n Eurotango", "drought stress \n Eurotango",
                                  "control \n Golf", "drought stress \n Golf",
                                  "control \n Jasia", "drought stress \n Jasia",
                                  "control \n Jumbo", "drought stress \n Jumbo",
                                  "control \n Karlena", "drought stress \n Karlena",
                                  "control \n Kiebitz", "drought stress \n Kiebitz",
                                  "control \n Kolibri", "drought stress \n Kolibri",
                                  "control \n Kormoran", "drought stress \n Kormoran",
                                  "control \n Kuras", "drought stress \n Kuras",
                                  "control \n Logo", "drought stress \n Logo",
                                  "control \n Maxi", "drought stress \n Maxi",
                                  "control \n Maxilla", "drought stress \n Maxilla",
                                  "control \n Milva", "drought stress \n Milva", 
                                  "control \n Pirol", "drought stress \n Pirol",
                                  "control \n Power", "drought stress \n Power",
                                  "control \n Priamos", "drought stress \n Priamos",
                                  "control \n Ramses", "drought stress \n Ramses",
                                  "control \n Saturna", "drought stress \n Saturna",
                                  "control \n Sibu", "drought stress \n Sibu",
                                  "control \n Sommergold", "drought stress \n Sommergold",
                                  "control \n Tomba", "drought stress \n Tomba",
                                  "control \n Tomensa", "drought stress \n Tomensa",
                                  "control \n Ulme", "drought stress \n Ulme",
                                  "control \n Verdi", "drought stress \n Verdi")

names_cultivar_all <- c("Albatros", "", "Alegria", "", "Burana", "", "Desiree", "", 
                        "Eldena", "", "Eurobravo", "",  "Euroflora", "", "Euronova", "",
                        "Euroresa", "", "Eurostarch", "", "Eurotango", "",  "Golf", "",
                        "Jasia", "", "Jumbo", "", "Karlena", "", "Kiebitz", "", 
                        "Kolibri", "", "Kormoran", "", "Kuras", "", "Logo", "", "Maxi", "",
                        "Maxilla", "", "Milva", "", "Pirol", "", "Power", "", "Priamos", "", 
                        "Ramses", "", "Saturna", "", "Sibu", "", "Sommergold", "", "Tomba", "", 
                        "Tomensa", "", "Ulme", "", "Verdi", "")

names_cultivar_all_ids <- c("2870", "", "Alegria", "", "2877", "", "Desiree", "", 
                        "2854", "", "2855", "",  "2856", "", "2857", "",
                        "2858", "", "2859", "", "2860", "",  "2878", "",
                        "2869", "", "2864", "", "2871", "", "2872", "", 
                        "2873", "", "2874", "", "2861", "", "2865", "", "2866", "",
                        "2875", "", "Milva", "", "2876", "", "2867", "", "2879", "", 
                        "2880", "", "Saturna", "", "2881", "", "2868", "", "Tomba", "", 
                        "2863", "", "2853", "", "2882", "")

# fuer MPI Feld 2013 --> Golf fehlt!
names_cultivar_33 <- c("Albatros", "", "Alegria", "", "Burana", "", "Desiree", "", 
                        "Eldena", "", "Eurobravo", "",  "Euroflora", "", "Euronova", "",
                        "Euroresa", "", "Eurostarch", "", "Eurotango", "",  
                        "Jasia", "", "Jumbo", "", "Karlena", "", "Kiebitz", "", 
                        "Kolibri", "", "Kormoran", "", "Kuras", "", "Logo", "", "Maxi", "",
                        "Maxilla", "", "Milva", "", "Pirol", "", "Power", "", "Priamos", "", 
                        "Ramses", "", "Saturna", "", "Sibu", "", "Sommergold", "", "Tomba", "", 
                        "Tomensa", "", "Ulme", "", "Verdi", "")

# fuer Dethlingen 2011 --> keine Check-Sorten!
names_cultivar_30 <- c("Albatros", "", "Burana", "", 
                        "Eldena", "", "Eurobravo", "",  "Euroflora", "", "Euronova", "",
                        "Euroresa", "", "Eurostarch", "", "Eurotango", "",  "Golf", "",
                        "Jasia", "", "Jumbo", "", "Karlena", "", "Kiebitz", "", 
                        "Kolibri", "", "Kormoran", "", "Kuras", "", "Logo", "", "Maxi", "",
                        "Maxilla", "", "Pirol", "", "Power", "", "Priamos", "", 
                        "Ramses", "", "Sibu", "", "Sommergold", "", "Tomba", "", 
                        "Tomensa", "", "Ulme", "", "Verdi", "")

names_trials <- c("MPI GH \n trial 1", "MPI GH \n trial 2", "MPI GH \n trial 3", 
                  "MPI GH \n trial 4", "MPI GH \n trial 5", "MPI GH \n trial 6", 
                  "JKI GH \n trial 1", "JKI GH \n trial 2", "JKI GH \n trial 3", 
                  "JKI GH \n trial 4", "MPI field \n trial 2011", "MPI field \n trial 2012", 
                  "MPI field \n trial 2013", "JKI field \n trial 2012", "JKI field \n trial 2013", 
                  "LWK field \n trial 2011", "LWK field \n trial 2012", "LWK field \n trial 2013")

names_trials_long <- c("MPI-MP \n greenhouse 1", "MPI-MP \n greenhouse 2", 
                       "MPI-MP \n greenhouse 3", "MPI-MP \n greenhouse 4", 
                       "MPI-MP \n greenhouse 5", "MPI-MP \n greenhouse 6", 
                       "JKI \n greenhouse 1", "JKI \n greenhouse 2", "JKI \n greenhouse 3", 
                       "JKI \n greenhouse 4", "MPI-MP \n field 2011", "MPI-MP \n field 2012", 
                       "MPI-MP \n field 2013", "JKI \n field 2012", "JKI \n field 2013", 
                       "LWK \n field 2011", "LWK \n field 2012", "LWK \n field 2013")

names_trials_phd <- c("MPI-MP \n greenhouse 1", "MPI-MP \n greenhouse 2", "MPI-MP \n greenhouse 3", 
                      "MPI-MP \n greenhouse 4", "MPI-MP \n greenhouse 5", "JKI \n greenhouse 1", 
                      "JKI \n greenhouse 2", "JKI \n greenhouse 3", "JKI \n greenhouse 4", 
                      "MPI-MP \n field 2011", "MPI-MP \n field 2012", "JKI \n field 2012", 
                      "JKI \n field 2013", "LWK \n field 2011", "LWK \n field 2012")

check_names <- c("Alegria", "Desiree", "Milva", "Saturna")

check_names_ordered <- c("Milva", "Alegria", "Desiree", "Saturna")

dethlingen_2011_names <- c("Albatros", "Burana", "Eldena", "Eurobravo", "Euroflora", "Euronova", 
                           "Euroresa", "Eurostarch",  "Eurotango",   "Golf",  "Jasia",  "Jumbo",
                           "Karlena",  "Kiebitz", "Kolibri",  "Kormoran",  "Kuras",  "Logo",
                           "Maxi", "Maxilla",  "Pirol",  "Power",  "Priamos", "Ramses",  "Sibu",  
                           "Sommergold",  "Tomba", "Tomensa",  "Ulme",  "Verdi")

cultivar_all_ids <- c("2870", "Alegria", "2877", "Desiree", "2854", "2855",  "2856", "2857", 
                            "2858", "2859", "2860",  "2878", "2869", "2864", "2871", "2872", 
                            "2873", "2874", "2861", "2865", "2866", "2875", "Milva", "2876", "2867", "2879", 
                            "2880", "Saturna", "2881", "2868", "2863", "2862", "2853", "2882")

names_cultivars <- c("Albatros", "Alegria", "Burana", "Desiree", "Eldena", "Eurobravo", 
                     "Euroflora", "Euronova", "Euroresa", "Eurostarch",  "Eurotango", 
                     "Golf", "Jasia", "Jumbo", "Karlena", "Kiebitz", "Kolibri", "Kormoran", 
                     "Kuras", "Logo", "Maxi", "Maxilla", "Milva", "Pirol", "Power", "Priamos", 
                     "Ramses", "Saturna", "Sibu", "Sommergold", "Tomba", "Tomensa", "Ulme", "Verdi")
