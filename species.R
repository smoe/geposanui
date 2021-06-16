library(data.table)

#' See [species].
species_matrix <- matrix(
    data = c(
        "abrachyrhynchus", "unknown", "Pink-footed goose",
        "acalliptera", "unknown", "Eastern happy",
        "acarolinensis", "shortlived", "Anole lizard",
        "acchrysaetos", "unknown", "Golden eagle", # (bird, Aquila chrysaetos chrysaetos)
        "acitrinellus", "unknown", "Midas child", # (fish)
        "amelanoleuca", "positive", "Panda",
        "amexicanus", "leaveout", "Mexican tetra", # (fish)
        "anancymaae", "unknown", "Ma's night monkey",
        "aocellaris", "unknown", "Clown anemonefish", # (fish)
        "apercula", "unknown", "Orange clownfish",
        "aplatyrhynchos", "shortlived", "Duck",
        "apolyacanthus", "unknown", "Spiny chromis", # (fish)
        "applatyrhynchos", "unknown", "Duck",
        "atestudineus", "unknown", "Climbing perch",
        "bbbison", "unknown", "American bison",
        "bgrunniens", "unknown", "Yak",
        "bihybrid", "replicative", "Bos indicus hybrid",
        "bmusculus", "unknown", "Blue whale",
        "bmutus", "unknown", "Wild yak",
        "bsplendens", "unknown", "Siamese fighting fish",
        "btaurus", "replicative", "Cow",
        "bthybrid", "replicative", "Bos taurus hybrid",
        "cabingdonii", "unknown", "Abington island giant tortoise",
        "capalliatus", "unknown", "Angola colobus", # (primate)
        "caperea", "unknown", "Brazilian guinea pig",
        "catys", "unknown", "Sooty mangabey",
        "cauratus", "unknown", "Goldfish", # (fish)
        "ccaeruleus", "unknown", "Blue tit", # (bird)
        "ccapucinus", "unknown", "Capuchin",
        "ccarpio", "unknown", "Common carp", # (fish)
        "cdromedarius", "unknown", "Arabian camel",
        "celegans", "shortlived", "Worm",
        "cfamiliaris", "replicative", "Dog",
        "cgchok1gshd", "unknown", "Chinese hamster",
        "cgobio", "unknown", "Channel bull blenny", # (fish)
        "cgpicr", "unknown", "Chinese hamster (PICR)",
        "charengus", "unknown", "Atlantic herring", # (fish)
        "chircus", "replicative", "Goat",
        "choffmanni", "positive", "Sloth", # lifespan 20-30 years
        "chyarkandensis", "unknown", "Yarkand deer",
        "cintestinalis", "leaveout", "Ciona intestinalis",
        "cjacchus", "replicative", "Marmoset",
        "cjaponica", "unknown", "Japanese quail",
        "clanigera", "unknown", "Long-tailed chinchilla",
        "cldingo", "unknown", "Dingo",
        "clfamiliaris", "replicative", "Dog",
        "clumpus", "unknown", "Lumpfish", # (fish)
        "cmilii", "unknown", "Elephant shark", # (fish)
        "cpbellii", "unknown", "Painted turtle",
        "cporcellus", "shortlived", "Guinea pig",
        "cporosus", "unknown", "Australian saltwater crocodile",
        "cpugnax", "unknown", "Ruff", # (bird)
        "csabaeus", "replicative", "African Green Monkey", # (Chlorocebus sabaeus)
        "csavignyi", "negative", "Ciona savignyi", # (early deep sea)
        "csemilaevis", "unknown", "Tongue sole", # (fish)
        "csyrichta", "unknown", "Tarsier",
        "cvariegatus", "unknown", "Sheepshead minnow", # (fish)
        "cwagneri", "unknown", "Chacoan peccary", # (pig)
        "dclupeoides", "unknown", "Denticle herring",
        "dlabrax", "unknown", "European seabass", # (fish)
        "dleucas", "unknown", "Beluga whale", # (mammal)
        "dmelanogaster", "shortlived", "Fruitfly",
        "dnovemcinctus", "negative", "Armadillo",
        "dordii", "shortlived", "Kangaroo rat",
        "drerio", "shortlived", "Zebrafish",
        "eaasinus", "unknown", "Donkey",
        "eburgeri", "unknown", "Hagfish", # (fish)
        "ecaballus", "replicative", "Horse",
        "ecalabaricus", "unknown", "Reedfish", # (fish)
        "eelectricus", "unknown", "Electric eel", # (fish)
        "eeuropaeus", "shortlived", "Hedgehog",
        "elucius", "unknown", "Northern pike", # (fish)
        "enaucrates", "unknown", "Live sharksucker", # (fish)
        "etelfairi", "shortlived", "Lesser hedgehog tenrec",
        "falbicollis", "leaveout", "Flycatcher", # (bird)
        "fcatus", "replicative", "Cat",
        "fheteroclitus", "unknown", "Mummichog", # (fish)
        "gaculeatus", "leaveout", "Stickleback", # (fish)
        "gaffinis", "unknown", "Western mosquitofish", # (fish)
        "gevgoodei", "unknown", "Goodes thornscrub tortoise",
        "gfortis", "unknown", "Medium gound-finch", # (bird)
        "ggallus", "shortlived", "Chicken",
        "ggorilla", "replicative", "Gorilla",
        "gmorhua", "leaveout", "Cod",
        "gwilldenowi", "unknown", "Blunt-snouted clingfish",
        "hburtoni", "unknown", "Burton's mouthbrooder", # (fish)
        "hcomes", "unknown", "Tiger sail seahorse",
        "hgfemale", "unknown", "Naked mole-rat female",
        "hhucho", "unknown", "Huchen", # (fish)
        "hsapiens", "replicative", "Human",
        "ipunctatus", "unknown", "Channel catfish", # (fish)
        "itridecemlineatus", "shortlived", "Squirrel",
        "jhyemalis", "unknown", "Dark-eyed junco", # (bird)
        "jjaculus", "unknown", "Lesser Egyptian jerboa", # (rodent)
        "kmarmoratus", "unknown", "Mangrove rivulus", # (fish)
        "lafricana", "replicative", "Elefant",
        "lbergylta", "unknown", "Ballan wrasse", # (fish)
        "lcalcarifer", "unknown", "Barramundi perch", # (fish)
        "lcanadensis", "unknown", "Lynx",
        "lchalumnae", "leaveout", "Coelacanth", # (fish)
        "lcrocea", "unknown", "Large yellow croaker", # (fish)
        "llaticaudata", "unknown", "Blue-ringed sea krait", # (snake)
        "lleishanense", "unknown", "Leishan spiny toad", # (reptile)
        "loculatus", "leaveout", "Spotted gar", # (fish)
        "marmatus", "unknown", "Zig-zag eel", # (fish)
        "mauratus", "unknown", "Golden Hamster", # (rodent)
        "mcaroli", "unknown", "Ryukyu mouse", # (rodent)
        "mdomestica", "leaveout", "Mus m. domesticus",
        "meugenii", "negative", "Wallaby",
        "mfascicularis", "replicative", "Crab-eating macaque",
        "mfuro", "shortlived", "Ferret", # (rodent)
        "mgallopavo", "shortlived", "Turkey", # (bird)
        "mleucophaeus", "unknown", "Drill", # (primate)
        "mlucifugus", "shortlived", "Microbat",
        "mmmarmota", "unknown", "Alpine marmot",
        "mmola", "unknown", "Ocean sunfish", # (fish)
        "mmonoceros", "unknown", "Narwhal", # (mammal)
        "mmoschiferus", "unknown", "Siberian musk deer",
        "mmulatta", "replicative", "Macaque", # (primate)
        "mmurdjan", "unknown", "Pinecone soldierfish", # (fish)
        "mmurinus", "replicative", "Lemur", # - of particular interest since so small but a primate
        "mmusculus", "negative", "Mouse", # (rodent)
        "mnemestrina", "replicative", "Pig-tailed macaque", # (primate)
        "mochrogaster", "unknown", "Prairie vole", # (rodent)
        "mpahari", "unknown", "Shrew mouse", # (rodent)
        "mpfuro", "unknown", "Ferret", # (rodent)
        "mspicilegus", "unknown", "Steppe mouse", # (rodent)
        "mspretus", "unknown", "Algerian mouse", # (rodent)
        "mundulatus", "unknown", "Budgerigar",
        "mvitellinus", "unknown", "Golden-collared manakin",
        "mzebra", "unknown", "Zebra mbuna", # (fish)
        "nbrichardi", "unknown", "Lyretail cichlid", # (fish)
        "neugenii", "unknown", "Wallaby",
        "nfurzeri", "unknown", "Turquoise killifish", # (fish)
        "ngalili", "unknown", "Upper Galilee mountains blind mole rat",
        "nleucogenys", "replicative", "Gibbon",
        "nmeleagris", "unknown", "Helmeted guineafowl", # (bird)
        "nnaja", "unknown", "Indian cobra", # (snake)
        "nscutatus", "unknown", "Mainland tiger snake",
        "nvison", "unknown", "American mink",
        "oanatinus", "negative", "Platypus",
        "oarambouillet", "unknown", "Sheep",
        "oaries", "replicative", "Sheep (texel)",
        "ocuniculus", "unknown", "Rabbit",
        "odegus", "unknown", "Degu",
        "ogarnettii", "positive", "Bushbaby", # (https://en.wikipedia.org/wiki/Galago)
        "ojavanicus", "unknown", "Javanese ricefish", # (fish)
        "okisutch", "unknown", "Coho salmon", # (fish)
        "olatipes", "leaveout", "Japahese rice fish", # (fish - Oryzias latipes)
        "olhni", "unknown", "Japanese rice fish (HNI)", # (fish)
        "olhsok", "unknown", "Japanese rice fish", # (fish)
        "omelastigma", "unknown", "Indian rice fish", # (fish)
        "omykiss", "unknown", "Oncorhynchus mykiss", # (fish)
        "oniloticus", "leaveout", "Nile tilapia", # (fish - Oreochromis niloticus)
        "oprinceps", "shortlived", "Pika", # (Ochotona princeps) (https://en.wikipedia.org/wiki/Pika) - max age 7 years, closest to rabbits
        "osinensis", "unknown", "Chinese medaka", # (fish)
        "otshawytscha", "unknown", "Chinook salmon", # (fish)
        "pabelii", "replicative", "Orang utan", # # (primate)
        "panubis", "replicative", "Olive baboon",
        "pcapensis", "shortlived", "Hyrax", # (mammal)
        "pcatodon", "unknown", "Sperm whale", # (mammal)
        "pcinereus", "unknown", "Koala", # (mammal)
        "pcoquereli", "unknown", "Coquerel's sifaka",
        "pformosa", "leaveout", "Amazon mollie", # (fish)
        "pkingsleyae", "unknown", "P. kingsleyae", # (fish) # Paramormyrops kingsleyae
        "platipinna", "unknown", "Sailfin molly", # (fish)
        "pleo", "unknown", "Lion",
        "pmajor", "unknown", "Great Tit", # (bird)
        "pmarinus", "leaveout", "Lamprey", # (https://en.wikipedia.org/wiki/Lamprey) (Neunaugen) (fish)
        "pmbairdii", "unknown", "Northern American deer mouse", # latin broken
        "pmuralis", "unknown", "Common wall lizard", # (reptile)
        "pnattereri", "unknown", "Red-bellied piranha", # (fish)
        "pnyererei", "unknown", "Makobe Island cichlid", # (fish)
        "ppaniscus", "replicative", "Bonobo",
        "ppardus", "unknown", "Leopard",
        "pranga", "unknown", "Indian glassy fish", # (fish)
        "preticulata", "unknown", "Guppy", # (fish)
        "psimus", "unknown", "Greater bamboo lemur", # (primate)
        "psinensis", "maybe", "Chinese softshell turtle", # (https://en.wikipedia.org/wiki/Chinese_softshell_turtle)
        "psinus", "unknown", "Vaquita", # (mammal)
        "ptaltaica", "unknown", "Tiger",
        "ptephrosceles", "unknown", "Ugandan red Colobus",
        "ptextilis", "unknown", "Eastern brown snake", # (snake)
        "ptroglodytes", "replicative", "Chimp", # (primate)
        "pvampyrus", "unknown", "Megabat",
        # "pvampyrus", "leaveout", "Megabat", # incomplete Ensembl build
        "rbieti", "unknown", "Black snub-nosed monkey", # (primate)
        "rferrumequinum", "unknown", "Greater horseshoe bat",
        "rnorvegicus", "negative", "Rat", # (rodent)
        "rroxellana", "unknown", "Golden snub-nosed monkey", # (primate)
        "saraneus", "shortlived", "Shrew", # (https://en.wikipedia.org/wiki/Shrew)
        "saurata", "unknown", "Gilthead seabream", # (fish)
        "sbboliviensis", "unknown", "Bolivian squirrel monkey", # (primate)
        "scanaria", "unknown", "Common canary", # (bird)
        "scaustralis", "unknown", "African austrich", # (bird)
        "scerevisiae", "shortlived", "Yeast",
        "sdumerili", "unknown", "Greater amberjack", # (fish)
        "sfasciatus", "unknown", "Jewelled Blenny", # (fish)
        "sformosus", "unknown", "Asian arowana", # (fish)
        "sgrahami", "unknown", "Golden-line barbel", # (fish)
        "shabroptila", "unknown", "Kakapo (parrot)", # (bird)
        "sharrisii", "shoatlived", "Tasmanian devil",
        "sldorsalis", "unknown", "Yellowtail amberjack", # (fish)
        "slucioperca", "unknown", "Pike-perch", # (fish)
        "smaximus", "unknown", "Turbot", # (fish)
        "smerianae", "unknown", "Argentine black and white tegu", # (fish)
        "sorbicularis", "unknown", "Orbiculate cardinalfish",
        "spunctatus", "unknown", "Tuatara",
        "spartitus", "unknown", "Bicolor damselfish", # (fish)
        "ssalar", "unknown", "Atlantic salmon", # (fish)
        "sscrofa", "replicative", "Pig", # (mammal)
        "strutta", "unknown", "River trout", # (fish)
        "svulgaris", "unknown", "Eurasian red squirrel", # (rodent)
        "tbelangeri", "leaveout", "Tree shrew", # (https://en.wikipedia.org/wiki/Treeshrew)
        "tctriunguis", "unknown", "Three-toad box turtle", # (reptile)
        "tgelada", "replicative", "Gelada", # (primate)
        "tguttata", "shortlived", "Zebra finch", # (bird)
        "tnigroviridis", "leaveout", "Tetraodon", # (fish)
        "trubripes", "leaveout", "Fugu", # (fish)
        "tsyrichta", "leaveout", "Tarsius/Carlito syrichta", #  (https://en.wikipedia.org/wiki/Tarsier)
        "ttruncatus", "maybe", "Dolphin", # (mammal)
        "uamericanus", "unknown", "American black bear", # (mammal)
        "umaritimus", "unknown", "Polar bear", # (mammal)
        "uparryii", "unknown", "Arctic ground squirrel", # (rodent)
        "vpacos", "maybe", "Alpaca", # (https://en.wikipedia.org/wiki/Alpaca)
        "vursinus", "unknown", "Common wombat",
        "vvulpes", "unknown", "Red fox", # (mammal)
        "xcouchianus", "unknown", "Monterrey platyfish",
        "xmaculatus", "shortlived", "Platyfish ",
        "xtropicalis", "shortlived", "Frog", # (reptile)
        "zalbicollis", "unknown", "White-throated sparrow" # (bird)
    ),
    ncol = 3,
    byrow = TRUE,
)

colnames(species_matrix) <- c("id", "group", "label")

#' Constant data on species in Ensembl Mart.
#' 
#' This is a data table with the following columns:
#' - `id`: An unique identifier for the species (based on the latin name).
#' - `group`: Whether and how the species has replicative aging.
#' - `label`: A human readable, english label.
species <- data.table(species_matrix)