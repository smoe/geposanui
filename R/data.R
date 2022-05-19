# All available methods from [geposan] and additional information on them.
methods <- geposan::all_methods()

# IDs of methods for geposan.
method_ids <- sapply(methods, function(method) method$id)

# Names of methods for geposan.
method_names <- sapply(methods, function(method) method$name)
