# Load toolbox functions
source("interface_functions.R")

# Read original SWMM input file using swmmr::read_inp
inp <- read_inp("./example/example4/raw_catchment.inp")

# Read GI modeling strategy using read_GI_plan
GI_plan <- read_GI_plan("./example/example4/gi_plan.csv")

# Write routing interface file using write_routing_interface_file
routing_interface_path <- "./example/example4/routing_interface.txt"
write_routing_interface_file(GI_plan, inp, routing_interface_path)

# Modify object associated with SWMM input file using modify_inp
new_inp <- modify_inp(GI_plan, inp, routing_interface_path)

# Write modified SWMM input file using swmmr::write_inp
new_inp_path <- "./example/example4/test.inp"
write_inp(new_inp, file = new_inp_path)