
#### Creating needed output directories ####

## function
check.dir = function(x){
  if(!dir.exists(x)){
    dir.create(x)
  }
}

## define all directories (order of nesting is important)
### find root output dir

#### get the file separator for the current OS
sep = .Platform$file.sep

#### split the paths into components
parts1 = strsplit(output.dir, sep)[[1]]
parts2 = strsplit(sm.dir, sep)[[1]]

#### determine the length of the shorter path
min_len = min(length(parts1), length(parts2))

#### find the index of the first differing component
diff_index = 0
for(i in 1:min_len) {
  if(parts1[i] == parts2[i]){
    diff_index = i
  }else{
    break # stop when the first difference is found
  }
}

#### Select the common components up to the differing index
root.dir = do.call(file.path, as.list(parts1[1:diff_index]))

### Provide a list of all subdirectories
all.dirs = list(root.dir,
                output.dir, # all the main text figures
                c(output.dir, "csv"), # used in multiple scripts
                c(output.dir, "csv", "by5", ""), # used in matrix creation
                sm.dir, # all the supplementary material figures
                c(sm.dir, "csv"), # used in multiple scripts
                c(sm.dir, "csv", "by5"), # used in by5_80.R
                c(sm.dir, "csv", "freq"), # used in freq-m.R
                c(sm.dir, "csv", "sym"), # used in sym-mat.R
                c(sm.dir, "csv", "compare"), # used in inpers.R
                c(sm.dir, "csv", "unw"), # used in unw-analysis.R
                c(sm.dir, "csv", "raw"), # used in raw-analysis.R
                c(sm.dir, "csv", "race"), # used in race.R
                c(sm.dir, "csv", "sex"), # used in bysex.R
                c(sm.dir, "gam") # used in multiple scripts
)

## create needed folders if they do not exist
for(x in seq_along(all.dirs)){
  z = do.call(file.path, as.list(all.dirs[[x]]))
  check.dir(z) # checks if the directory exists and create if it does not
}


