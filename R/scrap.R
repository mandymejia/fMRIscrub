scrap

```{r}
Dat1.results = matrix(list(mean.lev=clever.Dat1.mean.lev, 
                           mean.rds=clever.Dat1.mean.rds , 
                           mean.rbd=clever.Dat1.mean.rbd, 
                           kurt.lev=clever.Dat1.kurt.lev, 
                           kurt.rds=NULL, 
                           kurt.rbd=clever.Dat1.kurt.rbd),
                      ncol=3, nrow=2)
colnames(Dat1.results) = c('leverage','rob dist subset','rob dist')
rownames(Dat1.results) = c('mean','kurtosis')

Dat2.results = matrix(list(mean.lev=clever.Dat2.mean.lev, 
                           mean.rds=clever.Dat2.mean.rds , 
                           mean.rbd=clever.Dat2.mean.rbd, 
                           kurt.lev=clever.Dat2.kurt.lev, 
                           kurt.rds=clever.Dat2.kurt.rds,
                           kurt.rbd=clever.Dat2.kurt.rbd),
                      ncol=3, nrow=2)
colnames(Dat2.results) = c('leverage','rob dist subset','rob dist')
rownames(Dat2.results) = c('mean','kurtosis')
```

```{r}
get.value = function(clever.result.list, value){
  null.indices = sapply(clever.result.list, is.null)
  value.list = vector('list', length(clever.result.list))
  value.list[!null.indices] = lapply(clever.result.list[!null.indices],
                                     '[[', value)
  if(!any(null.indices)){ return(value.list) }
  classes = sapply(value.list[!null.indices], class) 
  if(length(unique(classes)) > 1){ stop('Different classes.')}
  example = value.list[!null.indices][[1]]
  if(class(example) == 'data.frame'){
    if(length(unique(lapply(value.list[!null.indices], dim))) == 1){
      for(null.i in which(null.indices)){
        value.list[[null.i]] = as.data.frame(matrix(NA, nrow=nrow(example),
                                   ncol=ncol(example)))
      }
      value.list = lapply(value.list, as.matrix)
      
      out = array(NA, dim=(c(dim(example), length(value.list))))
      for(k in 1:length(value.list)){
        out[,,k] = value.list[[k]]
      }
    } else {
      stop('data.frames not of same dimension.')
    }
  } else {
    stop('Unsupported class.')
  }
  return(out)
}

Dat1.outliers = get.value(Dat1.results, 'outliers')
Dat2.outliers = get.value(Dat2.results, 'outliers')
```

```{r}
n=c(list(clever.Dat1.kurt.lev), list(clever.Dat1.kurt.rbd), list(clever.Dat1.mean.lev), list(clever.Dat1.mean.rbd), list(clever.Dat1.mean.rds))
names = c('kurt.lev','kurt.rbd','mean.lev','mean.rbd','mean.rds')

for(i in 1:length(n)){
  r = n[[i]]
  print(image(as.matrix(r$outliers), main=names[i]))
}

```

```{r}
n=c(list(clever.Dat2.kurt.lev), list(clever.Dat2.kurt.rbd), list(clever.Dat2.kurt.rds), list(clever.Dat2.mean.lev), list(clever.Dat2.mean.rbd), list(clever.Dat2.mean.rds))
names = c('kurt.lev','kurt.rbd','kurt.rds','mean.lev','mean.rbd','mean.rds')

for(i in 1:length(n)){
  r = n[[i]]
  print(image(as.matrix(r$outliers), main=names[i]))
}

```


for(k in 1:6){
  print(n[k])
  if(all(is.na(Dat1.outliers[,,k]))){next}
  print(image(Dat1.outliers[,,k]))
}

for(k in 1:6){
  print(n[k])
  if(all(is.na(Dat1.outliers[,,k]))){next}
  image(Dat2.outliers[,,k])
}
```