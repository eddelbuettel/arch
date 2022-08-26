

## Arrow without Arrow, persisting missing values

```r
> summary(as.data.frame(as.list(from_arch_array(as_arch_array(palmerpenguins::penguins))), stringsAsFactors=TRUE))
      species          island    bill_length_mm bill_depth_mm  flipper_length_mm  body_mass_g       sex           year     
 Adelie   :152   Biscoe   :168   Min.   :32.1   Min.   :13.1   Min.   :172       Min.   :2700   female:165   Min.   :2007  
 Chinstrap: 68   Dream    :124   1st Qu.:39.2   1st Qu.:15.6   1st Qu.:190       1st Qu.:3550   male  :168   1st Qu.:2007  
 Gentoo   :124   Torgersen: 52   Median :44.5   Median :17.3   Median :197       Median :4050   NA's  : 11   Median :2008  
                                 Mean   :43.9   Mean   :17.2   Mean   :201       Mean   :4202                Mean   :2008  
                                 3rd Qu.:48.5   3rd Qu.:18.7   3rd Qu.:213       3rd Qu.:4750                3rd Qu.:2009  
                                 Max.   :59.6   Max.   :21.5   Max.   :231       Max.   :6300                Max.   :2009  
                                 NA's   :2      NA's   :2      NA's   :2         NA's   :2                                 
> 
```

Note that the `factor` variables are reconstructed at the end and become character
vectors first whereas they persist in arrow: 

```r
> summary(tibble::as_tibble(arrow::as_arrow_array(palmerpenguins::penguins)))
      species          island    bill_length_mm bill_depth_mm  flipper_length_mm  body_mass_g       sex           year     
 Adelie   :152   Biscoe   :168   Min.   :32.1   Min.   :13.1   Min.   :172       Min.   :2700   female:165   Min.   :2007  
 Chinstrap: 68   Dream    :124   1st Qu.:39.2   1st Qu.:15.6   1st Qu.:190       1st Qu.:3550   male  :168   1st Qu.:2007  
 Gentoo   :124   Torgersen: 52   Median :44.5   Median :17.3   Median :197       Median :4050   NA's  : 11   Median :2008  
                                 Mean   :43.9   Mean   :17.2   Mean   :201       Mean   :4202                Mean   :2008  
                                 3rd Qu.:48.5   3rd Qu.:18.7   3rd Qu.:213       3rd Qu.:4750                3rd Qu.:2009  
                                 Max.   :59.6   Max.   :21.5   Max.   :231       Max.   :6300                Max.   :2009  
                                 NA's   :2      NA's   :2      NA's   :2         NA's   :2                                 

> 
```

We can use `arch` arrays in place of `Arrow`.

```r
con <- DBI::dbConnect(duckdb::duckdb())
duckdb::duckdb_register(con, "pp", from_arch_array(as_arch_array(palmerpenguins::penguins)))
result <- DBI::dbGetQuery(con, "SELECT species, island, body_mass_g * 1000 as body_mass_kg FROM pp where island == 'Torgersen' limit 15")
print(result)
DBI::dbDisconnect(con, shutdown=TRUE)
```

(The example is simplified. One clearly does not need to wrap an existing
data set which would work directly, but the example shows how to use a
`arch` object in `duckdb`, and we continue with the earlier example of
turning this standard dataset into a `arch` object.)
