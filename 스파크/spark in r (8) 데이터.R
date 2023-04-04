
#경로로
letters <- data.frame(x = letters, y = 1:length(letters))

dir.create("data-csv")
write.csv(letters[1:3, ], "data-csv/letters1.csv", row.names = FALSE)
write.csv(letters[1:3, ], "data-csv/letters2.csv", row.names = FALSE)

do.call("rbind", lapply(dir("data-csv", full.names = TRUE), read.csv))
# -----------------------------------------------------------------------------
library(sparklyr)
sc <- spark_connect(master = "local", version = "3.0.0")

spark_read_csv(sc, "data-csv/letters1.csv")


#스키마
spec_with_r <- sapply(read.csv("data-csv/letters1.csv", nrows = 10), class)
spec_with_r
# --------------------------------------------------------------------------
spec_explicit <- c(x = "character", y = "numeric")
spec_explicit
# --------------------------------------------------------------------------
spark_read_csv(sc, "data-csv/letters1.csv", columns = spec_with_r)
# --------------------------------------------------------------------------
spec_compatible <- c(my_letter = "character", my_number = "character")

spark_read_csv(sc, "data-csv/letters1.csv", columns = spec_compatible)

#메모리
mapped_csv <- spark_read_csv(sc, "data-csv/letters1.csv", memory = FALSE)
# --------------------------------------------------------------------------
mapped_csv %>%
  dplyr::select(y) %>%
  dplyr::compute("test")

# 열
options(sparklyr.sanitize.column.names = FALSE)
copy_to(sc, iris, overwrite = TRUE)

# 데이터쓰기
# 복사
dir.create("largefile.txt")
write.table(matrix(rnorm(10 * 10^6), ncol = 10), "largefile.txt/1",
            append = T, col.names = F, row.names = F)
for (i in 2:30) 
  file.copy("largefile.txt/1", paste("largefile.txt/", i))
# --------------------------------------------------------------------------
spark_read_text(sc, "largefile.txt/1", memory = FALSE)







