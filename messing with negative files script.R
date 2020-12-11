bad3 <- read.delim("./input/absorbance_new/BD200804S12R3.txt", sep = "\t", 
                   header = FALSE)
good1 <- read.delim("./input/absorbance_new/BD200804S12R1.txt", sep = "\t",
                    header = FALSE)
good2 <- read.delim("./input/absorbance_new/BD200804S12R2.txt", sep = "\t",
                    header = FALSE)

ggplot()+
  geom_line(data = good1, aes(x = V1, y = V2))+
  geom_line(data = bad3, aes(x = V1, y = V2), color = "red")+
  geom_line(data = good2, aes(x = V1, y = V2), color = "blue")
