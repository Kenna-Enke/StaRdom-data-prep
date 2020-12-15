BD1 <- read.delim("./input/absorbance_new/BD200804S14R1.txt", sep = "\t", 
                   header = FALSE)
BD2 <- read.delim("./input/absorbance_new/BD200804S14R2.txt", sep = "\t",
                    header = FALSE)
BD3 <- read.delim("./input/absorbance_new/BD200804S14R3.txt", sep = "\t",
                    header = FALSE)

ggplot()+
  geom_line(data = BD1, aes(x = V1, y = V2))+
  geom_line(data = BD2, aes(x = V1, y = V2), color = "red")+
  geom_line(data = BD3, aes(x = V1, y = V2), color = "blue")


