
d<-df[1:100,]

keys1 <- d %>% filter(Event == "Cancel")
keys2 <- d %>% filter(Event == "Start")

keys1 %>% inner_join(keys2, by = "Customer_Key") -> k

k %>% filter(date.x-date.y <= 30) %>% pull(Customer_Key)

