require(powerSurvEpi)
require(purrr)
# Vignette: attached 

HR = c(1.2,1.25,1.30)
pE = 0.13
pU = 1- pE
pWhite = 0.49
pMino = c(0.16,0.19) # Variable: Black, Hispanic

df = expand.grid(HR,pE,pU,pWhite,pMino)
names(df) <- c("HR","pE","pU","pWhite","pMino")
df$p10 = rep(0.056,times=6)
df$p00 = rep(0.494,times=6) # White
df$p11 = rep(c(0.027,0.034),each=3) # Black, Hispanic
df$p01 = rep(c(0.134,0.159),each=3) # Black, Hispanic

power_out <- map(1:nrow(df),
    function(i){
      # powerEpiInt.default1(n = 300000,theta = df[i,"HR"], psi = 0.03,alpha = 0.05, 
      #                      p00 = df[i,"pU"]*df[i,"pWhite"],p01 = df[i,"pU"]*df[i,"pMino"], 
      #                      p10 = df[i,"pE"]*df[i,"pWhite"],p11 = df[i,"pE"]*df[i,"pMino"])
      powerEpiInt.default1(n = 300000*sum(df[i,c("p00","p01","p10","p11")]),theta = df[i,"HR"], psi = 0.03,alpha = 0.05, 
                           p00 = df[i,"p00"]/sum(df[i,c("p00","p01","p10","p11")]),p01 = df[i,"p01"]/sum(df[i,c("p00","p01","p10","p11")]), 
                           p10 = df[i,"p10"]/sum(df[i,c("p00","p01","p10","p11")]),p11 = df[i,"p11"]/sum(df[i,c("p00","p01","p10","p11")]))
     })

df[,"power"] <- unlist(power_out)


