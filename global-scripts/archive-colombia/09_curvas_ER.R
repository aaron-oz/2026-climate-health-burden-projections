# Deaths attributable to temperature 2010-2019
# Author: Jean Carlo Pineda Lozano
# Date created: 20/04/2023
# Date modified: 20/04/2023
# Institution: World Bank

library(readxl)
library(tidyr)
library(janitor)
library(dplyr)
library(tidyverse)


# clear memory
rm(list = ls()); invisible(gc())

# data loading
list_bases <- list.files(path = "Bases/Burkart/ERF",
                         pattern = ".csv$", full.names = TRUE)
list_curves <- lapply(list_bases, read_csv)
names(list_curves) <- c("curve_erc", "curve_miocardiopatia_miocarditis", "curve_cardiopatía_hipertensiva",
                        "curve_cardiopatia_isquemica", "curve_acv", "curve_dm", "curve_relacionadas_animales",
                        "curve_desastres", "curve_ahogamiento", "curve_homicidio", "curve_lesiones_mecanicas",
                        "inj_non_disaster_curve_samples", "curve_no_intencionales", "curve_suicidio",
                        "curve_relacion_transporte", "curve_accidentes_trafico", "curve_ivri",
                        "curve_epoc")

#mean and limits
list_curves_mean <- lapply(list_curves, function(x)
  x %>% 
    mutate(max = apply(x[,c(3:1002)], 1, quantile, probs = c(0.99)),
           lower = apply(x[,c(3:1002)], 1, quantile, probs = c(0.25)),
           upper = apply(x[,c(3:1002)], 1, quantile, probs = c(0.975)),
           mean = rowMeans(x[,c(3:1002)])
      ))

#summary measures and exponentiate
list_curves_mean <- lapply(list_curves_mean, function(x)
  x %>% 
    select(c(annual_temperature, daily_temperature, mean, lower, upper, max)) %>% 
    mutate(max = exp(max),
           mean = exp(mean),
           lower = exp(lower),
           upper = exp(upper)))

# save curves individually
names_curves <- names(list_curves_mean)
for (i in names_curves) {
  write.csv(list_curves_mean[[i]], file = paste0("Bases/Ambientales/Curvas ER/", i, ".csv"), 
            row.names = F)
}

# cause of death name
list_curves_mean$curve_erc$c_muerte <- "erc"
list_curves_mean$curve_miocardiopatia_miocarditis$c_muerte <- "miocardiopatia_miocarditis"
list_curves_mean$curve_cardiopatía_hipertensiva$c_muerte <- "cardiopatía_hipertensiva"
list_curves_mean$curve_cardiopatia_isquemica$c_muerte <- "cardiopatia_isquemica"
list_curves_mean$curve_acv$c_muerte <- "acv"
list_curves_mean$curve_dm$c_muerte <- "dm"
list_curves_mean$curve_relacionadas_animales$c_muerte <- "relacionadas_animales"
list_curves_mean$curve_desastres$c_muerte <- "desastres"
list_curves_mean$curve_ahogamiento$c_muerte <- "ahogamiento"
list_curves_mean$curve_homicidio$c_muerte <- "homicidio"
list_curves_mean$curve_lesiones_mecanicas$c_muerte <- "lesiones_mecanicas"
list_curves_mean$curve_no_intencionales$c_muerte <- "no_intencionales"
list_curves_mean$curve_suicidio$c_muerte <- "suicidio"
list_curves_mean$curve_relacion_transporte$c_muerte <- "relacion_transporte"
list_curves_mean$curve_accidentes_trafico$c_muerte <- "accidentes_trafico"
list_curves_mean$curve_ivri$c_muerte <- "ivri"
list_curves_mean$curve_epoc$c_muerte <- "epoc"

# combine curves into a single df
list_curves_mean$inj_non_disaster_curve_samples <- NULL
df_curves <- bind_rows(list_curves_mean)

saveRDS(df_curves, "Bases/Ambientales/Curvas ER/curves_all_causes.rds")



# plots ----------------------------------------------------------------
df_curves_plot <- df_curves %>% 
  filter(annual_temperature %in% c(6, 14, 28)) %>% 
  select(c_muerte, annual_temperature, daily_temperature, mean) %>% 
  mutate(annual_temperature = as.character(annual_temperature))

lista_c_muerte <- unique(df_curves_plot$c_muerte)
colores = c("#030303", "#CD2626", "#1874CD")

for (i in seq_along(lista_c_muerte)) {
  grafico_curvas =
    ggplot(subset(df_curves_plot, df_curves_plot$c_muerte == lista_c_muerte[i]),
           aes(x = daily_temperature, y = mean, color = annual_temperature)) +
    geom_line(linetype = 7,lwd = 0.5) + 
    # geom_point()+
    # geom_text(aes(label = tasa_esp), vjust = -0.3, color = "black", size = 6) +
    theme_minimal()+
    scale_color_manual(values = colores) +
    guides(color = guide_legend(title = "Temperatura media anual")) +
    labs(x="Temperatura",
         y="RR") +
    theme(text = element_text(size=10),
          plot.title = element_text(size=rel(1), vjust=2, face="bold", color="black", lineheight=1),
          axis.text.x = element_text(angle = 0, hjust = 1), 
          legend.position = "bottom",
          axis.title.x = element_text(face="bold", vjust=1.5, colour="black", size=rel(1)),
          axis.title.y = element_text(face="bold", vjust=1.5, colour="black", size=rel(1)),
          axis.text = element_text(colour = "black")) +
    ggtitle(paste("Curva ", lista_c_muerte[i])) #Set a title per group
  
  print (grafico_curvas)
  
  # save plot as .jpeg
  ggsave(grafico_curvas,
         file=paste("Salidas/Grafico curvas ER ", lista_c_muerte[i], ".jpeg", sep=''))
  
}
  