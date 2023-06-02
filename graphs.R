library(ggplot2)
library(zoo)

#Ubicación equipo personal
base <- read.csv("D:/RProjects/EEUU_PM/data.csv", skip = 1, as.is = TRUE)

attach(base)

date_m <- as.yearmon(base$Mensual, format = "%Y-%m")
date_t <- as.yearqtr(base$Trimestral, format = "%Y-0%q")

wmf <- function(filename = "", width = 7.7, height = 4.5, pointsize = 12)
{    
  win.metafile(filename = filename, width = width, height = height,
               pointsize = pointsize)
  par(bg = "white", las = 1)
}


#Grafica 1. Base Monetaria y Oferta de dinero.

wmf(file = "D:/RProjects/EEUU_PM/Grafica1_.wmf")

g1 <- ggplot(data = base, aes(date_m)) + 
  geom_line(aes(y = BM, color = "Base Monetaria"), lwd = 1, lty = 1, show.legend = FALSE) + 
  scale_color_manual(values=c('blue4')) +
  scale_x_continuous(expand = c(0,0), breaks = seq(2000,2022,2)) +
  ggtitle("Gr?fico 1. EEUU. Base Monetaria .", subtitle = "2000.01 - 2022.07")

g1 + labs(
  x = "A?o",
  y = "Miles de millones de d?lares",
  caption = "Fuente: Elaboraci?n propia con datos de la FRED. Nota: Observaciones mensuales") + 
  guides(color = guide_legend(title = NULL)) + 
  theme(
    plot.title = element_text(face = "bold", size = 13, family ="serif", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 10, family="mono", hjust = 0.5),
    plot.caption = element_text(family = "serif", size = 10, hjust = 0.5),
    axis.line = element_line(size = 1, linetype = 1),
    panel.background = element_rect(fill = 'white')) +
  geom_vline(xintercept = as.yearmon(
    c("2001-03", "2001-11", "2007-12", "2009-06", "2019-12", "2020-04")), lty = 3) +
  annotate("rect", 
           xmin = as.yearmon("2001-03"),
           xmax = as.yearmon("2001-11"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.3,
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearmon("2007-12"),
           xmax = as.yearmon("2009-06"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.3, 
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearmon("2019-12"),
           xmax = as.yearmon("2020-04"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.3, 
           fill = "gray50")

dev.off()

#Grafica 2. Base Monetaria y Agregados Monetarios.

wmf(file = "D:/RProjects/EEUU_PM/Grafica2_.wmf")

g2 <- ggplot(data = base, aes(date_m)) + 
  geom_line(aes(y = BM, color = "Base Monetaria"), lwd = 1, lty = 1) + 
  geom_line(aes(y = M1/5, color = "M1"), lwd = 1, lty = 1) +
  geom_line(aes(y = M2/5, color = "M2"), lwd = 1, lty = 1) +
  geom_line(aes(y = M3/5, color = "M3"), lwd = 1, lty =1) +
  scale_color_manual(values=c('#045EC7','#F7AD70', '#872F2F', 'black')) +
  scale_x_continuous(expand = c(0,0), breaks = seq(2000,2022,2)) +
  scale_y_continuous(expand = c(0,0), sec.axis = sec_axis(~.*5, name = "Miles de millones de d?lares")) +
  ggtitle("Gr?fico 2. EEUU. Base Monetaria y Agregados Monetarios.", subtitle = "2000.01 - 2022.07")

g2 + labs(
  x = "A?o",
  y = "Miles de millones de dolares",
  caption = "Fuente: Elaboraci?n propia con datos de la FRED. Nota: Observaciones mensuales
  M1, M2, M3 en el eje secundario") + 
  guides(color = guide_legend(title = NULL)) + 
  theme(
    plot.title = element_text(face = "bold", size = 13, family = "serif", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 10, family = "mono", hjust = 0.5),
    plot.caption = element_text(family = "serif", size = 10, hjust = 0.5),
    axis.text = element_text(size = 8),
    axis.line = element_line(size = 0.75, linetype = 1),
    panel.background = element_rect(fill = 'white'),
    legend.position = "top",
    legend.text = element_text(family = "serif", size = 8),
    legend.background = element_rect(fill = 'transparent'),
    legend.key = element_rect(fill = 'transparent')) +
  geom_vline(xintercept = as.yearmon(
    c("2001-03", "2001-11", "2007-12", "2009-06", "2019-12", "2020-04")), lty = 3) +
  annotate("rect", 
           xmin = as.yearmon("2001-03"),
           xmax = as.yearmon("2001-11"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2,
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearmon("2007-12"),
           xmax = as.yearmon("2009-06"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2, 
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearmon("2019-12"),
           xmax = as.yearmon("2020-04"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2, 
           fill = "gray50")
  
dev.off()

#Grafica 3. Multiplicador monetario
wmf(file = "D:/RProjects/EEUU_PM/Grafica3_.wmf")

g3 <- ggplot(data = base, aes(date_m)) + 
  geom_line(aes(y = BM/2000, color = "Base Monetaria"), lwd = 1, lty = 1) + 
  geom_line(aes(y = M2/2000, color = "M2"), lwd = 1, lty = 1) +
  geom_line(aes(y = Multiplicador, color = "Multiplicador"), lwd = 1, lty = 2) +
  scale_color_manual(values=c('blue4','#3A90A4', '#9E0000')) +
  scale_x_continuous(expand = c(0,0), breaks = seq(2000,2022,2)) +
  scale_y_continuous(expand = c(0,0), sec.axis = sec_axis(~.*2000, name = "Miles de millones de d?lares BM, M2")) +
  ggtitle("Gr?fico 3. EEUU. Multiplicador Monetario", subtitle = "2000.01 - 2022.07")

g3 + labs(
  x = "A?o",
  y = "Multiplicador",
  caption = "Fuente: Elaboraci?n propia con datos de la FRED. Nota: Observaciones mensuales.
  El Multiplicador se calcul? como la divisi?n de M2/BM") + 
  guides(color = guide_legend(title = NULL)) + 
  theme(
    plot.title = element_text(face = "bold", size = 13, family = "serif", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 10, family = "mono", hjust = 0.5),
    plot.caption = element_text(family = "serif", size = 10, hjust = 0.5),
    axis.line = element_line(size = 0.75, linetype = 1),
    panel.background = element_rect(fill = 'white'),
    legend.position = "top",
    legend.text = element_text(family = "serif", size = 8),
    legend.background = element_rect(fill = 'transparent'),
    legend.key = element_rect(fill = 'transparent')) +
  geom_vline(xintercept = as.yearmon(
    c("2001-03", "2001-11", "2007-12", "2009-06", "2019-12", "2020-04")), lty = 3) +
  annotate("rect", 
           xmin = as.yearmon("2001-03"),
           xmax = as.yearmon("2001-11"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2,
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearmon("2007-12"),
           xmax = as.yearmon("2009-06"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2, 
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearmon("2019-12"),
           xmax = as.yearmon("2020-04"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2, 
           fill = "gray50")

dev.off()

#Grafica 4. Primera funcion. Banquero del gobierno.

after2003 <- as.yearmon(subset(date_m, Mensual > "2002-12"), format = "%Y-%m")
CIN2003 <- subset(base$CIN, Mensual > "2002-12")
GOB2003 <- subset(base$GOB, Mensual > "2002-12")
DEP2003 <- subset(base$DEP, Mensual > "2002-12")

df<- data.frame(after2003, CIN2003, GOB2003, DEP2003)

wmf(file = "D:/RProjects/EEUU_PM/Grafica4_.wmf")

g4 <- ggplot(data = df, aes(after2003)) + 
  geom_line(aes(y = GOB2003, color = "Cuenta del Gobierno"), lwd = 0.75, lty = 1, show.legend = FALSE) + 
  scale_color_manual(values=c('#789003')) +
  scale_x_continuous(expand = c(0,0), breaks = seq(2003,2022,2)) +
  ggtitle("Gr?fico 4. EEUU. Funci?n de la FED como banquero del gobierno.", subtitle = "2003.01 - 2022.09")

g4 + labs(
  x = "A?o",
  y = "Miles de millones de d?lares",
  caption = "Fuente: Elaboraci?n propia con datos de la FRED. U.S. Treasury, General Account.
  Nota: Calculado como el promedio mensual de las observaciones semanales") + 
  guides(color = guide_legend(title = NULL)) + 
  theme(
    plot.title = element_text(face = "bold", size = 13, family = "serif", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 10, family = "mono", hjust = 0.5),
    plot.caption = element_text(family = "serif", size = 10, hjust = 0.5),
    axis.line = element_line(size = 0.75, linetype = 1),
    panel.background = element_rect(fill = 'white')) +
  geom_vline(xintercept = as.yearmon(
    c("2007-12", "2009-06", "2019-12", "2020-04")), lty = 3) +
  annotate("rect", 
           xmin = as.yearmon("2007-12"),
           xmax = as.yearmon("2009-06"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2, 
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearmon("2019-12"),
           xmax = as.yearmon("2020-04"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2, 
           fill = "gray50")
dev.off()

#Grafica 5. Segunda Funcion. Emisor de dinero de curso legal.

wmf(file = "D:/RProjects/EEUU_PM/Grafica5_.wmf")

g5 <- ggplot(data = base, aes(date_m)) + 
  geom_line(aes(y = BM, color = "BM Total"), lwd = 1, lty = 1) + 
  geom_line(aes(y = RESERVES, color = "Balances de reserva"), lwd = 1, lty = 2) +
  geom_line(aes(y = CURRENCY, color = "Moneda en circulacion"), lwd =1, lty = 2) +
  scale_color_manual(values=c('blue4','#3A90A4', '#9E0000')) +
  scale_x_continuous(expand = c(0,0), breaks = seq(2000,2022,2)) +
  ggtitle("Gr?fico 5. EEUU. Funcion de la FED como emisor de dinero de curso de legal.", subtitle = "2000.01 - 2022.09")

g5 + labs(
  x = "A?o",
  y = "Miles de millones de d?lares",
  caption = "Fuente: Elaboraci?n propia con datos de la FRED. Nota: Observaciones mensuales.") + 
  guides(color = guide_legend(title = NULL)) + 
  theme(
    plot.title = element_text(face = "bold", size = 13, family = "serif", hjust = 1),
    plot.subtitle = element_text(face = "italic", size = 10, family = "mono", hjust = 0.5),
    plot.caption = element_text(family = "serif", size = 10, hjust = 0.5),
    axis.line = element_line(size = 0.75, linetype = 1),
    panel.background = element_rect(fill = 'white'),
    legend.position = "top",
    legend.text = element_text(family = "serif", size = 8),
    legend.background = element_rect(fill = 'transparent'),
    legend.key = element_rect(fill = 'transparent')) +
  geom_vline(xintercept = as.yearmon(
    c("2001-03", "2001-11", "2007-12", "2009-06", "2019-12", "2020-04")), lty = 3) +
  annotate("rect", 
           xmin = as.yearmon("2001-03"),
           xmax = as.yearmon("2001-11"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2,
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearmon("2007-12"),
           xmax = as.yearmon("2009-06"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2, 
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearmon("2019-12"),
           xmax = as.yearmon("2020-04"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2, 
           fill = "gray50")
dev.off()

#Grafica 6. Tercera funcion. Guardian de las RRII.

wmf(file = "D:/RProjects/EEUU_PM/Grafica6_.wmf")

g6 <- ggplot(data = base, aes(date_m)) + 
  geom_line(aes(y = RI, color = "Reservas Internacionales"), lwd = 1, lty = 1) + 
  geom_line(aes(y = RIva/0.66, color = "Variaci?n anualizada de las RI"), lwd = 1, lty = 2) +
  scale_color_manual(values=c('#32949E', '#84D0D8')) +
  scale_x_continuous(expand = c(0,0), breaks = seq(2000,2022,1)) +
  scale_y_continuous(expand = c(0,0), sec.axis = sec_axis(~.*0.66, name = "Porcentaje")) +
  ggtitle("Gr?fico 6. EEUU. Funcion de la FED como guardian de las Reservas Internacionales.", subtitle = "2000.01 - 2022.09")

g6 + labs(
  x = "A?o",
  y = "Miles de millones de d?lares",
  caption = "Fuente: Elaboraci?n propia con datos de la FRED. Nota: Observaciones mensuales.") + 
  guides(color = guide_legend(title = NULL)) + 
  theme(
    plot.title = element_text(face = "bold", size = 12, family = "serif", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 10, family = "mono", hjust = 0.5),
    plot.caption = element_text(family = "serif", size = 10, hjust = 0.5),
    axis.line = element_line(size = 0.75, linetype = 1),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    panel.background = element_rect(fill = 'white'),
    legend.position = "top",
    legend.text = element_text(family = "serif", size = 8),
    legend.background = element_rect(fill = 'transparent'),
    legend.key = element_rect(fill = 'transparent')) +
  geom_vline(xintercept = as.yearmon(
    c("2001-03", "2001-11", "2007-12", "2009-06", "2019-12", "2020-04")), lty = 3) +
  annotate("rect", 
           xmin = as.yearmon("2001-03"),
           xmax = as.yearmon("2001-11"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2,
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearmon("2007-12"),
           xmax = as.yearmon("2009-06"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2, 
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearmon("2019-12"),
           xmax = as.yearmon("2020-04"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2, 
           fill = "gray50")

dev.off()

#Grafica 7. Cuarta funcion. Prestamista de ultima instancia.
wmf(file = "D:/RProjects/EEUU_PM/Grafica7_.wmf")

g7 <- ggplot(data = base, aes(date_t)) + 
  geom_line(aes(y = PrestamosVentanilla, color = "Pr?stamos a Bancos dom?sticos a trav?s
de la Ventanilla de descuento"), lwd = 1, lty = 1) + 
  scale_color_manual(values=c('#045EC7')) +
  scale_x_continuous(expand = c(0,0), breaks = seq(2000, 2022, 1)) +
  ggtitle("Gr?fico 7. EEUU. Funcion de la FED como prestamista de ?ltima instancia.", subtitle = "2000.Q1 - 2022.Q2")

g7 + labs(
  x = "A?o",
  y = "Miles de millones de d?lares",
  caption = "Fuente: Elaboraci?n propia con datos de la FRED. Nota: Observaciones trimestrales.") + 
  guides(color = guide_legend(title = NULL)) + 
  theme(
    plot.title = element_text(face = "bold", size = 12, family = "serif", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 10, family = "mono", hjust = 0.5),
    plot.caption = element_text(family = "serif", size = 10, hjust = 0.5),
    axis.line = element_line(size = 0.75, linetype = 1),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    panel.background = element_rect(fill = 'white'),
    legend.position = c(0.7, 0.7),
    legend.text = element_text(family = "serif", size = 8),
    legend.background = element_rect(fill = 'transparent'),
    legend.key = element_rect(fill = 'transparent')) +
  geom_vline(xintercept = as.yearqtr(c("2001-02", "2001-04", "2008-01", "2009-03", "2019-04", "2020-02")), lty = 3) +
  annotate("rect", 
           xmin = as.yearqtr("2001-02"),
           xmax = as.yearqtr("2001-04"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2,
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearqtr("2008-01"),
           xmax = as.yearqtr("2009-03"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2, 
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearqtr("2019-04"),
           xmax = as.yearqtr("2020-02"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2, 
           fill = "gray50")

dev.off()

#Grafica 8. Metas de Inflaci?n.

wmf(file = "D:/RProjects/EEUU_PM/Grafica8_.wmf") 

g8 <- ggplot(data = base, aes(date_m)) + 
  geom_line(aes(y = INFm, color = "Inflaci?n anual"), lwd = 1, lty = 1) + 
  geom_line(aes(y = TargetLow, color = "Limite inferior de la meta"), lwd = 1, lty = 2) +
  geom_line(aes(y = Target, color = "Meta"), lwd = 1, lty = 3) +
  geom_line(aes(y = TargetUp, color = "Limite superior de la meta"), lwd = 1, lty = 2) +
  scale_color_manual(values=c('#04C7C7', '#79ADDD', '#79ADDD', '#2E75B6')) +
  scale_x_continuous(expand = c(0,0), breaks = seq(2000,2022,1)) +
  ggtitle("Gr?fico 8. EEUU. La FED y el r?gimen de metas de inflaci?n.", subtitle = "2000.01 - 2022.09")

g8 + labs(
  x = "A?o",
  y = "Porcentaje",
  caption = "Fuente: Elaboraci?n propia con datos de la FRED. Nota: Observaciones mensuales.") + 
  guides(color = guide_legend(title = NULL)) + 
  theme(
    plot.title = element_text(face = "bold", size = 12, family = "serif", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 10, family = "mono", hjust = 0.5),
    plot.caption = element_text(family = "serif", size = 10, hjust = 0.5),
    axis.line = element_line(size = 0.75, linetype = 1),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    panel.background = element_rect(fill = 'white'),
    legend.position = "top",
    legend.text = element_text(family = "serif", size = 8),
    legend.background = element_rect(fill = 'transparent'),
    legend.key = element_rect(fill = 'transparent')) +
  annotate("rect", 
           xmin = as.yearmon("2001-03"),
           xmax = as.yearmon("2001-11"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2,
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearmon("2007-12"),
           xmax = as.yearmon("2009-06"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2, 
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearmon("2019-12"),
           xmax = as.yearmon("2020-04"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2, 
           fill = "gray50")
dev.off ()

#Gr?fica 9
wmf(file = "D:/RProjects/EEUU_PM/Grafica9_.wmf") 

g9 <- ggplot(data = base, aes(date)) + 
  geom_line(aes(y = Inflacion, color = "Inflaci?nn Anualizada"), lwd = .75, lty = 1) + 
  geom_line(aes(y = Crecimiento, color = "Variaci?n anual del PIB"), lwd = .75, lty = 1) +
  geom_line(aes(y = FFR, color = "Tasa de Fondos Federales"), lwd = 1, lty = 2) +
  scale_color_manual(values=c('blue4','black', 'gold')) +
  scale_x_continuous(expand = c(0,0), breaks = seq(2000,2022,1)) +
  ggtitle("Gr?fico 9. EEUU. Inflaci?n, Crecimiento y Tasa de Inter?s", subtitle = "2000.Q1 - 2022.Q2")

g9 + labs(
  x = "Año",
  y = "%",
  caption = "Fuente: Elaboración propia con datos del BLS para la inflaci?n, la FRED para la Tasa de Inter?s y el Buro de An?lisis Econ?mico
para el producto.Nota: La tasa de inter?s es el promedio trimestral de las observaciones de la tasa de fondos federales") + 
  guides(color = guide_legend(title = NULL)) + 
  theme(
    plot.title = element_text(face = "bold", family = "serif", size = 12, hjust = 0.5),
    plot.subtitle = element_text(face = "italic", family = "mono", size = 10, hjust = 0.5),
    plot.caption = element_text(family = "serif", size = 8, hjust = 0.5),
    axis.line = element_line(size = 0.75, linetype = 1),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    panel.background = element_blank(),
    legend.position = "top",
    legend.text = element_text(family = "serif", size = 8),
    legend.background = element_rect(fill =  'transparent'),
    legend.key = element_rect(fill = 'transparent')) +
  geom_hline(yintercept = 0, lty = 5, lwd = 1, color = "gray50") +
  annotate ("rect", 
           xmin = as.yearmon("2001-03"),
           xmax = as.yearmon("2001-11"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2,
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearmon("2007-12"),
           xmax = as.yearmon("2009-06"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2, 
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearmon("2019-12"),
           xmax = as.yearmon("2020-04"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2, 
           fill = "gray50")

dev.off()

#Grafica 10
wmf(file = "D:/RProjects/EEUU_PM/Grafica 10")
g10 <- ggplot(data = base, aes(date)) + 
  geom_line(aes(y = Inflacion, color = "Inflaci?n Anualizada"), lwd = .75, lty = 1) + 
  geom_line(aes(y = Crecimiento, color = "Variaci?n anual del PIB"), lwd = .75, lty = 1) +
  geom_line(aes(y = Credito, color = "Variaci?n Anual del Cr?dito Total"), lwd =.75, lty = 1) + 
  geom_line(aes(y = FFR, color = "Tasa de Fondos Federales"), lwd = 1.1, lty = 3) +
  scale_color_manual(values=c('blue4','black', 'red', 'gold')) +
  scale_x_continuous(expand = c(0,0), breaks = seq(2000,2022, 1)) +
  ggtitle("Gr?fico 10. EEUU. Inflaci?n, Crecimiento, Tasa de Inter?s y Cr?dito total ", subtitle = "2000.Q1-2022.Q2")

g10 + labs(
  x = "A?o",
  y = "%",
  caption = "Fuente: Elaboraci?n propia con datos de la FRED para el cr?dito total y la tasa de inter?s, el BSL para la inflaci?n y el BEA para
  el producto. Nota: La tasa de inter?s es el promedio trimestral de las observaciones de la tasa de fondos federales") + 
  guides(color = guide_legend(title = NULL)) + 
  theme(
    plot.title = element_text(face = "bold", family = "serif", size = 12, hjust = 0.5),
    plot.subtitle = element_text(face = "italic", family = "mono", size = 10, hjust = 0.5),
    axis.line = element_line(size = 0.75, linetype = 1),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    panel.background = element_blank(),
    legend.position = c(0.6, 0.8),
    legend.text = element_text(family = "serif", size = 7),
    legend.background = element_rect(fill = 'transparent'),
    legend.key = element_rect(fill = 'transparent')) +
  geom_hline(yintercept = 0, lty = 5, lwd = 1, color = "gray50") + 
  geom_vline(xintercept = as.yearqtr(c("2000-04", "2001-04", "2008-01", "2009-02", "2019-02", "2020-02")), lty =3) +
  annotate("rect", 
           xmin = as.yearqtr("2000-04"),
           xmax = as.yearqtr("2001-04"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2,
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearqtr("2008-01"),
           xmax = as.yearqtr("2009-02"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2, 
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearqtr("2019-02"),
           xmax = as.yearqtr("2020-02"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2, 
           fill = "gray50")
dev.off()

#Grafica 11.
wmf(file = "D:/RProjects/EEUU_PM/Grafica 11")
g11 <- ggplot(data = base, aes(date)) + 
  geom_line(aes(y = Inflacion, color = "Inflaci?n Anualizada"), lwd = .75, lty = 1) + 
  geom_line(aes(y = Crecimiento/2, color = "Variaci?n anual del PIB"), lwd = .75, lty = 1) +
  geom_line(aes(y = Tdc/2, color = "Variaci?n Anual del TDC Nominal"), lwd =.75, lty = 1) + 
  geom_line(aes(y = FFR, color = "Tasa de Fondos Federales"), lwd = .75, lty =3) +
  scale_color_manual(values=c('blue4','black', '#FFBD5A', '#872F2F')) +
  scale_x_continuous(expand = c(0,0), breaks = seq(2000,2022, 1)) +
  scale_y_continuous(sec.axis = sec_axis(~.*2, name = "%")) +
  ggtitle("Gr?fico 11. EEUU. Inflaci?n, Crecimiento, Tasa de Inter?s y Tipo de Cambio Nominal ", subtitle = "2000.Q1-2022.Q2")

g11 + labs(
  x = "A?o",
  y = "%",
  caption = "Fuente: Elaboraci?n propia con datos de la FRED para el TDC y la tasa de inter?s, el BSL para la 
  inflaci?n y el BEA  para el producto. Nota: TDC nominal en el segundo eje.") + 
  guides(color = guide_legend(title = NULL)) + 
  theme(
    plot.title = element_text(face = "bold", family = "serif", size = 12, hjust = 0.5),
    plot.subtitle = element_text(face = "italic", family = "mono", size = 10, hjust = 0.5),
    plot.caption = element_text(family = "serif", size = 9, hjust = 0.5),
    axis.line = element_line(size = 0.75, linetype = 1),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    panel.background = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 8),
    legend.background = element_rect(fill = 'transparent'),
    legend.key = element_rect(fill = 'transparent')) +
  geom_hline(yintercept = 0, lty = 5, lwd = 1, color = "gray50") + 
  geom_vline(xintercept = as.yearqtr(c("2000-04", "2001-04", "2008-01", "2009-02", "2019-02", "2020-02")), lty =3) +
  annotate("rect", 
           xmin = as.yearqtr("2000-04"),
           xmax = as.yearqtr("2001-04"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2,
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearqtr("2008-01"),
           xmax = as.yearqtr("2009-02"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2, 
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearqtr("2019-02"),
           xmax = as.yearqtr("2020-02"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2, 
           fill = "gray50")

dev.off()

#Grafica 12.
wmf(file = "D:/RProjects/EEUU_PM/Grafica 12")
g12 <- ggplot(data = base, aes(date)) + 
  geom_line(aes(y = Inflacion, color = "Inflaci?n Anualizada"), lwd = 1, lty = 1) + 
  geom_line(aes(y = Crecimiento, color = "Variaci?n anual del PIB"), lwd = 1, lty = 1) +
  geom_line(aes(y = NASDAQva / 4, color = "Variaci?n Anual del ?ndice NASDAQ Composite"), lwd =1, lty = 1) + 
  geom_line(aes(y = FFR, color = "Tasa de Fondos Federales"), lwd = 1.1, lty = 3) +
  scale_color_manual(values=c('blue4','black', '#C7046D', 'gold')) +
  scale_x_continuous(expand = c(0,0), breaks = seq(2000,2022, 1)) +  
  scale_y_continuous(sec.axis = sec_axis(~.*4, name = "%")) +
  ggtitle("Gr?fico 12. EEUU. Inflaci?n, Crecimiento, Tasa de Inter?s e ?ndice de Bolsa de Valores", subtitle = "2000.Q1-2022.Q2")

g12 + labs(
  x = "A?o",
  y = "%",
  caption = "Fuente: Elaboraci?n propia con datos de la FRED para la tasa de inter?s, el BSL para la inflaci?n y el BEA para el producto.Nota: La tasa de inter?s es el
promedio trimestral de las observaciones de la tasa de fondos federales y de Yahoo Finance para el NASDAQ Composite. Segundo eje: NASDAQ") + 
  guides(color = guide_legend(title = NULL)) + 
  theme(
    plot.title = element_text(face = "bold", family = "serif", size = 12, hjust = 0.5),
    plot.subtitle = element_text(face = "italic", family = "mono", size = 10, hjust = 0.5),
    plot.caption = element_text(family = "serif", size = 8, hjust = 0),
    axis.line = element_line(size = 0.75, linetype = 1),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    panel.background = element_blank(),
    legend.position = "top",
    legend.text = element_text(family = "serif", size = 8),
    legend.background = element_rect(fill = 'transparent'),
    legend.key = element_rect(fill = 'transparent')) +
  geom_hline(yintercept = 0, lty = 5, lwd = 1, color = "gray50") + 
  geom_vline(xintercept = as.yearqtr(c("2000-04", "2001-04", "2008-01", "2009-02", "2019-02", "2020-02")), lty =3) +
  annotate("rect", 
           xmin = as.yearqtr("2000-04"),
           xmax = as.yearqtr("2001-04"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2,
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearqtr("2008-01"),
           xmax = as.yearqtr("2009-02"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2, 
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearqtr("2019-02"),
           xmax = as.yearqtr("2020-02"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2, 
           fill = "gray50")

dev.off()

#Grafica 13.

wmf(file = "D:/Archivos de Clase/Pol?tica Monetaria/Graficas TF/Grafica13_.wmf") 

g13 <- ggplot(data = base, aes(date_m)) + 
  geom_line(aes(y = INFm, color = "Inflaci?n anual"), lwd = 1, lty = 1) + 
  geom_line(aes(y = TargetLow, color = "Limite inferior de la meta"), lwd = 1, lty = 2) +
  geom_line(aes(y = Target, color = "Meta"), lwd = 1, lty = 3) +
  geom_line(aes(y = TargetUp, color = "Limite superior de la meta"), lwd = 1, lty = 2) +
  geom_line(aes(y = Expectativas, color = "Inflacion esperada"), lwd = 1, lty = 1) +
  scale_color_manual(values=c('#04C7C7', "#789003", '#79ADDD', '#79ADDD', '#2E75B6')) +
  scale_x_continuous(expand = c(0,0), breaks = seq(2000,2022,1)) +
  ggtitle("Gr?fico 13. EEUU. Expectatitvas de inflaci?n.", subtitle = "2000.01 - 2022.09")

g13 + labs(
  x = "A?o",
  y = "Porcentaje",
  caption = "Fuente: Elaboraci?n propia con datos de la FRED. Nota: Las expectativas de inflaci?n se obtuvieron de la inflaci?n
esperada a 1 a?os que calcula la FED, basada en parte en encuestas al p?blico. Observaciones mensuales.") + 
  guides(color = guide_legend(title = NULL)) + 
  theme(
    plot.title = element_text(face = "bold", size = 12, family = "serif", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 10, family = "mono", hjust = 0.5),
    plot.caption = element_text(family = "serif", size = 9, hjust = 0.5),
    axis.line = element_line(size = 0.75, linetype = 1),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    panel.background = element_rect(fill = 'white'),
    legend.position = "top",
    legend.text = element_text(family = "serif", size = 8),
    legend.background = element_rect(fill = 'transparent'),
    legend.key = element_rect(fill = 'transparent')) +
  geom_vline(xintercept = as.yearmon(
    c("2001-03", "2001-11", "2007-12", "2009-06", "2019-12", "2020-04")), lty = 3) +
  annotate("rect", 
           xmin = as.yearmon("2001-03"),
           xmax = as.yearmon("2001-11"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2,
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearmon("2007-12"),
           xmax = as.yearmon("2009-06"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2, 
           fill = "gray50") +
  annotate("rect", 
           xmin = as.yearmon("2019-12"),
           xmax = as.yearmon("2020-04"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2, 
           fill = "gray50")

dev.off()



