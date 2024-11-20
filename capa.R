
library(ggplot2)
library(ggimage)
library(showtext)


# Adicionar fontes para personalização
font_add_google("Roboto", "roboto")
showtext_auto()

# Links para ícones de ferramentas (use URLs de imagens PNG ou SVG)
hammer_url <- "https://cdn-icons-png.flaticon.com/512/3523/3523063.png"
toolbox_url <- "https://cdn-icons-png.flaticon.com/512/454/454570.png"

# Criar a base da capa
capa <- ggplot() + 
  # Fundo da capa
  geom_rect(aes(xmin = 0, xmax = 10, ymin = 0, ymax = 15), fill = "lightblue") +
  geom_rect(aes(xmin = 0.5, xmax = 9.5, ymin = 1, ymax = 14), fill = "white") +
  # Título e subtítulo
  geom_text(aes(x = 5, y = 12), label = "Técnicas e Métodos", size = 10, fontface = "bold", family = "roboto", color = "navy") +
  geom_text(aes(x = 5, y = 10), label = "de Pesquisa no", size = 8, family = "roboto", color = "navy") +
  geom_text(aes(x = 5, y = 8), label = "R", size = 16, fontface = "bold", family = "roboto", color = "darkred") +
  # Nome do autor
  geom_text(aes(x = 5, y = 3), label = "Luiz G. Almeida", size = 6, family = "roboto", color = "grey50") +
  # Adicionar ícones de ferramentas
  geom_image(aes(x = 2, y = 6, image = hammer_url), size = 0.15) +
  geom_image(aes(x = 8, y = 6, image = toolbox_url), size = 0.2) +
  # Tema
  theme_void() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"))

capa
