# Usa a imagem padrão do Debian
FROM debian:latest

# Define argumentos de build para o nome e ID do usuário
ARG UNAME
ARG UID

# Atualiza a lista de pacotes e instala as dependências
RUN apt-get update && \
    apt-get install -y \
    git \
    ripgrep \
    fd-find \
    emacs \
    && rm -rf /var/lib/apt/lists/*

# Cria um grupo e um usuário com o mesmo UID e GID do host
RUN groupadd -g $UID $UNAME
RUN useradd -u $UID -g $UID -m -s /bin/bash $UNAME
RUN echo "$UNAME ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

# Define o novo usuário como o usuário padrão
USER $UNAME

# Define o diretório de trabalho no home do usuário
WORKDIR /home/$UNAME

# Clona o repositório do Doom Emacs.
# A flag --depth 1 é usada para clonar apenas o commit mais recente, economizando espaço.
RUN git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs

# Cria o diretório de configuração padrão do Doom Emacs.
RUN mkdir -p ~/.config/doom

# Instala o Doom Emacs
RUN ~/.config/emacs/bin/doom install
