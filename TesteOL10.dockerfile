# Usa a imagem padrão do Oracle Linux 10 Slim
FROM oraclelinux:10-slim

# Define argumentos de build para o nome e ID do usuário
ARG UNAME
ARG UID

# Atualiza a lista de pacotes e instala as dependências usando dnf
RUN dnf -y install oracle-epel-release-el10 && \
    dnf -y update && \
    dnf -y install \
    git \
    ripgrep \
    fd-find \
    fzf \
    "Development Tools" \
    curl \
    coreutils \
    pandoc \
    python3 \
    python3-pip \
    clang \
    gdb \
    cmake \
    clangd \
    valgrind \
    sudo \
    wget \
    unzip \
    fontconfig \
    libgccjit-devel \
    emacs && \
    dnf clean all

# Cria um grupo e um usuário com o mesmo UID e GID do host
RUN groupadd -g $UID $UNAME
RUN useradd -u $UID -g $UID -m -s /bin/bash $UNAME
RUN echo "$UNAME ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

# Define o novo usuário como o usuário padrão
USER $UNAME

# Define o diretório de trabalho no home do usuário
WORKDIR /home/$UNAME

# Instala a norminette da 42
RUN python3 -m pip install -U norminette --break-system-packages

# Clona o repositório do Doom Emacs.
# A flag --depth 1 é usada para clonar apenas o commit mais recente, economizando espaço.
RUN git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs

# Cria o diretório de configuração padrão do Doom Emacs.
RUN mkdir -p ~/.config/doom

# Instala as fontes Nerd Fonts (JetBrains Mono e Fira Code)
RUN mkdir -p ~/.local/share/fonts && \
    cd ~/.local/share/fonts && \
    wget https://github.com/ryanoasis/nerd-fonts/releases/latest/download/JetBrainsMono.zip && \
    wget https://github.com/ryanoasis/nerd-fonts/releases/latest/download/FiraCode.zip && \
    unzip -o JetBrainsMono.zip && \
    unzip -o FiraCode.zip && \
    rm JetBrainsMono.zip FiraCode.zip && \
    fc-cache -fv

# Limita o número de processos paralelos para a compilação nativa AOT
# para evitar consumo excessivo de memória durante o build.
ENV NATIVE_FULL_AOT_JOBS=8

# Instala o Doom Emacs
RUN ~/.config/emacs/bin/doom install --force --fonts --install --aot
