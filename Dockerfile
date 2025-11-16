# Usa a imagem padrão do Debian
FROM debian:latest

# Atualiza a lista de pacotes e instala as dependências
RUN apt-get update && \
    apt-get install -y \
    # Ferramentas essenciais do sistema
    build-essential \
    coreutils \
    sudo \
    wget \
    curl \
    unzip \
    fontconfig \
    # Controle de versão
    git \
    # Ferramentas de busca e navegação
    ripgrep \
    fd-find \
    fzf \
    # Desenvolvimento C/C++
    clang \
    clangd \
    gdb \
    cmake \
    valgrind \
    libgccjit-14-dev \
    # Python
    python3 \
    python3-pip \
    # Documentação e conversão
    pandoc \
    # Editor e correção ortográfica
    emacs \
    ispell \
    # GPG e criptografia
    gnupg \
    gnupg2 \
    gpg-agent \
    pinentry-curses \
    pinentry-gtk2 \
    dirmngr \
    && rm -rf /var/lib/apt/lists/*

# Define argumentos de build para o nome e ID do usuário
ARG UID
ARG GID

# Cria um grupo e um usuário com o mesmo UID e GID do host
RUN groupadd -g $GID dev
RUN useradd -u $UID -g $GID -m -s /bin/bash dev
RUN echo "dev ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

# Define o novo usuário como o usuário padrão
USER dev

# Define o diretório de trabalho no home do usuário
WORKDIR /home/dev

# Configura o GPG para o usuário
RUN mkdir -p ~/.gnupg && \
    chmod 700 ~/.gnupg && \
    echo "use-agent" > ~/.gnupg/gpg.conf && \
    echo "pinentry-mode loopback" >> ~/.gnupg/gpg.conf && \
    echo "allow-loopback-pinentry" > ~/.gnupg/gpg-agent.conf && \
    chmod 600 ~/.gnupg/gpg.conf ~/.gnupg/gpg-agent.conf

# Instala a norminette da 42
RUN python3 -m pip install -U norminette --break-system-packages

# Instala a norminette da 42
RUN python3 -m pip install -U norminette --break-system-packages

# Descomenta GCC_COLORS e aliases no bashrc (incluindo os com indentação)
RUN sed -i -E 's/^(\s*)#(export GCC_COLORS=|alias (ll|la|l|dir|vdir|grep|fgrep|egrep)=)/\1\2/g' ~/.bashrc

# Ajusta o PATH e configurações do shell
RUN echo '' >> ~/.bashrc && \
    echo '# PATH Configuration' >> ~/.bashrc && \
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc && \
    echo '' >> ~/.bashrc

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
ENV TERM=xterm-256color

# Instala o Doom Emacs
RUN ~/.config/emacs/bin/doom install --force --fonts --install --aot
