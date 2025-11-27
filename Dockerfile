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
    # vterm support
    libtool \
    pkg-config \
    libvterm-dev \
    libncurses-dev \
    # Python
    python3 \
    python3-pip \
    pipx \
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
    # Shell
    zsh \
    zsh-autosuggestions \
    zsh-syntax-highlighting \
    shellcheck \
    # Locale para suporte UTF-8
    locales \
    && rm -rf /var/lib/apt/lists/*

# Configura locale para suporte UTF-8 (necessário para ícones)
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen

# Define argumentos de build para o nome e ID do usuário
ARG UID
ARG GID

# Cria um grupo e um usuário com o mesmo UID e GID do host
# Verifica se o grupo já existe antes de criar
RUN if ! getent group $GID > /dev/null 2>&1; then \
        groupadd -g $GID dev; \
    fi
RUN useradd -u $UID -g $GID -m -s /bin/zsh dev
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

# Instala a norminette da 42 usando pipx
RUN pipx install norminette

# Instala o Starship prompt
RUN curl -sS https://starship.rs/install.sh | sh -s -- -y

# Configura o Zsh
RUN touch ~/.zshrc && \
    echo '# PATH Configuration' >> ~/.zshrc && \
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.zshrc && \
    echo 'export PATH="$HOME/.config/emacs/bin:$PATH"' >> ~/.zshrc && \
    echo '' >> ~/.zshrc && \
    echo '# Zsh plugins' >> ~/.zshrc && \
    echo 'source /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh' >> ~/.zshrc && \
    echo 'source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh' >> ~/.zshrc && \
    echo '' >> ~/.zshrc && \
    echo '# Starship prompt' >> ~/.zshrc && \
    echo 'eval "$(starship init zsh)"' >> ~/.zshrc && \
    echo '' >> ~/.zshrc && \
    echo '# Aliases' >> ~/.zshrc && \
    echo 'alias ll="ls -lh"' >> ~/.zshrc && \
    echo 'alias la="ls -lah"' >> ~/.zshrc && \
    echo 'alias l="ls -CF"' >> ~/.zshrc && \
    echo 'alias n42="norminette"' >> ~/.zshrc

# Clona o repositório do Doom Emacs.
# A flag --depth 1 é usada para clonar apenas o commit mais recente, economizando espaço.
RUN git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs

# Cria o diretório de configuração padrão do Doom Emacs.
RUN mkdir -p ~/.config/doom

# Copia os arquivos de configuração do Doom
RUN git clone --depth 1 https://github.com/carlosalbertofilho/doomEmacsOnDocker ~/.config/doom

# Instala fontes recomendadas para o Doom Emacs
RUN mkdir -p ~/.local/share/fonts && \
    cd ~/.local/share/fonts && \
    # Nerd Fonts para ícones e símbolos
    wget -q https://github.com/ryanoasis/nerd-fonts/releases/latest/download/JetBrainsMono.zip && \
    wget -q https://github.com/ryanoasis/nerd-fonts/releases/latest/download/FiraCode.zip && \
    wget -q https://github.com/ryanoasis/nerd-fonts/releases/latest/download/Iosevka.zip && \
    wget -q https://github.com/ryanoasis/nerd-fonts/releases/latest/download/SourceCodePro.zip && \
    wget -q https://github.com/ryanoasis/nerd-fonts/releases/latest/download/NerdFontsSymbolsOnly.zip && \
    # Descompacta as fontes
    unzip -oq JetBrainsMono.zip && \
    unzip -oq FiraCode.zip && \
    unzip -oq Iosevka.zip && \
    unzip -oq SourceCodePro.zip && \
    unzip -oq NerdFontsSymbolsOnly.zip && \
    # Remove os arquivos zip
    rm -f *.zip && \
    # Atualiza o cache de fontes
    fc-cache -fv

# Limita o número de processos paralelos para a compilação nativa AOT
# para evitar consumo excessivo de memória durante o build.
ENV NATIVE_FULL_AOT_JOBS=8
ENV TERM=xterm-256color
ENV COLORTERM=truecolor
ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

# Instala o Doom Emacs
RUN ~/.config/emacs/bin/doom install --force --fonts --install --aot

# Sincroniza a configuração do Doom após copiar os arquivos
RUN ~/.config/emacs/bin/doom sync
