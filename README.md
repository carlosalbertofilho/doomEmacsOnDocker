# Emacs on Docker

## Visão Geral

Este projeto fornece um ambiente de desenvolvimento completo e isolado para o **Doom Emacs**, empacotado em um contêiner **Docker**. Ele foi projetado para oferecer uma experiência pronta para uso, sem a necessidade de instalar dependências diretamente no seu sistema operacional.

O contêiner é baseado em **Debian** e inclui uma seleção de ferramentas essenciais para desenvolvimento, com foco em **C/C++**.

-----

## Ferramentas Inclusas

- **Emacs 29** com a configuração **Doom Emacs**
- **Shell Moderno**:
  - `zsh` (shell padrão)
  - `zsh-autosuggestions` (sugestões automáticas)
  - `zsh-syntax-highlighting` (destaque de sintaxe)
  - `starship` (prompt personalizável)
- **Ferramentas de Desenvolvimento C/C++**:
  - `clang` e `clangd` (compilador e language server)
  - `gdb` (debugger)
  - `cmake` (build system)
  - `valgrind` (análise de memória)
  - `build-essential` (pacote com `gcc`, `g++`, `make`, etc.)
- **Utilitários de Linha de Comando**:
  - `git`
  - `ripgrep`
  - `fd-find`
  - `fzf`
  - `curl`
  - `pandoc`
- **Suporte a Python**:
  - `python3`, `pip` e `pipx`
  - `norminette` (para estudantes da 42, instalada via pipx)
- **Fontes Nerd Fonts**:
  - JetBrains Mono, Fira Code, Iosevka, Source Code Pro
- **GPG/Criptografia**:
  - Configuração completa de GPG para commits assinados

-----

## Como Usar

Siga os passos abaixo para construir a imagem e executar o contêiner.

### 1. Pré-requisitos

- **Docker** instalado e em execução na sua máquina.

### 2. Construa a Imagem Docker

O `Dockerfile` cria um usuário `dev` dentro do contêiner com o mesmo UID e GID do seu usuário local para evitar problemas de permissão com arquivos montados. O shell padrão é o **Zsh** com **Starship** como prompt.

Em seguida, no diretório do projeto, construa a imagem com o comando:

```bash
# Com docker 
docker build --build-arg UID=$(id -u) --build-arg GID=$(id -g) -t emacs-dev .

# Com podman
podman build --build-arg UID=$(id -u) --build-arg GID=$(id -g) -t emacs-dev .
```

**Nota para usuários macOS:** O build funciona corretamente mesmo com o grupo `staff` (GID 20), pois o Dockerfile detecta automaticamente grupos existentes.

Este comando cria uma imagem local chamada `emacs-dev`.

### 3. Execute o Contêiner

Para iniciar o Emacs, você precisa executar o contêiner, montando os diretórios de trabalho que deseja acessar.

```bash
# Com docker
docker run -it --rm \
  --hostname emacs42 \
  --ipc=host \
  -v /dev/shm:/dev/shm \
  -v "$HOME/Projects:/home/dev/Projects:z" \
  -v "$HOME/.ssh:/home/dev/.ssh:ro,z" \
  -v "$HOME/.gitconfig:/home/dev/.gitconfig:ro,z" \
  emacs-dev /usr/bin/zsh

# Com podman
podman run -it --rm \
  --hostname emacs42 \
  --ipc=host \
  -v /dev/shm:/dev/shm \
  -v "$HOME/Projects:/home/dev/Projects:z" \
  -v "$HOME/.ssh:/home/dev/.ssh:ro,z" \
  -v "$HOME/.gitconfig:/home/dev/.gitconfig:ro,z" \
  emacs-dev /usr/bin/zsh
```

**Explicação dos Parâmetros:**

- `docker run -it --rm`: Executa o contêiner de forma interativa (`-it`) e o remove ao final da sessão (`--rm`).
- `--hostname emacs42`: Define o hostname do contêiner (personalize como preferir).
- `--ipc=host`: Compartilha o namespace IPC com o host para comunicação entre processos.
- `-v /dev/shm:/dev/shm`: Monta memória compartilhada para melhor performance.
- `-v "HOST_PATH:CONTAINER_PATH:OPTIONS"`: Monta um diretório do seu computador (host) para dentro do contêiner.
  - `:ro`: Torna o volume somente leitura (`read-only`). Usado para chaves SSH e `.gitconfig` por segurança.
  - `:z`: Ajusta os rótulos de segurança do **SELinux**. **Essencial no Fedora/RHEL**. No macOS/Ubuntu sem SELinux, pode ser omitido.

**Nota sobre a configuração do Doom Emacs:** A imagem vem com o Doom Emacs instalado no diretório `~/.config/emacs`. Para usar sua **própria configuração** do Doom, monte seu diretório de configuração local:

```bash
-v "$HOME/.config/doom:/home/dev/.config/doom:z"
```

**Personalização do Zsh:** Se quiser usar seu `.zshrc` personalizado:

```bash
-v "$HOME/.zshrc:/home/dev/.zshrc:ro,z"
```

-----

## Dicas

### Crie um Alias para Facilitar

Digitar o comando `docker run` toda vez pode ser cansativo. Adicione um `alias` ao seu arquivo de configuração do shell (`~/.bashrc`, `~/.zshrc`, etc.) para simplificar o processo.

```bash
alias emacs-docker="docker run -it --rm \
  --hostname emacs42 \
  --ipc=host \
  -v /dev/shm:/dev/shm \
  -v \"\$HOME/Projects:/home/dev/Projects:z\" \
  -v \"\$HOME/.ssh:/home/dev/.ssh:ro,z\" \
  -v \"\$HOME/.gitconfig:/home/dev/.gitconfig:ro,z\" \
  emacs-dev"
```

Depois de adicionar o alias, recarregue seu shell (`source ~/.bashrc`) e simplesmente execute:

```bash
emacs-docker
```

### Atualizando a Imagem

Se você modificar o `Dockerfile` ou quiser atualizar as dependências, basta reconstruir a imagem com o mesmo comando de build:

```bash
# Com docker
docker build --build-arg UID=$(id -u) --build-arg GID=$(id -g) -t emacs-dev .

# Com podman
podman build --build-arg UID=$(id -u) --build-arg GID=$(id -g) -t emacs-dev .
```

### Recursos Adicionais

**Shell Zsh com Starship:**
O contêiner já vem configurado com Zsh, plugins de autosugestão e syntax highlighting, além do prompt Starship.

**Fontes:**
Quatro Nerd Fonts estão pré-instaladas (JetBrains Mono, Fira Code, Iosevka, Source Code Pro) para total compatibilidade com ícones do Doom Emacs.

**Norminette:**
Instalada via `pipx` em ambiente isolado. Execute com `norminette` ou use o alias `n42`.

**GPG:**
Configuração de GPG pronta para commits assinados. Monte seu diretório `~/.gnupg` se necessário:
```bash
-v "$HOME/.gnupg:/home/dev/.gnupg:z"
```

