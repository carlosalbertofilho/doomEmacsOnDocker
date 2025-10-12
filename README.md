# Emacs on Docker

## Visão Geral

Este projeto fornece um ambiente de desenvolvimento completo e isolado para o **Doom Emacs**, empacotado em um contêiner **Docker**. Ele foi projetado para oferecer uma experiência pronta para uso, sem a necessidade de instalar dependências diretamente no seu sistema operacional.

O contêiner é baseado em **Debian** e inclui uma seleção de ferramentas essenciais para desenvolvimento, com foco em **C/C++**.

-----

## Ferramentas Inclusas

- **Emacs 29** com a configuração **Doom Emacs**
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
  - `python3` e `pip`
  - `norminette` (para estudantes da 42)

-----

## Como Usar

Siga os passos abaixo para construir a imagem e executar o contêiner.

### 1. Pré-requisitos

- **Docker** instalado e em execução na sua máquina.

### 2. Construa a Imagem Docker

O `Dockerfile` cria um usuário dentro do contêiner com o mesmo nome e ID do seu usuário local para evitar problemas de permissão com arquivos montados.

Primeiro, obtenha seu nome de usuário e ID:

```bash
echo "UNAME=$(whoami)" # Nome de usuário
echo "UID=$(id -u)"   # ID de usuário
```

Em seguida, no diretório do projeto, construa a imagem com o comando:

```bash
docker build --build-arg UNAME=$(whoami) --build-arg UID=$(id -u) -t emacs-dev .
```

Este comando cria uma imagem local chamada `emacs-dev`.

### 3. Execute o Contêiner

Para iniciar o Emacs, você precisa executar o contêiner, montando os diretórios de trabalho que deseja acessar.

```bash
docker run -it --rm \
  -v "$HOME/Projects:/home/$(whoami)/Projects:z" \
  -v "$HOME/.ssh:/home/$(whoami)/.ssh:ro,z" \
  emacs-dev
```

**Explicação dos Parâmetros:**

- `docker run -it --rm`: Executa o contêiner de forma interativa (`-it`) e o remove ao final da sessão (`--rm`).
- `-v "HOST_PATH:CONTAINER_PATH:OPTIONS"`: Monta um diretório do seu computador (host) para dentro do contêiner.
  - `:ro`: Torna o volume somente leitura (`read-only`). Usado aqui para as chaves SSH por segurança.
  - `:z`: Ajusta os rótulos de segurança do **SELinux** no diretório do host. **Essencial se você usa Fedora, RHEL, etc.** Se você não usa um sistema com SELinux, pode omitir esta flag.

**Nota sobre a configuração do Doom Emacs:** A imagem já vem com uma configuração do Doom Emacs clonada a partir de [deste repositório](https://github.com/carlosalbertofilho/doom-emacs.git). Se você quiser usar sua **própria configuração local**, adicione a seguinte flag ao comando `docker run`:

```bash
-v "$HOME/.config/doom:/home/$(whoami)/.config/doom:z"
```

-----

## Dicas

### Crie um Alias para Facilitar

Digitar o comando `docker run` toda vez pode ser cansativo. Adicione um `alias` ao seu arquivo de configuração do shell (`~/.bashrc`, `~/.zshrc`, etc.) para simplificar o processo.

```bash
alias emacs-docker="docker run -it --rm \
  -v \"\$HOME/Projects:/home/\$(whoami)/Projects:z\" \
  -v \"\$HOME/.ssh:/home/\$(whoami)/.ssh:ro,z\" \
  emacs-dev"
```

Depois de adicionar o alias, recarregue seu shell (`source ~/.bashrc`) e simplesmente execute:

```bash
emacs-docker
```

### Atualizando a Imagem

Se você modificar o `Dockerfile` ou quiser atualizar as dependências, basta reconstruir a imagem com o mesmo comando de build:

```bash
docker build --build-arg UNAME=$(whoami) --build-arg UID=$(id -u) -t emacs-dev .
```

