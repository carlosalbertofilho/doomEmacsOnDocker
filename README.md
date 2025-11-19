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
  - `shellcheck` (linter para shell scripts)
- **Suporte a Python**:
  - `python3`, `pip` e `pipx`
  - `norminette` (para estudantes da 42, instalada via pipx)
- **Fontes Nerd Fonts**:
  - JetBrains Mono, Fira Code, Iosevka, Source Code Pro, Symbols Only
- **GPG/Criptografia**:
  - Configuração completa de GPG para commits assinados
- **Integração 42 School**:
  - **Header 42** automático com keybinding `SPC c h`
  - **Flycheck + Norminette** com verificação em tempo real e highlight de erros
  - Atualização automática do header ao salvar
  - **Aliases Eshell** otimizados para projetos C
- **AI Assistants**:
  - **Ellama** com suporte para OpenAI GPT-4, GPT-3.5 e Google Gemini
  - Code completion, review, refactoring e chat

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
# Export envs
export OPENAI_API_KEY="DIGITE_SUA_API_KEY"
export FT_LOGIN="SE_LOGIN_42"

# Com docker
docker run -it --rm \
  --hostname emacs42 \
  --ipc=host \
  -v /dev/shm:/dev/shm \
  -v "$HOME/Projects:/home/dev/Projects:z" \
  -v "$HOME/.ssh:/home/dev/.ssh:ro,z" \
  -v "$HOME/.gitconfig:/home/dev/.gitconfig:ro,z" \
  -e OPENAI_API_KEY="$OPENAI_API_KEY"\
  -e FT_LOGIN="$FT_LOGIN" \
  --userns=keep-id \
  emacs-dev /usr/bin/zsh

# Com podman
podman run -it --rm \
  --hostname emacs42 \
  --ipc=host \
  -v /dev/shm:/dev/shm \
  -v "$HOME/Projects:/home/dev/Projects:z" \
  -v "$HOME/.ssh:/home/dev/.ssh:ro,z" \
  -v "$HOME/.gitconfig:/home/dev/.gitconfig:ro,z" \
  -e OPENAI_API_KEY="$OPENAI_API_KEY"\
  -e FT_LOGIN="$FT_LOGIN" \
  --userns=keep-id \
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

-----

## Funcionalidades 42 School

Este ambiente inclui integração completa com as ferramentas da 42 School:

### Header 42 Automático

O header padrão da 42 é inserido e atualizado automaticamente:

- **`SPC c h`** - Insere ou atualiza o header 42
- **Atualização automática** - O campo "Updated" é atualizado automaticamente ao salvar
- **Suporte a variáveis de ambiente** - Usa `FT_LOGIN` ou `USER` para nome do autor

### Verificação com Norminette

Integração do Flycheck com a Norminette para verificação em tempo real:

- **Verificação automática** ao salvar arquivos `.c` e `.h`
- **Highlight de erros** diretamente nas linhas com problemas
- **Mensagens contextualizadas** com dicas úteis para erros comuns
- **Keybindings**:
  - **`SPC m n`** (ou `, n`) - Executar verificação manual
  - **`SPC m N`** (ou `, N`) - Ativar/desativar verificação automática

### Fontes Configuradas

Quatro opções de Nerd Fonts prontas para uso no `config.el`:

- **Fira Code** (padrão) - Excelente suporte a ligaduras
- **JetBrains Mono** - Muito popular, ótima legibilidade
- **Iosevka** - Fonte estreita, maximiza espaço horizontal
- **Source Code Pro** - Clássica da Adobe, altamente legível

Para trocar de fonte, edite `~/.config/doom/config.el` e descomente a opção desejada.

### Eshell Aliases para 42

Aliases pré-configurados no Eshell (`SPC o e`) para agilizar o workflow:

- **`cc42 file.c`** - Compila com flags da 42 (`-Wall -Wextra -Werror`)
- **`r42`** - Executa `./a.out`
- **`clean42`** - Remove o binário `a.out`
- **`cr42 file.c`** - Compila e executa diretamente
- **`val42 file.c`** - Compila e executa com valgrind (leak check completo)
- **`n42 file.c`** - Executa norminette no arquivo

**Exemplo de uso:**
```bash
# No Eshell (SPC o e)
cc42 ft_strlen.c          # Compila
r42                       # Executa
cr42 ft_strlen.c          # Compila e executa
val42 ft_strlen.c         # Testa com valgrind
n42 ft_strlen.c           # Verifica com norminette
```

-----

## AI Assistants - Ellama

Integração com assistentes de IA para aumentar a produtividade no desenvolvimento.

### Provedores Suportados

- **OpenAI GPT-4** (padrão) - Melhor qualidade de resposta
- **OpenAI GPT-3.5 Turbo** - Mais rápido e econômico
- **Google Gemini Pro** - Alternativa do Google

### Configuração de API Keys

Defina as variáveis de ambiente antes de iniciar o container:

```bash
# Adicione ao comando docker run:
-e OPENAI_API_KEY="sk-sua-chave-aqui"
-e GEMINI_API_KEY="sua-chave-gemini"

# Ou adicione ao seu ~/.zshrc no host:
export OPENAI_API_KEY="sk-..."
export GEMINI_API_KEY="..."
```

### Keybindings (`SPC A`)

- **`SPC A a`** - Ask about (perguntar sobre código selecionado)
- **`SPC A c`** - Chat com AI
- **`SPC A d`** - Define word (definir palavra)
- **`SPC A s`** - Summarize (resumir texto/código)
- **`SPC A r`** - Code review (revisar código)
- **`SPC A i`** - Improve code (melhorar código)
- **`SPC A C`** - Complete code (completar código)
- **`SPC A A`** - Add code (adicionar código)
- **`SPC A t`** - Translate (traduzir texto)
- **`SPC A S`** - Session management (gerenciar sessões)
- **`SPC A p`** - Switch provider (trocar entre GPT-4, GPT-3.5, Gemini)

### Exemplos de Uso

1. **Code Review**: Selecione uma função e pressione `SPC A r` para obter sugestões de melhorias
2. **Explicar Código**: Selecione código complexo e use `SPC A a` para pedir explicação
3. **Refatoração**: Selecione código e use `SPC A i` para sugerir melhorias
4. **Tradução**: Selecione texto e use `SPC A t` para traduzir

**Nota**: Após adicionar o Ellama, execute `doom sync` dentro do container para instalar o pacote
n42 ft_strlen.c           # Verifica com norminette
```

