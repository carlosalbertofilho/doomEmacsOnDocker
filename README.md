## **Doom Emacs Docker Container**

Este projeto oferece uma maneira de usar o **Doom Emacs** em um ambiente isolado com o **Docker**, garantindo que todas as dependências estejam configuradas corretamente sem poluir seu sistema local.

-----

### **Pré-requisitos**

  * **Docker** instalado na sua máquina.

-----

### **Como Usar**

Siga estes passos para construir e executar o contêiner.

#### 1\. Construa a Imagem

O Dockerfile irá criar um novo usuário dentro do contêiner com o mesmo nome e ID do seu usuário local. Isso garante que não haja problemas de permissão ao interagir com seus arquivos.

Primeiro, obtenha o nome e o ID do seu usuário local. Você pode encontrá-los facilmente no terminal:

```bash
echo $(whoami)  # Nome do usuário
echo $(id -u)     # ID do usuário
```

Agora, construa a imagem passando essas informações como argumentos. No diretório onde você salvou o `Dockerfile`, execute:

```bash
docker build --build-arg UNAME=$(whoami) --build-arg UID=$(id -u) -t doom-emacs-local .
```

Este comando cria uma imagem chamada `doom-emacs-local`.

-----

#### 2\. Execute o Contêiner

Para executar o Doom Emacs dentro do contêiner, você precisa montar seus diretórios de trabalho e a sua configuração do Doom. Isso permite que você edite seus arquivos e mantenha as configurações persistentes.

Use o seguinte comando para iniciar o contêiner e abrir o Emacs:

```bash
docker run -it --rm \
  -v ~/workspace:/home/$(whoami)/workspace \
  -v ~/Documents:/home/$(whoami)/Documents \
  -v ~/.config/doom:/home/$(whoami)/.config/doom \
  -v ~/.doom.d:/home/$(whoami)/.doom.d \
  doom-emacs-local
```

**Explicação dos Parâmetros:**

  * `docker run -it --rm`: Inicia o contêiner de forma interativa (`-it`) e o remove automaticamente ao sair (`--rm`).
  * `-v ~/workspace:/home/$(whoami)/workspace`: Monta seu diretório local `~/workspace` no diretório correspondente dentro do contêiner. Repita isso para qualquer outro diretório que você precise acessar.
  * `-v ~/.config/doom:/home/$(whoami)/.config/doom` e `-v ~/.doom.d:/home/$(whoami)/.doom.d`: **Esses são os volumes mais importantes.** Eles mantêm sua configuração do Doom Emacs (`init.el`, `config.el`, `packages.el`) e a pasta de dados (`.doom.d`) sincronizadas com o seu sistema local. Isso significa que qualquer alteração de configuração feita no contêiner será salva no seu sistema host.

-----

#### 3\. Como Acessar os Arquivos

Uma vez que o Emacs é iniciado, você pode navegar até os diretórios montados, como `/home/seu-usuario/workspace`, para abrir seus projetos e arquivos.

Você pode usar o atalho `SPC SPC` para abrir o `find-file` e navegar facilmente.

-----

#### 4\. Atualizar a Imagem

Se você precisar de uma versão mais recente do Emacs ou de suas dependências, pode reconstruir a imagem com o mesmo comando:

```bash
docker build --build-arg UNAME=$(whoami) --build-arg UID=$(id -u) -t doom-emacs-local .
```
