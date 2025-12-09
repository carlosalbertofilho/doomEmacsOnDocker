;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; Nerd Fonts options installed in the container:

;; Fira Code (default - excellent ligatures support)
;;(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))

;; JetBrains Mono (very popular, excellent readability)
;; (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 12 :weight 'normal)
;;       doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))

;; Iosevka (narrow, great for maximizing horizontal space)
(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 14 :weight 'normal)
       doom-variable-pitch-font (font-spec :family "Fira Sans" :size 15))

;; Source Code Pro (Adobe's classic, highly readable)
;; (setq doom-font (font-spec :family "SauceCodePro Nerd Font" :size 12 :weight 'normal)
;;       doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))

;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-Iosvkem)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; This enable the mouse mode
(setq xterm-mouse-mode t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; suprime os warnings do ellama
(setq warning-suppress-types '(llm))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; =============================================================================
;; CORFU MODE - AUTOCOMPLETION
;; =============================================================================

(after! corfu
  ;; 1. COMPORTAMENTO GERAL
  (setq corfu-auto t)                    ; Habilita auto-popup
  (setq corfu-auto-delay 0.1)            ; Mais rápido (0.1s). Se ficar pesado no Docker, volte para 0.2
  (setq corfu-auto-prefix 2)             ; Mínimo de 2 caracteres para ativar

  ;; 2. SELEÇÃO E NAVEGAÇÃO
  (setq corfu-cycle t)                   ; Permite navegar do último para o primeiro
  ;(setq corfu-preselect 'first)          ; [MELHORIA] Seleciona o primeiro candidato automaticamente (mais rápido que 'prompt)
  ;(setq corfu-quit-no-match 'separator)  ; Comportamento ao sair
  ;(setq corfu-quit-at-boundary t)        ; Sair ao encontrar separador

  ;; 3. HISTÓRICO (Aprendizado)
  ;; O Corfu lembrará suas escolhas frequentes e as colocará no topo
  (corfu-history-mode 1)
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history)

  ;; 4. DOCUMENTAÇÃO FLUTUANTE (Semelhante ao VS Code)
  ;; Mostra documentação/assinatura da função ao lado do popup
  (corfu-popupinfo-mode 1)
  (setq corfu-popupinfo-delay '(0.5 . 0.2)) ; Delay para aparecer (0.5s) e atualizar (0.2s)
  (setq corfu-popupinfo-max-width 70)
  (setq corfu-popupinfo-max-height 20)

  ;; 5. VISUAL
  (setq corfu-count 14)                  ; Mostra mais candidatos (padrão é 10)
  (setq corfu-max-width 100)             ; Permite largura maior (útil para C++ templates)
  (setq corfu-min-width 20)

  ;; 6. KEYBINDINGS
  (map! :map corfu-map
        "C-n"       #'corfu-next
        "C-p"       #'corfu-previous
        "C-j"       #'corfu-next                ; Alternativa VIM-like para descer
        "C-k"       #'corfu-previous            ; Alternativa VIM-like para subir
        "<tab>"     #'corfu-complete
        "TAB"       #'corfu-complete
        "S-TAB"     #'corfu-previous            ; Shift-Tab volta na lista
        "<backtab>" #'corfu-previous
        "RET"       nil                         ; Enter cria nova linha (Padrão do Doom, evita completar sem querer)
        "<return>"  nil
        ;; Controle da janela de documentação
        "M-d"       #'corfu-popupinfo-toggle    ; Liga/Desliga doc manualmente
        "M-p"       #'corfu-popupinfo-scroll-down ; Rola a doc para cima
        "M-n"       #'corfu-popupinfo-scroll-up))

;; =============================================================================
;; CAPE - Completion At Point Extensions (para Corfu)
;; =============================================================================

(after! cape
  ;; Adiciona backends de completion
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)  ; Palavras do buffer
  (add-to-list 'completion-at-point-functions #'cape-file)     ; Arquivos
  (add-to-list 'completion-at-point-functions #'cape-keyword)) ; Keywords

;; =============================================================================
;; 42 SCHOOL CONFIGURATION
;; =============================================================================

;; Load and enable 42 header
(load! "header42")
(header-42-enable)

;; Load and setup Flycheck Norminette integration
(load! "flycheck-norminette")

(after! flycheck
  ;; 1. Garante que o checker da norminette está pronto para ser usado
  (flycheck-norminette-setup)

  ;; 2. Função para adicionar a norminette aos checkers de C/C++
  (defun my-add-norminette-checker ()
    (add-to-list 'flycheck-checkers 'c-norminette))

  ;; 3. Hooks para ativar a função nos modos corretos
  ;;    Quando um arquivo C/C++ for aberto, a norminette será adicionada
  ;;    à lista de verificadores que o Flycheck usará.
  (add-hook 'c-mode-hook #'my-add-norminette-checker)
  (add-hook 'c-ts-mode-hook #'my-add-norminette-checker)
  (add-hook 'c++-mode-hook #'my-add-norminette-checker)
  (add-hook 'c++-ts-mode-hook #'my-add-norminette-checker)

  ;; Configuração visual opcional
  (setq flycheck-indication-mode 'right-fringe)
)

;; ---------------------------------------------------------------------------
;; 1. Função Central de Estilo 42 (Compatível com cc-mode e Tree-Sitter)
;; ---------------------------------------------------------------------------
(defun my-c-42-style ()
  "Força o estilo da 42 (Tabs reais, largura 4) para C e C++.
Esta função é segura para ser chamada em qualquer modo C/C++."
  (interactive)

  ;; Configurações básicas de buffer (comuns a ambos os sistemas)
  (setq-local indent-tabs-mode t        ; Usa TABS reais, não espaços.
              tab-width 4               ; Largura visual do TAB.
              c-basic-offset 4          ; Offset para cc-mode (usado como fallback).
              fill-column 80)           ; Linha vertical na coluna 80.

  ;; Desabilita "adivinhadores" de estilo que podem interferir
  (when (fboundp 'dtrt-indent-mode) (dtrt-indent-mode -1))
  (setq-local editorconfig-mode nil)

  ;; Lógica de indentação específica para cada sistema
  (if (derived-mode-p 'c-ts-mode 'c++-ts-mode)
      ;; --- Para TREE-SITTER ---
      (progn
        (setq-local c-ts-mode-indent-style 'bsd)
        (setq-local c-ts-mode-indent-offset 4))
    ;; --- Para CC-MODE (clássico) ---
    (when (derived-mode-p 'c-mode 'c++-mode)
      (c-add-style "42-bsd"
                   '("bsd" (c-basic-offset . 4) (c-tab-always-indent . t)))
      (c-set-style "42-bsd")))

  ;; Reforço visual e limpeza automática
  (display-fill-column-indicator-mode 1)
  (setq-local whitespace-style '(face trailing tabs))
  (whitespace-mode 1)
  (add-hook 'before-save-hook #'whitespace-cleanup nil t))

;; ---------------------------------------------------------------------------
;; 2. Hooks para aplicar o estilo automaticamente
;; ---------------------------------------------------------------------------
;; Aplica o estilo assim que um arquivo C/C++ é aberto
(add-hook 'c-mode-hook #'my-c-42-style t)
(add-hook 'c-ts-mode-hook #'my-c-42-style t)
(add-hook 'c++-mode-hook #'my-c-42-style t)
(add-hook 'c++-ts-mode-hook #'my-c-42-style t)


;; ---------------------------------------------------------------------------
;; 3. Configuração do EGLOT para coexistir com o estilo 42
;; ---------------------------------------------------------------------------
(after! eglot
  ;; PASSO CRÍTICO: Impede que o servidor LSP (clangd) formate o código.
  ;; Isso garante que ele não substitua nossos TABS por espaços.
  (add-to-list 'eglot-ignored-server-capabilities :documentFormattingProvider)
  (add-to-list 'eglot-ignored-server-capabilities :documentRangeFormattingProvider)
  (add-to-list 'eglot-ignored-server-capabilities :documentOnTypeFormattingProvider)


  ;; REFORÇO: Re-aplica nosso estilo sempre que Eglot iniciar.
  ;; Isso garante que nosso estilo tem a palavra final, mesmo que
  ;; outro pacote tente alterá-lo durante a inicialização.
  (add-hook 'eglot-managed-mode-hook #'my-c-42-style))


;; ---------------------------------------------------------------------------
;; MAPEAMENTO DE TECLAS PARA INDENTAÇÃO
;; ---------------------------------------------------------------------------

(defun my-indent-line-or-region ()
  "Indenta a região se ativa, senão a linha atual."
  (interactive)
  (if (region-active-p)
      (indent-region (region-beginning) (region-end))
    ;; `indent-for-tab-command` é um comando inteligente que funciona bem
    ;; tanto para cc-mode quanto para tree-sitter.
    (indent-for-tab-command)))

;; Força TAB a ser literal e C-<tab> a indentar
(map! :after (cc-mode c-ts-mode)
      :map (c-mode-base-map c-ts-mode-map c++-ts-mode-map)
      ;; TAB insere um caractere de tabulação literal
      "<tab>" #'self-insert-command
      "TAB"   #'self-insert-command
      ;; M-i (Alt+i) faz a indentação (muito mais confiável que C-<tab>)
      "M-i" #'my-indent-line-or-region)


;; =============================================================================
;; GPTEL Configuration
;; =============================================================================
(after! gptel
  ;; ---------------------------------------------------------------------------
  ;; 1. CONFIGURAÇÃO GERAL
  ;; ---------------------------------------------------------------------------

  ;; Define Org-mode como o formato padrão para os buffers de chat
  (setq gptel-default-mode 'org-mode)

  ;; ---------------------------------------------------------------------------
  ;; 2. BACKENDS (Modelos)
  ;; ---------------------------------------------------------------------------

  ;; --- OPÇÃO 1: GEMINI (O PADRÃO) ---
  ;; Definimos ele numa variável e já setamos como o backend ativo
  (let ((gemini-backend (gptel-make-gemini "Gemini"
                          :key (getenv "GEMINI_API_KEY")
                          :stream t
                          :models '(gemini-3-pro-preview
                                    gemini-2.5-pro
                                    gemini-2.5-flash))))

    ;; AQUI definimos que ele é o chefe (Default)
    (setq gptel-backend gemini-backend)
    (setq gptel-model 'gemini-2.5-pro))

  ;; --- OPÇÃO 2: GITHUB COPILOT ---
  ;; O gptel agora suporta Copilot nativamente.
  ;; Ele tenta ler o token automaticamente do arquivo de config do Copilot.
  (gptel-make-gh-copilot "Copilot"
    :stream t
    :models '(gpt-4o claude-3.5-sonnet))

  ;; --- OPÇÃO 3: CHATGPT (OPENAI) ---
  (gptel-make-openai "ChatGPT"
    :key (getenv "OPENAI_API_KEY")
    :stream t
    :models '(gpt-4o gpt-4-turbo gpt-3.5-turbo))

  ;; ---------------------------------------------------------------------------
  ;; 3. PERSONAS / DIRETRIZES (Contexto 42, Python, etc)
  ;; ---------------------------------------------------------------------------

  (setq gptel-directives
      '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")

        (programmer . "You are an expert programmer. Provide code snippets and explanations. Focus on clean, efficient, and modern code.")

        (c-42 . "You are an expert C tutor at 42 School conforming strictly to Norm v4.1.
CRITICAL RULES (Violating these fails the project):
1. FORBIDDEN SYNTAX:
   - No `for`, `do...while`, `switch`, `case`, `goto`.
   - No ternary operators (`condition ? a : b`).
   - No Variable Length Arrays (VLAs).
   - No interleaving declarations and code.
2. FORMATTING & LIMITS:
   - Indentation: REAL TABS (width 4).
   - Max 25 lines per function (excluding braces).
   - Max 80 columns width.
   - Max 4 parameters per function.
   - Max 5 local variables per function.
3. STRUCTURE:
   - Declarations MUST be at the top of the function.
   - Separate declarations from code with exactly one empty line.
   - NEVER initialize variables in the declaration line (e.g., `int i = 0;` is ILLEGAL; split into `int i;` and `i = 0;`).
4. NAMING CONVENTIONS (snake_case):
   - Structs: `s_name` | Typedefs: `t_name` | Unions: `u_name` | Globals: `g_name`.
5. OUTPUT:
   - Code must compile with `-Wall -Wextra -Werror`.
   - If logic is complex, suggest helper functions to satisfy the 25-line limit.")

          (cpp-42 . "You are an expert C++ mentor at 42 School.
CRITICAL RULES:
1. Adhere to C++98 standard (unless asked otherwise).
2. ALWAYS implement the 'Orthodox Canonical Form' for classes (Constructor, Destructor, Copy Constructor, Assignment Operator).
3. No external libraries (like Boost) unless specified.
4. Use standard STL containers.
5. Explain memory management (RAII) clearly.")

          (python . "You are a senior Python Developer.
RULES:
1. Follow PEP8 strict guidelines.
2. Use Type Hinting (typing module) for all function arguments and returns.
3. Write concise, pythonic code (list comprehensions where appropriate).
4. Include docstrings for classes and complex functions.
5. Prefer modern Python 3.10+ syntax.")))
  ;; ---------------------------------------------------------------------------
  ;; 4. KEYBINDINGS PERSONALIZADOS
  ;; ---------------------------------------------------------------------------

  ;; Desvincula o atalho original "SPC o l a" para que possamos usá-lo como um prefixo.
  ;; O Doom por padrão mapeia para `llm-add-context`.
  (map! :leader "o l a" nil)

  ;; Cria o novo "sub-menu" de atalhos para adicionar contexto
  (map! :leader
        :prefix ("o l a" . "+Add context")
        :desc "Add Text (region)" "t" #'gptel-context-add-region
        :desc "Add Buffer"        "b" #'gptel-context-add
        :desc "Add File"          "f" #'gptel-context-add-file)

  ;; Função para trocar o prompt automaticamente baseada no modo
  (defun my-gptel-setup-directive ()
    (let ((directive (pcase major-mode
                     ('c-mode 'c-42)           ; Se for C, usa a persona 42
                     ('c-ts-mode 'c-42)        ; Tree-sitter C
                     ('c++-mode 'cpp-42)       ; C++
                     ('c++-ts-mode 'cpp-42)
                     ('python-mode 'python)    ; Python
                     ('python-ts-mode 'python)
                     (_ 'default))))           ; Caso contrário, default
    (setq-local gptel-system-message (alist-get directive gptel-directives))))

  ;; Adiciona o hook quando o gptel for ativado num buffer
  (add-hook 'gptel-mode-hook #'my-gptel-setup-directive)

)


;; =============================================================================
;; ESHELL CONFIGURATION
;; =============================================================================

(after! eshell
  ;; -------------------------
  ;; ALIASES for 42 projects (C)
  ;; -------------------------

  ;; Compile with 42 standard flags
  (add-hook 'eshell-mode-hook
    (lambda ()
    (eshell/alias "cc42" "cc -Wall -Wextra -Werror $* -o a.out")

    ;; Run program
    (eshell/alias "r42" "./a.out")

    ;; Clean binary
    (eshell/alias "clean42" "rm -f a.out")

    ;; Compile and run directly
    (eshell/alias "cr42" "cc42 $* && ./a.out")

    ;; Compile with valgrind
    (eshell/alias "val42" "cc42 $* && valgrind --leak-check=full ./a.out")

    ;; Norminette check (installed via pipx)
    (when (executable-find "norminette")
    (eshell/alias "n42" "norminette $*"))
    )
  )
)

