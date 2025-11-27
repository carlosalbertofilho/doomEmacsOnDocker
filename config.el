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
  ;; Ativa autocompletar automático enquanto digita
  (setq corfu-auto t)                    ; Habilita auto-popup
  (setq corfu-auto-delay 0.2)            ; Delay em segundos (0.2 = 200ms)
  (setq corfu-auto-prefix 2)             ; Mínimo de caracteres para iniciar

  ;; Comportamento do popup
  (setq corfu-cycle t)                   ; Permite navegar circularmente
  (setq corfu-preselect 'prompt)         ; Comportamento de pré-seleção
  (setq corfu-quit-no-match 'separator)  ; Quando sair sem match
  (setq corfu-quit-at-boundary t)        ; Sair ao alcançar limite de palavra

  ;; Visual
  (setq corfu-count 10)                  ; Número de candidatos visíveis
  (setq corfu-max-width 80)              ; Largura máxima do popup
  (setq corfu-min-width 20)              ; Largura mínima do popup

  ;; Ordenação com orderless (já configurado no seu init.el)
  (setq corfu-sort-override-function nil)

  ;; Keybindings customizados
  (map! :map corfu-map
        "C-n" #'corfu-next           ; Ctrl-n: próxima sugestão
        "C-p" #'corfu-previous       ; Ctrl-p: sugestão anterior
        "C-d" #'corfu-show-documentation  ; Ctrl-d: documentação
        "<tab>" #'corfu-complete     ; Tab: confirma seleção
        "TAB" #'corfu-complete
        "RET" nil                    ; Enter: não confirma automaticamente
        "<return>" nil))

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
  ;; Configura o checker básico (para quando não estiver usando LSP)
  (flycheck-norminette-setup)

  ;; Garante que warnings do compilador não escondam erros da norminette
  (setq flycheck-indication-mode 'right-fringe) ; Mostra ícones na direita para não poluir
)


;; =============================================================================
;; EGLOT CONFIGURATION - LSP for C with 42 Style
;; =============================================================================


(after! eglot
  ;; 2. A "Mágica" da Integração
  ;; O Eglot cria dinamicamente um checker chamado 'eglot-check'.
  ;; Nós precisamos dizer ao Flycheck: "Sempre que o eglot terminar, rode a norminette".

  (defun my-chain-norminette-to-eglot ()
    "Encadeia a norminette após o eglot-check em buffers C/C++."
    (when (derived-mode-p 'c-mode 'c-ts-mode 'c++-mode 'c++-ts-mode)
      ;; Adiciona c-norminette como o PRÓXIMO checker após o eglot
      (flycheck-add-next-checker 'eglot-check 'c-norminette 'append)))

  ;; Adiciona esse hook para rodar toda vez que o Eglot iniciar num buffer
  (add-hook 'eglot-managed-mode-hook #'my-chain-norminette-to-eglot))


;; ---------------------------------------------------------------------------
;; ESTILO 42 - Configuração Robusta para Tree-Sitter e CC-Mode
;; ---------------------------------------------------------------------------

(defun my-c-42-style ()
  "Força o estilo da 42 (Tabs reais, largura 4) e desabilita interferências."
  (interactive)

  ;; 1. Configurações Básicas de Buffer
  (setq tab-width 4
        indent-tabs-mode t   ; Usa tabs reais
        fill-column 80)      ; Coluna 80 para a linha vertical

  ;; 2. Desabilitar "Adivinhadores" de estilo do Doom/Emacs
  (when (fboundp 'dtrt-indent-mode) (dtrt-indent-mode -1)) ; Desliga detecção automática
  (setq-local editorconfig-mode nil) ; Opcional: Desliga editorconfig se estiver atrapalhando

  ;; 3. Configuração para c-mode (Legacy/CC-Mode)
  (when (eq major-mode 'c-mode)
    (c-add-style "42-bsd"
                 '("bsd"
                   (c-basic-offset . 4)
                   (c-tab-always-indent . t)))
    (c-set-style "42-bsd"))

  ;; 4. Configuração para c-ts-mode (Tree-Sitter - O que você provavelmente está usando)
  (when (or (eq major-mode 'c-ts-mode)
            (eq major-mode 'c++-ts-mode))
    ;; Estilo base do tree-sitter (bsd é o mais próximo da 42 - chaves na linha de baixo)
    (setq-local c-ts-mode-indent-style 'bsd)
    (setq-local c-ts-mode-indent-offset 4))

  ;; 5. Visualização
  (display-fill-column-indicator-mode 1)
  (whitespace-mode 1))

;; Aplicar o hook com prioridade alta (append) para garantir que rode por último
(add-hook 'c-mode-hook #'my-c-42-style t)
(add-hook 'c-ts-mode-hook #'my-c-42-style t)
(add-hook 'c++-mode-hook #'my-c-42-style t)
(add-hook 'c++-ts-mode-hook #'my-c-42-style t)

;; TAB inteligente: Indenta se no começo da linha, insere TAB se no meio/fim
(defun my-smart-tab-42 ()
  (interactive)
  (if (save-excursion
        (skip-chars-backward " \t")
        (bolp))
      ;; Se estamos no começo da linha (ou só tem espaço antes), indentar
      (if (derived-mode-p 'c-ts-mode 'c++-ts-mode)
          (call-interactively #'indent-for-tab-command) ; Tree-sitter
        (c-indent-line-or-region))                      ; CC-mode
    ;; Se estamos no meio do texto, inserir TAB real
    (insert "\t")))

;; Força o bind do TAB
(map! :after cc-mode
      :map c-mode-base-map
      "<tab>" #'my-smart-tab-42
      "TAB"   #'my-smart-tab-42)

(map! :after c-ts-mode
      :map c-ts-mode-map
      "<tab>" #'my-smart-tab-42
      "TAB"   #'my-smart-tab-42)


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
                          :models '(gemini-2.5-pro
                                    gemini-3-pro-preview
                                    gemini-2.5-flash
                                    gemini-2.0-flash-exp))))

    ;; AQUI definimos que ele é o chefe (Default)
    (setq gptel-backend gemini-backend)
    (setq gptel-model 'gemini-1.5-pro-latest))

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

        (c-42 . "You are an expert C tutor at 42 School.
CRITICAL RULES:
1. STRICTLY follow 42 Norminette rules:
   - Max 25 lines per function.
   - Variable declarations ONLY at the top of the function scope.
   - No `for` loops (use `while`).
   - No `do...while`.
   - No `switch/case`.
   - Macros must be UPPERCASE.
   - Indentation: Use real tabs (4 spaces width).
2. Explain complex concepts simply.
3. If providing code, ensure it compiles with `-Wall -Wextra -Werror`.
4. Suggest splitting functions if logic is too long.")

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

