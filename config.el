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
(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 12 :weight 'normal)
       doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))

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
;; 42 SCHOOL CONFIGURATION
;; =============================================================================

;; Load and enable 42 header
(load! "header42")
(header-42-enable)

;; Load and setup Flycheck Norminette integration
(load! "flycheck-norminette")

(after! flycheck
  ;; Setup norminette checker
  (flycheck-norminette-setup)
  
  ;; Auto-enable norminette checking for C files
  (add-hook 'c-mode-hook #'flycheck-norminette-auto-enable)
  (add-hook 'c-ts-mode-hook #'flycheck-norminette-auto-enable)
  
  ;; Additional keybindings for norminette
  (map! :map c-mode-map
        :localleader
        :desc "Check with norminette" "n" #'flycheck-norminette-check-buffer
        :desc "Toggle norminette" "N" #'flycheck-norminette-toggle)
  
  ;; Customize flycheck display for better readability
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  
  ;; Show error count in modeline
  (setq flycheck-mode-line-prefix "✓"))

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

;; ---------------------------------------------------------------------------
;; Estilo de indentação 42 (inspirado em BSD, ajustado pra Norminette)
;; ---------------------------------------------------------------------------
(c-add-style
 "42"
 '("bsd"
   (c-basic-offset . 4)
   (indent-tabs-mode . t)
   (c-tab-always-indent . t)
   (c-offsets-alist
    (defun-open . 0)
    (defun-close . 0)
    (defun-block-intro . +)
    (brace-list-open . 0)
    (brace-list-close . 0)
    (brace-list-intro . +)
    (block-open . 0)
    (block-close . 0)
    (statement-block-intro . +)
    (substatement-open . 0)
    (substatement . +)
    (case-label . 0)
    (statement-case-intro . +)
    (statement-cont . +))))

;; ---------------------------------------------------------------------------
;; TAB inteligente para o padrão da 42
;; ---------------------------------------------------------------------------
(defun my-c-tab-42 ()
  "Se o cursor estiver no início da linha → indenta.
Se estiver no meio da linha → insere TAB literal."
  (interactive)
  (if (and (bolp) (looking-at "[ \t]*$"))
      (c-indent-line-or-region)
    (insert-tab)))

;; ---------------------------------------------------------------------------
;; Configuração final do estilo 42 para C-mode
;; ---------------------------------------------------------------------------
(defun my-c-42-style ()
  "Configura o estilo C da 42 (Norminette friendly)."
  (setq indent-tabs-mode t        ;; usa tabs reais
        tab-width 4               ;; tab = 4 colunas
        fill-column 80)           ;; limite de 80 colunas

  ;; Para CC Mode (c-mode, c++-mode, etc) podemos usar c-set-style
  (when (eq major-mode 'c-mode)
    (setq c-basic-offset 4)
    (c-set-style "42")
    ;; TAB inteligente só faz sentido aqui, porque usa c-indent-line-or-region
    (local-set-key (kbd "TAB") #'my-c-tab-42))

  ;; Para c-ts-mode, não existe c-set-style; só usamos variáveis básicas
  (when (eq major-mode 'c-ts-mode)
    ;; c-ts-mode ainda respeita c-basic-offset/tab-width em Emacs 29+
    (setq-local c-basic-offset 4)
    ;; Se quiser um TAB “42 friendly” parecido:
    (local-set-key (kbd "TAB") #'insert-tab))

  ;; coluna 80
  (display-fill-column-indicator-mode 1)

  ;; indentação automática
  (electric-indent-mode 1)

  ;; visualização de whitespace (TAB → »)
  (setq whitespace-style '(face tabs tab-mark trailing spaces space-mark))
  (setq whitespace-display-mappings
        '((tab-mark ?\t [?» ?\t] [?\\ ?\t])
          (space-mark ?\  [?·]   [?\.])))
  (whitespace-mode 1))

;; ativa automaticamente no C-mode
(add-hook 'c-mode-hook #'my-c-42-style)
(add-hook 'c-ts-mode-hook #'my-c-42-style)

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

;; =============================================================================
;; EGLOT CONFIGURATION - LSP for C with 42 Style
;; =============================================================================
(after! eglot
  (add-to-list 'eglot-server-programs
               '(c-mode . ("clangd" "--background-index" "--clang-tidy"
                                   "--completion-style=detailed"
                                   "--header-insertion=iwyu"
                                   "--pch-storage=memory")))

  (defun my-c-c++-eglot-setup ()
    "Configuração Eglot para C/C++ compatível com estilo 42."
    ;; Não deixar o LSP mexer em indentação/formatting
    (setq-local eglot-ignored-server-capabilities
                '(:documentFormattingProvider
                  :documentRangeFormattingProvider
                  :documentOnTypeFormattingProvider))

    ;; Ativar features úteis de navegação
    (eldoc-mode 1)             ; doc na minibuffer
    (flymake-mode 1)           ; diagnostics do clangd
    (company-mode -1)          ; evitar conflito se por acaso company estiver
    ;; (você já usa corfu/cape)

    ;; Atalhos locais sob leader para coisas comuns do Eglot
    (map! :localleader
          :map (c-mode-map c-ts-mode-map c++-mode-map c++-ts-mode-map)
          "e" #'eglot          ; (re)conectar servidor
          "r" #'eglot-rename
          "a" #'eglot-code-actions
          "f" #'eglot-format   ; se quiser formatar trecho manualmente
          "h" #'eldoc-doc-buffer
          "i" #'eglot-find-implementation
          "d" #'eglot-find-declaration
          "t" #'eglot-find-typeDefinition))

  ;; Auto complete do eglot primeiro
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;;Garante que o backend de completion do eglot esteja em primeiro
              (setq-local completion-at-point-functions
                          (cons #'eglot-completion-at-point
                                (remove #'eglot-completion-at-point
                                        completion-at-point-functions)))))
  ;; Apply the setup when c-mode starts.
  (add-hook 'c-mode-hook #'my-c-mode-eglot-setup)
  (add-hook 'c-ts-mode-hook #'my-c-mode-eglot-setup)
  (add-hook 'c++-mode-hook #'my-c-mode-eglot-setup)
  (add-hook 'c++-ts-mode-hook #'my-c-mode-eglot-setup)
)

;; =============================================================================
;; AI ASSISTANTS - ELLAMA
;; =============================================================================

(use-package! ellama
  :defer t
  :init
  ;; General settings
  (setq ellama-language "English")
  (setq ellama-sessions-directory (expand-file-name "ellama-sessions" doom-cache-dir))
  (setq warning-suppress-types '(llm))
  
  :config
  ;; Load LLM backends after ellama is loaded
  (require 'llm-openai nil t)
  
  ;; Default provider: OpenAI GPT-4
  ;; API key should be set via environment variable OPENAI_API_KEY
  (when (and (featurep 'llm-openai)
             (getenv "OPENAI_API_KEY"))
    (setq ellama-provider
          (make-llm-openai
           :key (getenv "OPENAI_API_KEY")
           :chat-model "gpt-5.1")))
  
  ;; Alternative providers configuration
  ;; Uncomment and configure the one you want to use:
  
  ;; Google Gemini
  ;; (when (getenv "GEMINI_API_KEY")
  ;;   (require 'llm-gemini)
  ;;   (setq ellama-provider
  ;;         (make-llm-gemini
  ;;          :key (getenv "GEMINI_API_KEY")
  ;;          :chat-model "gemini-pro")))
  
  ;; OpenAI GPT-3.5 (faster, cheaper alternative)
  ;; (when (getenv "OPENAI_API_KEY")
  ;;   (setq ellama-provider
  ;;         (make-llm-openai
  ;;          :key (getenv "OPENAI_API_KEY")
  ;;          :chat-model "gpt-3.5-turbo")))
  
  ;; Keybindings - Using SPC A (uppercase) to avoid conflicts with embark
  (map! :leader
        ;; Prefix A - AI Assistant (ações gerais)
        (:prefix ("A" . "AI Assistant")
         :desc "Ask about selection" "a" #'ellama-ask-about
         :desc "Chat"                "c" #'ellama-chat
         :desc "Define word"         "d" #'ellama-define-word
         :desc "Summarize"           "s" #'ellama-summarize
         :desc "Code review"         "r" #'ellama-code-review
         :desc "Improve code"        "i" #'ellama-code-improve
         :desc "Complete code"       "C" #'ellama-code-complete
         :desc "Add code"            "A" #'ellama-code-add
         :desc "Translate"           "t" #'ellama-translate
         :desc "Session"             "S" #'ellama-session
         :desc "Switch provider"     "p" #'my/ellama-switch-provider

         ;; Submenu X: contexto → SPC A X ...
         (:prefix ("x" . "AI Context")
          :desc "Add region to context"    "a" #'ellama-context-add-region
          :desc "Add buffer to context"    "b" #'ellama-context-add-buffer
          :desc "Add file to context"      "f" #'ellama-context-add-file
          :desc "Remove item from context" "r" #'ellama-context-remove
          :desc "Clear all context"        "c" #'ellama-context-clear
          :desc "Show current context"     "s" #'ellama-context-show)))
  )

;; Helper function to switch between providers
(defun my/ellama-switch-provider (provider)
  "Switch Ellama provider interactively."
  (interactive
   (list (intern (completing-read "Select provider: "
                                   '("openai-gpt4" "openai-gpt3.5" "gemini")
                                   nil t))))
  (require 'llm-openai)
  (pcase provider
    ('openai-gpt4
     (if (getenv "OPENAI_API_KEY")
         (progn
           (setq ellama-provider
                 (make-llm-openai
                  :key (getenv "OPENAI_API_KEY")
                  :chat-model "gpt-4"))
           (message "Switched to OpenAI GPT-4"))
       (user-error "OPENAI_API_KEY environment variable not set")))
    ('openai-gpt3.5
     (if (getenv "OPENAI_API_KEY")
         (progn
           (setq ellama-provider
                 (make-llm-openai
                  :key (getenv "OPENAI_API_KEY")
                  :chat-model "gpt-4o"))
           (message "Switched to OpenAI GPT-3.5 Turbo"))
       (user-error "OPENAI_API_KEY environment variable not set")))
    ('gemini
     (if (getenv "GEMINI_API_KEY")
         (progn
           (require 'llm-gemini)
           (setq ellama-provider
                 (make-llm-gemini
                  :key (getenv "GEMINI_API_KEY")
                  :chat-model "gemini-pro"))
           (message "Switched to Google Gemini Pro"))
       (user-error "GEMINI_API_KEY environment variable not set")))
    (_ (user-error "Unknown provider"))))


