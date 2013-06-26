;;;;;;;;;;;;;;;;;;;;;;;;; mode defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; etags

(setq tags-revert-without-query t)

;; clear tags when the file is reverted
(defun tags-revert-hook ()
  (initialize-new-tags-table)
  (if (functionp 'ffip-clear-project-cache)
      (ffip-clear-project-cache)))

;; add tags-revert-hook after tags-table-mode is called
(defadvice tags-table-mode
  (after tabs-table-mode-revert-hook activate)
  (add-hook 'after-revert-hook 'tags-revert-hook nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile

(eval-when-compile (require 'compile))

(defun my-compile-setup ()
  (setq compilation-scroll-output t
        compilation-window-height 20))
(add-hook 'compilation-mode-hook 'my-compile-setup)

;; this little helper highlights gcc compilation lines
(defvar compilation-added-font-lock-keywords nil)
(setq compilation-added-font-lock-keywords
      (list
       '("^\\(g?cc\\|[gc][+][+]\\) .* \\([^ ]+\.\\(c\\|cc\\|cpp\\)\\)$"
         (1 font-lock-keyword-face) (2 font-lock-constant-face))))
(font-lock-add-keywords 'compilation-mode compilation-added-font-lock-keywords)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; filladapt mode

(require 'filladapt)
(setq filladapt-mode-line-string nil)
(setq-default filladapt-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c/cc/java mode

;; shared

(eval-when-compile (require 'cc-mode))

(setq minor-mode-alist (assq-delete-all 'abbrev-mode minor-mode-alist))

;; guess-offset for all c++ files
(eval-after-load "cc-mode"
  '(require 'guess-offset))

(defun cc-debug ()
  (interactive)
  (setq c-echo-syntactic-information-p (not c-echo-syntactic-information-p)))

;; some added keywords
(when window-system
  (defvar plain-face (make-face 'plain-face))
  (defconst comment-fixer (list (cons "^\\([ \t]+\\)" '(1 plain-face t))))
  (font-lock-add-keywords 'c++-mode comment-fixer)
  (font-lock-add-keywords 'java-mode comment-fixer))

(defun my-cc-common-setup ()
  (setq c-auto-newline nil
        c-basic-offset 4
        c-electric-pound-behavior '(alignleft)
        fill-column 80
        indent-tabs-mode nil
        )
  (c-setup-filladapt)
  (define-key c-mode-base-map "\C-m" `c-context-line-break)
  (define-key c-mode-base-map "\C-c\C-u" 'uncomment-region))
(add-hook 'c-mode-common-hook 'my-cc-common-setup)

;; java

(defun my-java-setup ()
  (abbrev-mode 1)
  (c-add-style
   "amd-java"
   '((c-offsets-alist . ((arglist-intro . +)
                         (access-label . 0)
                         (case-label . *)
                         (statement-case-intro . *)
                         (substatement-open . 0)
                         (inline-open . 0)
                         (block-open - 0)
                         )))
   t)
  (setq tab-width 4)
  (when window-system
    (setq font-lock-keywords java-font-lock-keywords-3)))
(add-hook 'java-mode-hook 'my-java-setup)

;; cc

(defun my-cc-setup ()
  (c-add-style
   "amd"
   '((c-comment-only-line-offset . (0 . 0))
     (c-offsets-alist . ((statement-block-intro . +)
                         (knr-argdecl-intro . 5)
                         (substatement-open . 0)
                         (label . 0)
                         (statement-case-open . 0)
                         (statement-cont . +)
                         (arglist-close . c-lineup-arglist)
                         (arglist-intro . +)
                         (access-label . -)
                         (case-label . +)
                         (statement-case-intro . +)
                         (inline-open . 0)
                         )))
   t)
  )
(add-hook 'c++-mode-hook 'my-cc-setup)

(add-to-list 'auto-mode-alist '("\\.\\(c\\|h\\|cpp\\)$" . c++-mode))

;; bah - remove c-mode entirely! We have to do it this way because it's
;; in autoloads
(defun adjust-autoloads ()
  (delete '("\\.[ch]\\'" . c-mode) auto-mode-alist))
(add-hook 'after-init-hook 'adjust-autoloads)

;; csharp

(defun my-csharp-mode ()
  (setq indent-tabs-mode nil)
  (local-set-key (kbd "{") 'c-electric-brace))
(add-hook 'csharp-mode-hook 'my-csharp-mode)
(setq csharp-want-flymake-fixup nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; objc

; gotta remove this rule since image mode tries to steal .m
(setq auto-mode-alist (remove (rassoc 'image-mode auto-mode-alist) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text

(defun my-text-setup ()
  (turn-on-auto-fill)
  (if (eq indent-line-function 'indent-to-left-margin)
      (setq indent-line-function 'indent-relative-maybe)))
(add-hook 'text-mode-hook 'my-text-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perl

(eval-when-compile (require 'perl-mode))
(defun my-perl-setup ()
  (setq tab-width 4))
(add-hook 'perl-mode-hook 'my-perl-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cperl

(eval-when-compile (require 'cperl-mode))
(autoload 'perl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
(defun my-cperl-setup ()
  (setq cperl-indent-level 4)
  (setq cperl-invalid-face nil)
  (setq cperl-continued-brace-offset -2)
  (setq tab-width 4)
  (set-face-background 'cperl-hash-face (face-background 'font-lock-keyword-face))
  )

(add-hook 'cperl-mode-hook 'my-cperl-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python

(eval-when-compile (require 'python))
(defun my-python-setup ()
  (setq tab-width 4)
  (define-key python-mode-map [backspace] 'py-electric-backspace))
 (add-hook 'python-mode-hook 'my-python-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; makefile

(eval-when-compile (require 'make-mode))
(defun my-makefile-setup ()
  (define-key makefile-mode-map "\M-m" 'compile-make)
  (define-key makefile-mode-map "\M-p" 'compile-re-make))
(add-hook 'makefile-mode-hook 'my-makefile-setup)
(add-to-list 'auto-mode-alist '("\\.mak$" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\(defs\\|rules\\)$" . makefile-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html and friends

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; css

(eval-when-compile (require 'css-mode))
(eval-when-compile (require 'rainbow-mode))
(defun my-css-setup ()
  (require 'rainbow-mode)
  (setq css-indent-offset 2)
  (rainbow-turn-on)
  )
(add-hook 'css-mode-hook 'my-css-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scss

(defconst scss-font-lock-keywords
  ;; Variables
  '(("$[a-z_-][a-z-_0-9]*" . font-lock-constant-face)))

(define-derived-mode scss-mode css-mode "SCSS"
  (font-lock-add-keywords nil scss-font-lock-keywords)
  (font-lock-add-keywords nil (list (cons "&" 'font-lock-string-face)))
  ;; Add the single-line comment syntax ('//', ends with newline)
  ;; as comment style 'b' (see "Syntax Flags" in elisp manual)
  (modify-syntax-entry ?/ ". 124b" css-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" css-mode-syntax-table))

(add-to-list 'auto-mode-alist '("\\.scss" . scss-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; js

(eval-when-compile (load "js2-mode"))
(defun my-js2-setup ()
  (make-local-variable 'backward-delete-char-untabify-method)
  (setq
   backward-delete-char-untabify-method 'untabify
   js2-mode-escape-quotes nil
   js2-basic-offset 2
   tab-width 2)
  ;; use this to turn OFF the standard js stuff
  ;; indent-line-function 'indent-relative-maybe
  ;; (define-key js2-mode-map "\C-m" 'newline-and-indent)
  ;; (make-local-variable 'standard-indent)
  ;; (setq standard-indent 2)
  (define-key js2-mode-map [C-left] 'my-decrease)
  (define-key js2-mode-map [C-right] 'my-increase))
(add-hook 'js2-mode-hook 'my-js2-setup)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; coffee
(eval-when-compile (require 'coffee-mode))
(defun my-coffee-setup ()
  (make-local-variable 'standard-indent)
  (make-local-variable 'backward-delete-char-untabify-method)
  (setq
   backward-delete-char-untabify-method 'untabify
   coffee-tab-width 2
   standard-indent 2
   tab-width 2)
  (define-key coffee-mode-map [C-left] 'my-decrease)
  (define-key coffee-mode-map [C-right] 'my-increase)
  )
(add-hook 'coffee-mode-hook 'my-coffee-setup)
; need this one for coffee.erb
(add-to-list 'auto-mode-alist '("\\.coffee" . coffee-mode))
(add-to-list 'interpreter-mode-alist '("coffee" . coffee-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sql

(eval-when-compile (require 'sql))

;; default user/pw/db
(setq sql-user "xxxx"
      sql-password "yyy"
      sql-database "zzz"
      )
(eval-after-load "sql"
  '(progn
     (when (not (fboundp 'old-sql-get-login))
       (fset 'old-sql-get-login (symbol-function 'sql-get-login)))
     (defun sql-get-login (&rest what)
       "Overridden to add \"as sysdba\" when sql-user is sys"
       (apply 'old-sql-get-login what)
       (setq sql-oracle-options
             (if (string= sql-user "sys")
                 (list "as" "sysdba")
               nil)))))

(defvar sql-added-font-lock-keywords nil)
(when window-system
  (let* ((keywords
          (eval-when-compile
            (regexp-opt
             '("access" "add" "all" "alter" "and" "any" "as" "asc" "audit"
               "between" "by" "char" "check" "cluster" "column" "comment"
               "compress" "connect" "create" "current" "date" "decimal"
               "default" "delete" "desc" "distinct" "drop" "else" "exclusive"
               "exists" "file" "float" "for" "from" "grant" "group" "having"
               "identified" "immediate" "in" "increment" "index" "initial"
               "insert" "integer" "intersect" "into" "is" "level" "like"
               "lock" "long" "maxextents" "minus" "mlslabel" "mode" "modify"
               "noaudit" "nocompress" "not" "nowait" "null" "number" "of"
               "offline" "on" "online" "option" "or" "order" "pctfree" "prior"
               "privileges" "public" "raw" "rename" "resource" "revoke" "row"
               "rowid" "rownum" "rows" "select" "session" "set" "share" "size"
               "smallint" "start" "successful" "synonym" "sysdate" "table"
               "then" "to" "trigger" "uid" "union" "unique" "update" "user"
               "validate" "values" "varchar" "varchar2" "view" "whenever"
               "where" "with" "bfile" "binary_double" "binary_float" "blob"
               "byte" "char" "clob" "date" "long" "long" "nchar" "nclob"
               "number" "nvarchar2" "raw" "rowid" "timestamp" "urowid"
               "varchar2" "join" "inner" "outer" "left" "right"
               "use" "source" "if"
               ) t)))
         (functions
          (eval-when-compile
            (regexp-opt
             '("case" "else" "then" "when" "avg" "corr" "covar_pop"
               "covar_samp" "count" "cume_dist" "dense_rank" "first"
               "first_value" "lag" "last" "last_value" "lead" "max" "min"
               "ntile" "percent_rank" "percentile_cont" "percentile_disc"
               "rank" "ratio_to_report" "row_number" "stddev" "stddev_pop"
               "stddev_samp" "sum" "var_pop" "var_samp" "variance" "abs"
               "acos" "add_months" "ascii" "asciistr" "asin" "atan" "atan2"
               "bfilename" "bin_to_num" "bitand" "cardinality" "cast" "ceil"
               "chartorowid" "chr" "coalesce" "collect" "compose" "concat"
               "convert" "cos" "cosh" "current_date" "current_timestamp"
               "cv" "dbtimezone" "decode" "decompose" "depth" "deref" "dump"
               "empty_blob, empty_clob" "existsnode" "exp" "extract" "extract"
               "extractvalue" "floor" "from_tz" "greatest" "hextoraw" "initcap"
               "instr" "iteration_number" "last_day" "least" "length" "ln"
               "lnnvl" "localtimestamp" "log" "lower" "lpad" "ltrim" "make_ref"
               "mod" "months_between" "nanvl" "new_time" "next_day" "nlssort"
               "nls_charset_decl_len" "nls_charset_id" "nls_charset_name"
               "nls_initcap" "nls_lower" "nls_upper" "nullif" "numtoyminterval"
               "nvl" "nvl2" "ora_hash" "path" "power" "powermultiset"
               "powermultiset_by_cardinality" "presentnnv" "presentv" "previous"
               "rawtohex" "rawtonhex" "ref" "reftohex" "regexp_instr"
               "regexp_replace" "regexp_substr" "remainder" "replace"
               "round" "round" "rowidtochar" "rowidtonchar" "rpad" "rtrim"
               "scn_to_timestamp" "sessiontimezone" "set" "sign" "sin" "sinh"
               "soundex" "sqrt" "substr" "sysdate" "systimestamp"
               "sys_connect_by_path" "sys_context" "sys_dburigen"
               "sys_extract_utc" "sys_extract_utc" "sys_guid" "sys_typeid"
               "sys_xmlagg" "sys_xmlgen" "tan" "tanh" "timestamp_to_scn"
               "to_binary_double" "to_binary_float" "to_char" "to_clob"
               "to_date" "to_dsinterval" "to_lob" "to_multi_byte" "to_nchar"
               "to_nclob" "to_number" "to_single_byte" "to_timestamp"
               "to_timestamp_tz" "to_yminterval" "translate" "translate"
               "treat" "trim" "trunc" "trunc" "tz_offset" "uid" "unistr"
               "updatexml" "upper" "user" "userenv" "value" "vsize"
               "width_bucket" "xmlagg" "xmlcolattval" "xmlconcat"
               "xmlforest" "xmlsequence" "xmltransform") t)))
         )
    (setq sql-added-font-lock-keywords
          (list
           (cons "SQL>" 'font-lock-comment-face)
           (cons (concat "\\<" keywords "\\>")  'font-lock-constant-face)
           (cons (concat "\\<" functions "\\>") 'font-lock-constant-face)
           ;; this is for TemplateToolkit, not SQL... a bit of a hack
           (cons "\\[%.*%\\]" 'border))))
  (font-lock-add-keywords 'sql-mode sql-added-font-lock-keywords)
  (font-lock-add-keywords 'sql-interactive-mode sql-added-font-lock-keywords)
  )

(defun my-sql-setup ()
  (sql-set-sqli-buffer-generally))
(defun my-sql-send-file (path)
  (if (file-readable-p path)
      (progn
        (message "Sending %s..." path)
        (goto-char (point-max))
        (insert-file-contents path)
        (setq path (buffer-substring (point) (point-max)))
        (delete-region (point) (point-max))
        (comint-send-string sql-buffer path))
    (message "%s not found." path)))

(defun my-sql-interactive-setup ()
  (sql-set-sqli-buffer-generally)
  (setq comint-scroll-to-bottom-on-output t)
  (my-sql-send-file (format "%s/default.sql" (getenv "HOME"))))

(add-hook 'sql-mode-hook 'my-sql-setup)
(add-hook 'sql-interactive-mode-hook 'my-sql-interactive-setup)
(add-to-list 'auto-mode-alist '("\\.ddl$" . sql-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; imenu

(when window-system
  (global-set-key [C-down-mouse-3] 'imenu))

(setq imenu-sort-function 'imenu--sort-by-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generic modes

(require 'generic-x)

(setq generic-extras-enable-list
      (append
       (if is-win32 generic-mswindows-modes generic-unix-modes)
       (list 'ini-generic-mode)))

(add-to-list 'auto-mode-alist '("my.cnf$" . samba-generic-mode))

(defvar generic-mode-map nil "Keys used in some generic modes.")
(setq generic-mode-map (make-sparse-keymap))
(define-key generic-mode-map "\C-c\C-c" 'comment-region)

(eval-when-compile (require 'generic))
(require 'generic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pager

(require 'pager)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ascii

(eval-after-load "ascii"
  '(set-face-background 'ascii-ascii-face (face-background 'region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gud

(eval-when-compile (require 'gud))

(defvar gud-added-font-lock-keywords nil)
(when window-system
  (let* ((commands
          (eval-when-compile
            (regexp-opt
             '("adb" "assign" "attach" "bsearch" "call" "cancel" "catch" "check" "clear"
               "collector" "commands" "cont" "dbxdebug" "dbxenv" "debug" "delete" "detach"
               "dis" "display" "document" "down" "dump" "edit" "examine" "exception"
               "exists" "file" "files" "fix" "fixed" "frame" "func" "funcs" "handler"
               "help" "hide" "history" "ignore" "import" "intercept" "language" "line"
               "list" "listi" "loadobject" "loadobjects" "lwp" "lwps" "mmapfile" "module"
               "modules" "next" "nexti" "pathmap" "pop" "print" "prog" "quit" "regs"
               "replay" "rerun" "restore" "rprint" "run" "runargs" "save" "scopes"
               "search" "setenv" "showblock" "showleaks" "showmemuse" "source" "status"
               "step" "stepi" "stop" "stopi" "suppress" "sync" "syncs" "thread" "threads"
               "trace" "tracei" "unbutton" "uncheck" "undisplay" "unhide" "unintercept"
               "unsuppress" "up" "use" "whatis" "when" "wheni" "where" "whereami" "whereis"
               "which" "whocatches") t)))
         )
    (setq gud-added-font-lock-keywords
          (list
           (cons "dbx<[0-9]+>" 'font-lock-comment-face)
           (cons (concat "\\<" commands "\\>") 'font-lock-builtin-face)))
    (font-lock-add-keywords 'gud-mode gud-added-font-lock-keywords)))

(defun my-gud-setup ()
  (when window-system (font-lock-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diff

(eval-when-compile (require 'diff-mode))
(defun my-diff-setup ()
  (copy-face 'font-lock-string-face 'diff-removed-face)
  (copy-face 'font-lock-builtin-face 'diff-added-face)
  (copy-face 'font-lock-comment-face 'diff-hunk-header-face))
(add-hook 'diff-mode-hook 'my-diff-setup)
(add-to-list 'auto-mode-alist '("[.-]patch$" . diff-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell-script-mode

(add-to-list
 'auto-mode-alist
 (cons
  (format "/%s$"
          (eval-when-compile
            (regexp-opt '(".bash_logout" ".bash_profile" ".bashrc"
                          ".inputrc" "bashrc" "inputrc" "profile"))))
  'shell-script-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell-mode (REMIND)

;; (global-set-key-override "\t" 'comint-dynamic-complete 'shell-mode)
;; (global-set-key-override "\C-c\C-c" 'comint-interrupt-subjob 'shell-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xml

(eval-when-compile (require 'nxml-mode))
(defun my-nxml-setup ()
  (modify-syntax-entry ?= ".")
  (modify-syntax-entry ?& "w")
  (setq nxml-child-indent 2)
  (setq nxml-slash-auto-complete-flag t))
(add-hook 'nxml-mode-hook 'my-nxml-setup)

(defun my-nxml-tab (arg)
  "Tab that either indents or nxml completes."
  (interactive "*P")
  (cond
   ((and transient-mark-mode mark-active)
    (indent-region (region-beginning) (region-end) nil))
   ((and (eq (char-syntax (preceding-char)) ?w)
         (not (= (current-column) 0)))
    (hippie-expand arg))
   ;;    (nxml-complete))
   (t (indent-for-tab-command))))

(add-to-list 'auto-mode-alist '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|tld\\|jsp\\|tag\\|xul\\|htm\\|html\\|rhtml\\)\\'" . nxml-mode))
(fset 'html-mode 'nxml-mode)
(fset 'sgml-mode 'nxml-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; antlr

(autoload 'antlr-mode "antlr-mode" nil t)
(setq auto-mode-alist (cons '("\\.g\\'" . antlr-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby mode

(eval-when-compile (require 'ruby-mode))

(autoload 'rdebug "rdebug" "ruby-debug interface" t)

(defun my-ruby-setup ()
  (setq indent-tabs-mode nil)
  (setq ruby-insert-encoding-magic-comment nil)
  (define-key ruby-mode-map "\C-m" 'newline-and-indent))
(add-hook 'ruby-mode-hook 'my-ruby-setup)

(defvar ruby-added-font-lock-keywords nil)
(when window-system
  (let* ((commands
          (eval-when-compile
            (regexp-opt
             '("include" "extend" "require" "require_dependency") t)))
         )
    (setq ruby-added-font-lock-keywords
          (list
           (cons (concat "\\<" commands "\\>") 'font-lock-keyword-face)))
    (font-lock-add-keywords 'ruby-mode ruby-added-font-lock-keywords)))

(add-to-list 'auto-mode-alist '("/\\(Rake\\|Gem\\|Cap\\|Tel\\)file$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.\\(rake\\|rxml\\|pill\\|irbrc\\|Rules\\|pryrc\\)\\'$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.\\(sinew\\|gemspec\\|builder\\)$" . ruby-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lua

(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-hook 'lua-mode-hook 'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; haml

(eval-when-compile (require 'haml-mode))
(defun my-haml-setup ()
  (make-local-variable 'standard-indent)
  (setq standard-indent 2)
  (define-key haml-mode-map [C-left] 'my-decrease)
  (define-key haml-mode-map [C-right] 'my-increase)
  (setq haml-backspace-backdents-nesting nil)
  (modify-syntax-entry ?_ "." haml-mode-syntax-table))
(add-hook 'haml-mode-hook 'my-haml-setup)

(setq auto-mode-alist (cons '("\\.haml$" . haml-mode) auto-mode-alist))
(autoload 'haml-mode "haml-mode" "Haml editing mode." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sh-script

(eval-when-compile (require 'sh-script))
(defun my-sh-setup ()
  (define-key sh-mode-map "\C-c\C-c" 'comment-region)
  (define-key sh-mode-map "\C-c\C-u" 'uncomment-region)
  (setq sh-basic-offset 2))
(add-hook 'sh-mode-hook 'my-sh-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown-mode

(eval-when-compile (require 'markdown-mode))
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))
(defun my-markdown-setup ()
  (turn-on-auto-fill)
  (define-key markdown-mode-map (kbd "<tab>") 'my-hippie-tab)
  (modify-syntax-entry ?\" "."))
(add-hook 'markdown-mode-hook 'my-markdown-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ispell

(eval-after-load "ispell"
  '(progn
     (setq
      ispell-extra-args '("--mode=sgml")
      ispell-program-name "aspell"
      ispell-silently-savep t)
     (set-default 'ispell-skip-html t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git

(add-to-list 'vc-handled-backends 'Git)
(setq vc-git-diff-switches '("--ignore-space-change"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grep

(eval-after-load "grep"
  '(grep-apply-setting 'grep-command "grep -nH -i "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido

(eval-when-compile (require 'ido))
(eval-after-load "ido"
  '(progn
     (defun my-ido-setup ()
       (global-set-key "\C-x\C-b" 'ido-switch-buffer)
       (global-set-key "\C-xb" 'ido-switch-buffer)
       (setq
        ido-case-fold t
        ido-confirm-unique-completion t
        ido-enable-flex-matching t
        ido-enable-tramp-completion nil
        ido-ignore-buffers '("\\` " "^\*")
        ))

     (ido-mode 'buffer)
     (my-ido-setup)
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; warnings

(eval-after-load "warnings"
  '(add-to-list 'warning-suppress-types '(undo discard-info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; midnight

(require 'midnight)
; kill buffers that haven't been used in 5 days
(setq clean-buffer-list-delay-general 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniquify

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ffip

(setq
 ffip-patterns '("*.rb" "*.haml" "*.el" "*.js" "*.yml" "*.erb" "*.scss" "*.jade" "*.coffee" "*.less")
 ffip-find-options "-or -path '*/node_modules' -prune")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs-lisp-mode

(eval-when-compile (require 'lisp-mode))
(defun my-emacs-lisp-setup ()
  (setq tab-width 8))
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-setup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; less-css-mode

(eval-when-compile (require 'less-css-mode))
(defun my-less-css-setup ()
  (define-key less-css-mode-map "\C-c\C-c" 'comment-region))
(add-hook 'less-css-mode-hook 'my-less-css-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jade-mode

;; (eval-when-compile (require 'jade-mode))
;; (defun my-jade-setup ()
;;   (make-local-variable 'standard-indent)
;;   (setq standard-indent 2)
;;   (define-key jade-mode-map [C-left] 'my-decrease)
;;   (define-key jade-mode-map [C-right] 'my-increase))
;; (add-hook 'jade-mode-hook 'my-jade-setup)
