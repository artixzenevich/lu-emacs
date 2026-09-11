;;; lu-lang-mode.el --- Major mode for the lu-lang programming language -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Artik Zenevich

;; Author: Artik Zenevich <azenevich91@gmail.com>
;; Created: 2026
;; Keywords: languages, education
;; URL: https://github.com/artixzenevich/lu-emacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is part of lu-emacs.

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Major mode for lu-lang — учебного языка программирования на русском
;; языке, интерпретируемого поверх Python.
;;
;; Возможности:
;;   - подсветка синтаксиса (font-lock);
;;   - автоматические отступы для логических блоков;
;;   - сворачивание блоков (hideshow);
;;   - автодополнение ключевых слов и символов файла
;;     (completion-at-point).
;;
;; Для режима не требуется ничего, кроме GNU Emacs 29+. Сниппеты лежат
;; в каталоге snippets/lu-mode/ и подключаются через yasnippet
;; (см. README.md).

;;; Code:

(require 'cl-lib)
(require 'font-lock)
(require 'hideshow)

(defgroup lu-lang nil
  "Режим редактирования исходников lu-lang."
  :group 'languages
  :prefix "lu-lang-")

(defcustom lu-lang-indent-offset 4
  "Ширина отступа для вложенных блоков."
  :type 'integer
  :group 'lu-lang)

(defconst lu-lang--keywords-control
  '("если" "то" "иначе" "конец" "пока" "для" "от" "до" "повтори" "раз"))

(defconst lu-lang--keywords-declaration
  '("запомнить" "процедура" "вернуть"))

(defconst lu-lang--keywords-command
  '("печать" "ввод" "выполнить"))

(defconst lu-lang--keywords-logical
  '("и" "или" "не"))

(defconst lu-lang--constants
  '("истина" "ложь" "ничего"))

(defun lu-lang--kw-regexp (words)
  "Регэкспеп, матчащий целые слова WORDS с границами слова."
  (concat "\\b\\(" (regexp-opt words t) "\\)\\b"))

(defvar lu-lang-font-lock-keywords
  `(
    ;; многострочный комментарий /// ... ///
    (lu-lang--fontify-block-comment 0 font-lock-comment-face prepend)
    ;; однострочные комментарии
    ("#[^\n]*" . font-lock-comment-face)
    ("//[^\n]*" . font-lock-comment-face)
    ;; строки и символы
    ("\"[^\"\n]*\"" . font-lock-string-face)
    ("'[^'\n]'" . font-lock-string-face)
    ;; числа
    ("\\b[0-9]+\\(?:\\.[0-9]+\\)?\\b" . font-lock-constant-face)
    ;; ключевые слова управления
    (,(lu-lang--kw-regexp lu-lang--keywords-control) . font-lock-keyword-face)
    ;; объявления
    (,(lu-lang--kw-regexp lu-lang--keywords-declaration) . font-lock-keyword-face)
    ;; команды
    (,(lu-lang--kw-regexp lu-lang--keywords-command) . font-lock-keyword-face)
    ;; логические операторы
    (,(lu-lang--kw-regexp lu-lang--keywords-logical) . font-lock-keyword-face)
    ;; константы
    (,(lu-lang--kw-regexp lu-lang--constants) . font-lock-constant-face)
    ;; встроенная функция длина
    ("\\bдлина\\b" . font-lock-function-name-face)
    ;; операторы
    ("[+*/<>=!-]" . font-lock-keyword-face)
    )
  "Набор правил подсветки для lu-lang-mode.")

(defun lu-lang--fontify-block-comment (limit)
  "Матчер многострочного комментария /// ... /// до LIMIT."
  (when (re-search-forward "///" limit t)
    (let* ((start (match-beginning 0))
           (has-end (re-search-forward "///" limit t))
           (end (if has-end (match-end 0) (point))))
      (set-match-data (list start end))
      t)))

;; ---------------------------------------------------------------------------
;; Отступы

(defconst lu-lang--block-open-regexp
  (concat "^\\s-*\\(?:процедура\\b\\|если\\b\\|пока\\b\\|для\\b\\|повтори\\b\\|иначе\\b\\)")
  "Строка, открывающая логический блок.")

(defconst lu-lang--block-close-regexp
  (concat "^\\s-*\\(?:конец\\b\\|иначе\\b\\)")
  "Строка, закрывающая (или прерывающая продолжение) логического блока.")

(defun lu-lang--line-text ()
  "Текст текущей строки без завершающего перевода строки."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun lu-lang--skip-blank-backward ()
  "Перейти на последнюю значимую (непустую) строку назад."
  (while (and (not (bobp))
              (or (looking-at-p "^\\s-*$")
                  (looking-at-p "^\\s-*[#/]")))
    (forward-line -1)))

(defun lu-lang--previous-code-pos ()
  "Точка начала предыдущей значимой строки или nil, если её нет."
  (save-excursion
    (let ((cur (line-number-at-pos)))
      (forward-line -1)
      (lu-lang--skip-blank-backward)
      (when (< (line-number-at-pos) cur)
        (point)))))

(defun lu-lang--previous-code-indent ()
  "Отступ предыдущей непустой и некомментарной строки."
  (let ((pos (lu-lang--previous-code-pos)))
    (if pos
        (save-excursion
          (goto-char pos)
          (current-indentation))
      0)))

(defun lu-lang--previous-code-opens-block ()
  "Открывает ли предыдущая кодовая строка логический блок."
  (let ((pos (lu-lang--previous-code-pos)))
    (when pos
      (save-excursion
        (goto-char pos)
        (looking-at-p lu-lang--block-open-regexp)))))

(defun lu-lang--indent-column ()
  "Нужный столбец отступа для текущей строки."
  (let ((offset lu-lang-indent-offset))
    (save-excursion
      (back-to-indentation)
      (cond
       ;; строка закрывает блок — возврат на уровень открывателя
       ((string-match-p lu-lang--block-close-regexp (lu-lang--line-text))
        (max 0 (- (lu-lang--previous-code-indent) offset)))
       ;; после строки-открывателя — вложенный уровень
       ((lu-lang--previous-code-opens-block)
        (+ (lu-lang--previous-code-indent) offset))
       (t
        (lu-lang--previous-code-indent))))))

(defun lu-lang-indent-line ()
  "Выровнять текущую строку по правилам отступов lu-lang."
  (interactive)
  (indent-line-to (lu-lang--indent-column)))

(defun lu-lang--auto-outdent-closer ()
  "После ввода слова «конец»/«иначе» выровнять строку влево."
  (when (and (eq this-command 'self-insert-command)
             (string-match-p
              (concat "^\\s-*\\b\\(?:конец\\|иначе\\)\\b.*$")
              (lu-lang--line-text)))
    (save-excursion
      (lu-lang-indent-line)
      (end-of-line))))

;; ---------------------------------------------------------------------------
;; Сворачивание

(add-to-list 'hs-special-modes-alist
             (list 'lu-lang-mode
                   (concat "^\\s-*\\(?:процедура\\|если\\|пока\\|для\\|повтори\\)\\b.*$")
                   "^\\s-*\\bконец\\b.*$"
                   nil nil))

;; ---------------------------------------------------------------------------
;; Автодополнение

(defconst lu-lang--complete-keywords
  '("запомнить" "печать" "ввод" "выполнить" "вернуть" "процедура"
    "если" "то" "иначе" "конец" "пока" "для" "от" "до" "повтори" "раз"
    "и" "или" "не" "длина" "истина" "ложь" "ничего"))

(defun lu-lang--file-symbols ()
  "Имена переменных, параметров, счётчиков и процедур из буфера."
  (let (syms)
    (save-excursion
      (goto-char (point-min))
      ;; переменные, объявленные через запомнить
      (while (re-search-forward "\\bзапомнить\\s-+\\(\\w+\\)" nil t)
        (push (match-string-no-properties 1) syms))
      ;; счётчики циклов для
      (goto-char (point-min))
      (while (re-search-forward "\\bдля\\s-+\\(\\w+\\)\\s-+\\bот\\b" nil t)
        (push (match-string-no-properties 1) syms))
      ;; параметры процедур
      (goto-char (point-min))
      (while (re-search-forward "\\bпроцедура\\s-+\\w+\\s-*:" nil t)
        (let ((rest (buffer-substring-no-properties (point) (line-end-position))))
          (dolist (part (split-string rest "[,]+" t))
            (setq part (string-trim part))
            (when (string-match-p "\\`\\w+\\'" part)
              (push part syms))))
        (forward-line))
      (nreverse syms))))

(defun lu-lang--completion-at-point ()
  "Точка завершения: ключевые слова и символы буфера."
  (let* ((end (point))
         (start (save-excursion (skip-syntax-backward "w_") (point))))
    (when (> end start)
      (list start end
            (cl-remove-duplicates
             (append lu-lang--complete-keywords
                     (lu-lang--file-symbols))
             :test #'equal)))))

;; ---------------------------------------------------------------------------
;; Режим

;;;###autoload
(define-derived-mode lu-lang-mode prog-mode "Lu"
  "Режим редактирования исходников lu-lang — русского учебного языка.

Основные возможности: подсветка синтаксиса, автоматические отступы
логических блоков, сворачивание блоков и автодополнение."
  (setq-local comment-start "#")
  (setq-local comment-start-skip "\\(?:#\\|//\\|///\\)\\s-*")
  (setq-local comment-end "")
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width lu-lang-indent-offset)
  (setq-local indent-line-function #'lu-lang-indent-line)
  (setq-local font-lock-defaults '(lu-lang-font-lock-keywords nil nil))
  (setq-local completion-at-point-functions '(lu-lang--completion-at-point))
  (add-hook 'post-self-insert-hook #'lu-lang--auto-outdent-closer nil t)
  (hs-minor-mode +1))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lu\\'" . lu-lang-mode))

(provide 'lu-lang-mode)

;;; lu-lang-mode.el ends here