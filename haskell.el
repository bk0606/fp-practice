
(add-to-list 'exec-path "~/.cabal/bin")

(defun red/haskell-mode-hook ()
  (capitalized-words-mode)
  (turn-on-haskell-indent)
  (interactive-haskell-mode))

(add-hook 'haskell-mode-hook 'red/haskell-mode-hook)
(remove-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(setq haskell-process-type 'cabal-repl)

