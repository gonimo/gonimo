;; (
;;  (haskell-mode . ((haskell-process-args-stack-ghci . ("--ghc-options=-ferror-spans" "gonimo-back:exe:gonimo-back"))))
;;  )
((haskell-mode . ((haskell-compile-cabal-build-command . "cd %s && ../runIn.sh cabal build --ghc-option=-ferror-spans")
                  ))
 )
