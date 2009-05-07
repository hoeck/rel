 
(ns hoeck.rel.iris-interfaces
  (:import (org.deri.iris.api.terms IStringTerm)))

;; custom iris-types:
;; marker interfaces for keywords and symbols

(gen-interface :name hoeck.rel.iris/ISymbol
               :extends [org.deri.iris.api.terms.IStringTerm])

(gen-interface :name hoeck.rel.iris/IKeyword
               :extends [org.deri.iris.api.terms.IStringTerm])

