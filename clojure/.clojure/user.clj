(ns user
  (:require potemkin clj-java-decompiler.core criterium.core))

(potemkin/import-vars
  [clj-java-decompiler.core
   decompile
   disassemble]
  [criterium.core
   bench
   quick-bench])
