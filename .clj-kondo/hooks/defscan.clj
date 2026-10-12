(ns hooks.defscan
  "What clj-kondo reads dk.simongray.whatwg-url.impl.chars/defscan as: a
  private function of the params whose test sees the code unit.")

(defmacro defscan
  "A private function `name` of the params of a text and an index, which
  gives an index, and whose `test` has the code unit `x`."
  [name doc [s i :as params] [x] test]
  `(defn- ~name ~doc ~params
     (let [~x (+ ~i (count ~s))]
       (if ~test ~i (count ~s)))))
