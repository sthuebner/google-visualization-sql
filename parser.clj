;; TODO add namespace

(use 'name.choi.joshua.fnparse)
(use 'clojure.set)
(use 'clojure.test)

(defn run-p 
  [parser input]
  (let [result (rule-match parser 
                           #(prn "fail:" %&) 
                           #(prn "incomplete:" %&) 
                           {:remainder input})]
    (cond (nil? result) nil
          (vector? result) (apply str result)
          :else (str result))))

(defn- rule-to-set
  "Take a rule and create the set of characters it represents."
  [rule]
  (cond (map? rule)    (set (mapcat (fn [[lower upper]]
                                      (map char (range (int lower)
                                                       (inc (int upper)))))
                                    rule))
        (char? rule)   #{rule}
        (string? rule) (set rule)
        (set? rule)    rule
        :else (rule-to-set ({:digit {\0 \9}
                             :alpha {\a \z \A \Z}
                             :word  {\a \z \A \Z \0 \9}
                             :space #{\newline \tab \space}}
                            rule #{}))))

(defn char-class
  "This is an example parser that mirrors the character class 
           notation from regular expressions. It is also heavily inspired by 
           Christophe Grand's (https://github.com/cgrand/) regexp, 
           moustache, and enlive librarys."
  [ & rules ]
  (let [negated  (= (first rules) :not)
        rules    (if negated (rest rules) rules)
        ruleset  (apply union (map rule-to-set rules))
        validate (if negated (complement contains?) contains?)]
    (term #(validate ruleset %))))

(def apply-str (partial apply str))

(def minus-sign (lit \-))

(def decimal-point (lit \.))

(def zero-digit (lit \0))

(def nonzero-decimal-digit (lit-alt-seq "123456789" lit))

(def decimal-digit (alt zero-digit nonzero-decimal-digit))

(def fractional-part (conc decimal-point (rep* decimal-digit)))

(def integer-lit
  (complex [m (opt minus-sign)
            a (alt zero-digit (rep+ decimal-digit))]
           (-> [m a] flatten apply-str
               Integer/parseInt)))

(def double-lit
  (complex [m (opt minus-sign)
            a (opt (alt zero-digit (rep+ decimal-digit)))
            b fractional-part]
           (-> [m a b] flatten apply-str
               Double/parseDouble)))

(def number-lit (alt double-lit integer-lit))


(def newline-lit (lit \n))

(def return-lit (lit \r))

(def line-break (rep+ (alt newline-lit return-lit)))

(def string-raw-char (alt line-break anything))

(def string-delimiter (lit-alt-seq "'\"" lit))

(def escape-char (lit \\))

(def unescaped-string-raw-char
  (except string-raw-char (alt escape-char string-delimiter)))

;; TOOD add escape sequences
(def string-char unescaped-string-raw-char)

(def string-lit
  (complex [_ string-delimiter
            contents (rep* string-char)
            _ string-delimiter]
           (-> contents apply-str)))

(deftest test-string-lit
  (is (= (run-p string-lit "'abc'") "abc"))
  (is (= (run-p string-lit "''") "")))
