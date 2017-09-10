(ns logical-interpreter
  (:require [database_parser :refer [parse-database parse-as-fact]])
)

(defn is-fact?
  "Determines whether the query is an existing fact."
  [db query]
  (let  [ is-fact-array (map #(= % query) (db :facts))
          is-fact (some true? is-fact-array)
        ]
    is-fact
  )
)

(defn fact-apply-args
  "Instantiates the given fact in a rule with the correct args."
  [fact args-map]
  ; (println "fact.arguments: " (fact :arguments))
  ; (println "args-map: " args-map)
  (update fact :arguments #(map (fn [arg-name] (args-map arg-name)) %)) ; (args-map (keyword arg-name))
)

(defn apply-rule
  "Applies the given rule with the given args-"
  [db rule args]
  (let  [ args-map (zipmap (rule :arguments) args)
          facts-array (map #(fact-apply-args % args-map) (rule :facts))
          facts-are-true-array (map #(is-fact? db %) facts-array)
          result (every? true? facts-are-true-array)
        ]
    result
  )
)

(defn try-apply-rule
  "Determines whether the passed query corresponds to the rule.
  If it does, it applies tne rule."
  [db rule query]
  (if-not ( = (rule :name) (query :name) )
          false
          (apply-rule db rule (query :arguments))
  )
)

(defn infer-truth
  "Determines whether the query can be infered as a rule."
  [db query]
  (let  [ is-rule-array (map #(try-apply-rule db % query) (db :rules))
          is-true (if (some true? is-rule-array) true false)
        ]
    is-true
  )
)

(defn query-is-true?
  "Determines whether a given query evaluates to true or not."
  [db query]
  (cond (= db nil)          nil
        (= query nil)       nil
        (is-fact? db query) true
        :else               (infer-truth db query)
  )
)

(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil."
  [database query]
  (let  [parsed-database (parse-database database)
         parsed-query     (parse-as-fact query)
        ]
    ; (println "parsed database facts: ")
    ; (run! #(println "        " %) (parsed-database :facts))
    ; (println "parsed database rules: ")
    ; (run! #(println "        " %) (parsed-database :rules))
    ; (println "query is true? " (query-is-true? parsed-database parsed-query))
    (query-is-true? parsed-database parsed-query)
  )
)