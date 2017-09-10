(ns database_parser
  (:require [clojure.string :refer [split-lines trim]])
)

(defn parse-args
  "Parses arguments of a rule or fact and retuns them as an array"
  [args-string]
  (re-seq #"[^,\s]+" args-string)
)

(defn parse-as-fact
  "Tries to parse something as a fact.
  A fact has the following structure:
  { :name aName :arguments [arg1 arg2 ..] }"
  [candidate]
  (let  [match  (re-matches #"(.*)\((.*)\).?" candidate)
         result (if-not match
                        nil
                        {
                          :name (match 1)
                          :arguments (parse-args (match 2))
                        }
                )
        ]
    result
  )
)

(defn parse-rule-facts
  "Parses all facts that compose a certain rule and returns them as an array"
  [facts-string]
  (map parse-as-fact (re-seq #"[^\s,][^)]*\)" facts-string))
)

(defn parse-db-line-as-rule
  "Tries to parse something as a rule. 
  A rule has the following structure: 
  { :name aName :arguments [arg1 arg2 ..] :facts [fact1 fact2 ..] }"
  [candidate]
  (let  [match  (re-matches #"(.*)\((.*)\) *:- *(.*)\." candidate)
        result  (if-not match 
                        nil 
                        { 
                          :name (match 1) 
                          :arguments (parse-args (match 2)) 
                          :facts (parse-rule-facts (match 3)) 
                        } 
                )
        ]
    result
  )
)

(defn parse-db-line
  "Parses a line in the input db."
  [line]
  (let  [line-as-fact (parse-as-fact line)
         line-as-rule (parse-db-line-as-rule line)
        ]
    (cond line-as-rule  {
                          :type "rule" 
                          :parsed-line line-as-rule
                        }
          line-as-fact  {
                          :type "fact" 
                          :parsed-line (parse-as-fact line)
                        }
          :else         nil
    )
  )
)

(defn get-by-type
  "Filters the given array of lines by type and obtains their parsed values"
  [parsed-lines-array type]
  (let  [filtered (filter #( = (% :type) type ) parsed-lines-array)
         return-values (map :parsed-line filtered)
        ]
    return-values
  )
)

(defn build-db-map
  "Builds the database map from an array of parsed lines. 
  The resulting array has the following structure: 
  { :facts [fact1 fact2 ..] :rules [rule1 rule2 ..] } "
  [parsed-lines-array]
  (let  [facts (get-by-type parsed-lines-array "fact")
         rules (get-by-type parsed-lines-array "rule")
         db-map {:facts facts :rules rules}
        ]
    db-map
  )
)

(defn parse-database
  "Returns a dictionary with rules and facts."
  [db]
  (let  [lines-array           	(split-lines db)
         trimmed-lines-array   	(map trim lines-array)
         no-empty-lines-array  	(filter not-empty trimmed-lines-array)
         parsed-lines-array    	(map parse-db-line no-empty-lines-array)
         invalid-db			  		 	(some nil? parsed-lines-array)
         parsed-db             	(if-not invalid-db 
         															  (build-db-map parsed-lines-array) 
         															  nil
         												)
        ]
    parsed-db
  )
)