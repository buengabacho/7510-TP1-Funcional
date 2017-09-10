(ns logical-interpreter
	(:require [clojure.string :refer [split-lines trim]])
)

(defn parse-as-rule
	"Tries to parse something as a rule."
	[candidate]
	(re-matches #".*\((.*)\) *:- *(.*)\." candidate)
)

(defn parse-line
	"Parses a line in the input db."
	[line]
	(let [line-as-rule (parse-as-rule line)]
		(if line-as-rule {:type "rule" :parsed-line line-as-rule} {:type "fact" :parsed-line line})
	)
)

(defn get-by-type
	"Filters the given array of lines by type and obtains their parsed values"
	[parsed-lines-array type]
	(let [filtered (filter #( = (% :type) type ) parsed-lines-array)
			  return-values (map :parsed-line filtered)]
		return-values
	)
)

(defn build-db-map
	"Builds the database map from an array of parsed lines. The resulting array has the 
	following structure: { :facts [fact1 fact2 ..] :rules [rule1 rule2 ..] } "
	[parsed-lines-array]
	(let [facts (get-by-type parsed-lines-array "fact")
				rules (get-by-type parsed-lines-array "rule")
				db-map {:facts facts :rules rules}]
		db-map
	)
)

(defn parse-database
	"Returns a dictionary with rules and facts."
	[db]
	(let [lines-array (split-lines db)
				trimmed-lines-array (map trim lines-array)
				no-empty-lines-array (filter not-empty trimmed-lines-array)
				parsed-lines-array (map parse-line no-empty-lines-array)
				parsed-db (build-db-map parsed-lines-array)]
		; (map (fn [line] (println (str "line: " line))) split-db)
		; (map println split-db)
		parsed-db
	)
)

(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  (let [parsed-database (parse-database database)]
 		(println "parsed database facts: ")
 		(run! #(println "        " %) (parsed-database :facts))
 		(println "parsed database rules: ")
 		(run! #(println "        " %) (parsed-database :rules))
  )
)