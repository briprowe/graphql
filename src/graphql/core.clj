(ns graphql.core
  (:import [graphql.parser Parser]
           [graphql.language Document]))


(defmulti ->om-next
  (fn [document]
    (type document)))

(defmethod ->om-next Document
  [document]
  `[~@(mapcat ->om-next (.getChildren document))])

(defmethod ->om-next graphql.language.OperationDefinition
  [op]
  (->om-next (.getSelectionSet op)))

(defmethod ->om-next graphql.language.SelectionSet
  [selection-set]
  (mapv ->om-next (.getSelections selection-set)))

(defn ->argument-map
  [args]
  (into {}
        (map (juxt #(keyword (.getName %)) #(.getValue (.getValue %))))
        args))

(defn apply-arguments
  [query args]
  (if (empty? args)
    query
    (list query (->argument-map args))))

(defn classify-field
  [field]
  (let [args (.getArguments field)
        selection-set (.getSelectionSet field)]
    (cond
      (and (not (empty? args)) (not (nil? selection-set)))
      :parameterized-join

      selection-set
      :join

      :else
      :parameterized)))

(defmulti render-field classify-field)

(defn render-field-name
  [field]
  (keyword (.getName field)))

(defn render-join
  [field]
  `{~(render-field-name field) ~(->om-next (.getSelectionSet field))})

(defmethod render-field :parameterized-join
  [field]
  (apply-arguments (render-join field)
                   (.getArguments field)))

(defmethod render-field :join
  [field]
  (render-join field))

(defmethod render-field :parameterized
  [field]
  (apply-arguments (render-field-name field) (.getArguments field)))

(defmethod ->om-next graphql.language.Field
  [field]
  (render-field field))

(comment

  (def query "{ user(id: 4) { name age } }")

  (-> (Parser.)
      (.parseDocument query)
      ->om-next)

  )
