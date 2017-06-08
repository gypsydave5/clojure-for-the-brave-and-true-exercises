(ns ale.core)

(def invalid-order-details
  {:name "Mitchard Blimmons"
   :email "mitchard.blimmonsgmail.com"})
(def valid-order-details
  {:name "Barry Doleman"
   :email "barry.doleman@gmail.com"})

(def order-details-validations
  {:name ["Please enter a name" not-empty]
   :email ["Please enter an email address"
           not-empty
           "Your email address doesn't look like an email address"
           #(or (empty %)
                (re-seq #"@" %))]})

(defn error-messages-for
  "Return a seq of error messages"
  [to-validate message-validator-pairs]
  (map first (filter #(not ((second %) to-validate))
                     (partition 2 message-validator-pairs))))

(defn validate
  "Returns a map with a vector of errors for each key"
  [to-validate validations]
  (reduce (fn [errors validation]
            (let [[fieldname validation-check-groups] validation
                  value (get to-validate fieldname)
                  error-messages (error-messages-for value validation-check-groups)]
              (if (empty? error-messages)
                errors
                (assoc errors fieldname error-messages))))
          {}
          validations))

(defmacro if-valid
  "Handle validation more concisely"
  [to-validate validations errors-name & then-else]
  `(let [~errors-name (validate ~to-validate ~validations)]
     (if (empty? ~errors-name)
       ~@then-else)))

(defmacro when-valid
  [to-validate validations & on-valid]
  `(if (empty? (validate ~to-validate ~validations))
     (do ~@on-valid)))

(defmacro my-and
  ([] true)
  ([x] x)
  ([x & rest]
   `(let [andy# ~x]
      (if andy# (my-and ~@rest) andy#))))

(defmacro my-or
  ([] nil)
  ([x] x)
  ([x & rest]
   `(let [this-or# ~x]
      (if this-or# this-or# (my-or ~@rest)))))
