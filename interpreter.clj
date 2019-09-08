(ns clojure-noob.core)

(defn get-value
  "returns the currently pointed at value"
  [{:keys [ptr tape]}]
  (get tape ptr))

(defn tape-operation
  "Makes an operation on the tape defaulting value to 1,"
  [{:keys [tape ptr] :as machine} operation]
  (let [value (get tape ptr)]
    (assoc-in machine [:tape ptr] (operation value 1))))

(defn ptr-operation
  "Increments or decrements the ptr safely."
  [{:keys [tape ptr] :as machine} operation]
  (let [max-key (apply max (keys tape))
        min-key (apply min (keys tape))
        updated-ptr (operation ptr 1)]
    (let [new-entry-key (if (or (> updated-ptr max-key) (< updated-ptr min-key))
                          updated-ptr)]
      {:tape (if (nil? new-entry-key)
               tape
               (assoc tape new-entry-key 0))
       :ptr updated-ptr})))

(defn tape-operation-read
  "Reads a single character into the position of the tape"
  [{:keys [ptr] :as machine}]
  (assoc-in machine [:tape ptr] (int (.charAt (read-line) 0))))

(defn tape-operation-print
  "Prints the currently pointed at character into the chat."
  [machine convert-to-ascii]
  (let [val (get-value machine)]
    (do
      (print (if (= convert-to-ascii true)
               (char val)
               val))
      machine)))


(defn execute-simple-operation
  "Executes a single operation."
  [machine operation]
  (let [operation-distribution {
                                \+ [tape-operation machine +]
                                \- [tape-operation machine -]
                                \> [ptr-operation machine +]
                                \< [ptr-operation machine -]
                                \. [tape-operation-print machine true]
                                \, [tape-operation-read machine]}]
    (let [chosen-operation (first (get operation-distribution operation))
          chosen-args (drop 1 (get operation-distribution operation))]
      (apply chosen-operation chosen-args))))

(defn execute-simple-bunch
  "Executes a vector of instructions on a machine."
  [machine [instruction & instructions]]
  (if (empty? instructions)
    (execute-simple-operation machine instruction)
    (execute-simple-bunch (execute-simple-bunch machine instruction) instructions)))

(defn find-matching-parenthesis
  "Finds the matching paranthesis position in a given instructionset"
  ([instructions position]
   (find-matching-parenthesis instructions position (inc position) 1))
  ([instructions start-position current-pos parenthesis-score]
   (let
     [instruction (nth instructions current-pos)]
     (let
       [curr-score (if (= instruction \[)
                     (inc parenthesis-score)
                     (if (= instruction \])
                       (- parenthesis-score 1)
                       parenthesis-score))]
       (if (= curr-score 0)
         {start-position current-pos}
         (recur instructions start-position (inc current-pos) curr-score))))))

(defn validate-parenthesis
   "Checks if parenthesis is in dykt-language"
   ([instructions accumulator]
    (if (empty? instructions)
      (= accumulator 0)
      (let [instruction (first instructions)]
        (recur (drop 1 instructions) (if (= instruction \[)
                                       (inc accumulator)
                                       (if (= instruction \[)
                                         (dec accumulator)
                                         accumulator))))))
  ([instructions]
   (validate-parenthesis instructions 0)))

(declare execute-instructions)

(defn execute-loop
  [machine instructions]
  (let [curr-value (get-value machine)]
    (if (= curr-value 0)
      machine
      (recur (execute-instructions machine instructions) instructions))))

(defn execute-instructions
  [machine instructions]
  (if (empty? instructions)
    machine
    (let [instruction (first instructions)
          remaining-instructions (drop 1 instructions)]
      (if (= instruction \[)
        (let [loop-size (get (find-matching-parenthesis instructions 0) 0)]
          (let [loop-content (take (dec loop-size) remaining-instructions)]
            (let [loop-removed (drop loop-size remaining-instructions)]
              (recur (execute-loop machine loop-content) loop-removed))))
        (recur (execute-simple-operation machine instruction) remaining-instructions)))))


(def initial-machine {:tape {0 0} :ptr 0})
(def test-instructions (seq "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."))

(defn -main
  [& args]
  (do
    (println (execute-instructions initial-machine test-instructions))))

