(ns wordspacebook.test.core
  (:use [wordspacebook.core] :reload)
  (:use [clojure.test]))

(deftest test-friend?
  (is (friend? "foo" "fooo"))
  (is (friend? "foo" "fo"))
  (is (friend? "foo" "foa"))
  (is (not (friend? "foo" "bar")))
  (is (not (friend? "fo" "fooo"))))



(deftest test-find-friends
  (let [word-list ["foo" "bar" "fo" "foc" "baz" "bap"]
        word-map (make-word-map word-list)]
    (is (= ["fo" "foo" "foc"] (find-friends "foz" word-map)))))




