(ns portal-kaocha)

(defn- start-test
  []
  {:fail? false
   :reports []})

(def test-reports (atom (start-test)))

(defn slim-report
  [report]
  (dissoc report
          :kaocha/test-plan
          :kaocha/testable))

(defn conj-report!
  [report]
  (swap! test-reports
         #(update % :reports conj report)))

(comment
  (deref test-reports)
  (conj-report! {})
  ,)

(defn hiccup-summary
  [{:keys [error fail pass test]}]
  (let [hiccup [:div
                [:h3 "Summary"]
                [:table
                 [:tr [:th "Tests"]
                      [:th "Passed"]
                      [:th "Failed"]
                      [:th "Error"]]
                 [:tr [:td test]
                      [:td pass]
                      [:td fail]
                      [:td error]]]]]
    (with-meta
      hiccup
      {:portal.viewer/default
       :portal.viewer/hiccup})))

(defmulti test-report :type)

(defmethod test-report :default [_report])

(defmethod test-report :begin-test-suite [_]
  (reset! test-reports (start-test)))

(defmethod test-report :begin-test-var [report]
  (conj-report! (slim-report report)))

(defmethod test-report :begin-test-ns [report]
  (conj-report! (slim-report report)))

(defmethod test-report :pass [_report])

(defmethod test-report :fail [report]
  (swap! test-reports
         (fn [report-info]
           (-> report-info
               (update :reports conj (slim-report report))
               (assoc :fail? true)))))

(defmethod test-report :summary [report]
  (if (:fail? @test-reports)
    (do
     (conj-report! (hiccup-summary report))
     (doseq [report (:reports @test-reports)]
       (tap> report)))
    (tap> (hiccup-summary report))))
