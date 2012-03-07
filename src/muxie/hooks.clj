(ns muxie.hooks)

(defn register-hook [uref event hookvar]
  (swap! uref assoc-in [:hooks event (keyword (:name (meta hookvar)))] hookvar))

(defn remove-hook [uref event hookvar]
  (swap! uref (fn [usr]
		(assoc usr :hooks
		       (dissoc
			(get-in usr [:hooks event])
			(keyword (:name (meta hookvar))))))))

(defn run-hooks [uref event & args]
  (doseq [[hookid hook] (get-in @uref [:hooks event])]
    (apply hook uref args)))