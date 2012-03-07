(ns muxie.save
  "Serialize to disk.")

(defn output-user
  [user]
  (let [user (dissoc user :cxnin :cxnout :outs :log)]
    (pr user)))
  