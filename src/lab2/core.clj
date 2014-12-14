(ns lab2.core
  (:require [net.cgrand.enlive-html :as html])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clj-http.client :as client])
  (:require [clojurewerkz.urly.core :as url])
  (:gen-class))


;codes for checking
(def http-redirect-codes #{301 302 303 307})
(def http-success-codes #{200 201 202 203 204 205 206 207})



;		atomed set with crawled url

(def crawled-urls (atom #{}))



(defn contain-in-sequence?
  [seqence elm]  
  (not (nil? (some #(= elm %) seqence))))

(defn get-a-href [base href]
  (let [url-href (url/url-like href)]
    (->> (if (not (url/absolute? url-href))
           (url/resolve (url/url-like base) url-href)
           url-href)
         (.toString)))
)	

(defn response-status [response]
	(let [code (:status response)]
		(cond 
			(= 404 code) {:status "Bad url"}
			(contain-in-sequence? http-redirect-codes code) {:status "Redirect" :location (:location (:headers response))}
			(contain-in-sequence? http-success-codes code) {:status "Good"}
		)
	)
)

(defn url-response [url]
	(try 
 		(client/get (url/url-like url)
 		  {:throw-exceptions false
            :conn-timeout     60000
            :follow-redirects false}
            )
 		(catch Exception e {:status 404}))
)

(defn read-url-from-file [file-name]
  {:pre [(not (nil? file-name))]}
	(if (.exists (io/file file-name))
	  	(with-open [rdr (io/reader file-name)]
	 	 	(doall (map read-string (line-seq rdr)))
	 	)
	  	(println (str "ERROR: file " file-name " does not exists"))
	)
)

(defn links-from-html [body base]
	(reduce (fn [hrefs a-tag]
    			(let [href (:href (:attrs a-tag))]
                	(if (not (nil? href))
                    	(conj hrefs (get-a-href base href)))))
      		[]
      		(html/select (html/html-snippet body) #{[:a]})
    )
)

(defn process-url [url level]
	(let [response (url-response url)
		body (:body response)]

	)
)

(defn crawl-urls [urls level]
	(when (> level 0)
		(pmap (fn [url] 
			(process-url url level)) urls)
	)
)


(defn -main [file-path levels]
	(let [levels (Integer/parseInt levels)
		  urls (read-url-from-file file-path)]
		  (println levels)
		  (println urls)
		(crawl-urls urls levels)
	)
)