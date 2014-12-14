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
(def output (atom []))



(defn contain-in-sequence?
  [seqence elm]  
  (not (nil? (some #(= elm %) seqence))))


(defn indent [level]
  (str/join "" (take level (repeat "  "))))

(defn get-a-href [base href]
  (let [url-href (url/url-like href)
  		base-url (url/url-like base)]

  	; (println href "->" url-href)
  	; (println base "->" base-url)
    (->> (if (url/relative? url-href)
           (url/resolve base-url url-href)
           url-href)
    	 (.toString))
  )
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
 		(client/get url
 		  	{:throw-exceptions false
            :conn-timeout     60000
            :follow-redirects false}
            )
)

(defn read-url-from-file [file-name]
  {:pre [(not (nil? file-name))]}
	(if (.exists (io/file file-name))
	  	(with-open [rdr (io/reader file-name)]
	 	 	(defn read-string [string] string)
	 	 	(doall (map read-string (line-seq rdr)))
	 	)
	  	(println (str "ERROR: file " file-name " does not exists"))
	)
)

(defn links-from-html [body base]
	(reduce (fn [hrefs a-tag]
    			(let [href (:href (:attrs a-tag))]
                	(if (not (nil? href))
                    	(conj hrefs (get-a-href base href))
                    	hrefs)))								;;; FIX - when is empty (:a without href)
      		[]
      		(html/select (html/html-snippet body) #{[:a]})
    )
)

(defn process-url [url base level]
	; (println "process-url" url)

	(let [response (url-response url)
		body (:body response)
		hrefs (links-from-html body base)
		status (response-status response)]

		(swap! output conj (str (indent level) url " " (:status status)))
		(crawl-urls hrefs base (- level 1))
	)
)

(defn crawl-urls 
	([urls level]
		(when (> level 0)
			(doall (map (fn [url]
							(when (not (contain-in-sequence? @crawled-urls url)) 
								((swap! crawled-urls conj url)
								(process-url url url level)))) 
						urls)))
	)

	([urls base level]
		(when (> level 0)
			(doall (map (fn [url]
							(when (not (contain-in-sequence? @crawled-urls url)) 
								((swap! crawled-urls conj url)
								(process-url url base level)))) 
						urls)))
	)
)


(defn -main [file-path levels]
	(let [levels (Integer/parseInt levels)
		  urls (read-url-from-file file-path)]
		(crawl-urls urls levels)
		(println output)
	)
)