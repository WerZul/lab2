(ns lab2.core-test
  (:require [clojure.test :refer :all]
            [lab2.core :refer :all :as core]))

(deftest test-parse-html
  (testing "Find links"

    (testing "Simple find links"
      (let [html-body (str "<div>"
                         		"<a href=\"http://site1.dom/\"></a>"
                         		"<a href=\"https://site1.dom/page1\"></a>"
                         		"<div><a href=\"www.site2.com\"></a></div>"
                         		"<a>empty a</a>"
                         	"</div>")
            hrefs (#'core/links-from-html html-body "http://base.com")]
            (println hrefs)
        (is (= (count hrefs) 3))))

    (testing "find links with base url"
      (let [html-body (str "<div>"
                         		"<a href=\"/page\"></a>"
                         		"<a href=\"page?url-value=1\"></a>"
                         	"</div>")
      		base-url "http://base.com"
            hrefs (#'core/links-from-html html-body base-url)]
            (println hrefs)
        (is (= hrefs ["http://base.com/page" "http://base.com/page?url-value=1"]))))
  )
)

(deftest test-http-errors
  (testing "HTTP Status test"

    (testing "404"
      (let [status (:status (#'core/response-status {:status 404}))]
      	(is (= "Bad url" status))))

    (testing "301"
      (let [status (:status (#'core/response-status {:status 301}))]
      	(is (= "Redirect" status))))

    (testing "200"
      (let [status (:status (#'core/response-status {:status 200}))]
      	(is (= "Good" status)))) 
  )
)
