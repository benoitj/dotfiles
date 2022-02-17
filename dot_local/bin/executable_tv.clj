#!/usr/bin/env -S bb -o

(require '[babashka.curl :as curl])
(require '[cheshire.core :as json])

(defn fetch-lineup [hdhr-device-host]
  (let [lineup-response (curl/get (str hdhr-device-host "/lineup.json"))]
    (if (= 200 (lineup-response :status))
      (json/parse-string (lineup-response :body)))))

(defn get-lineup-menu-list [lineup-data]
  (map #(str (get % "GuideNumber") " " (get % "GuideName")) lineup-data))

(get-lineup-menu-list (fetch-lineup "HDHR-1050AAD8"))


                                        ;(get-lineup-menu-list (fetch-lineup "HDHR-1050AAD8"))


;#launch_tv() {
;#	selected="$(curl -s HDHR-1050AAD8/lineup.json | jq '.[] | (.GuideNumber + " "+ .GuideName)' | sed -e 's/"//g' | rofi -dmenu)"
;#
;#	url="$(curl -s HDHR-1050AAD8/lineup.json | jq '.[] | select(.GuideNumber == "'${selected%% *}'")| .URL' | sed -e 's/"//g')?transcode=heavy";

;#	mpv "$url"
;#}

;#launch_tv

;;Local Variables:
;;mode: clojure
;;End:
