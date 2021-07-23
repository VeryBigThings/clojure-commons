(ns com.verybigthings.commons.aws
  (:require [clojure.string :as s]
            [buddy.core.keys :as bk])
  (:import (java.util Base64)
           (java.security Signature SecureRandom)))

(defn- cloudfront-policy [hostname epoch-expiry-timestamp]
  (str "{\"Statement\":[{\"Condition\":{\"DateLessThan\":{\"AWS:EpochTime\":" epoch-expiry-timestamp "}},\"Resource\":\"https://" hostname "/*\"}]}"))

(defn- safe-base64-encode [data]
  (let [encoder (java.util.Base64/getUrlEncoder)]
    (.encodeToString encoder data)))

(defn- sign [key data]
  (let [data (.getBytes data)
        signature (doto (Signature/getInstance "SHA1withRSA")
                    (.initSign (bk/str->private-key key) (SecureRandom.))
                    (.update data))]
    (safe-base64-encode (.sign signature))))

(defn cdn-cookies [hostname epoch-expiry-timestamp cdn-access-key-id cdn-private-key]
  (let [policy (cloudfront-policy hostname epoch-expiry-timestamp)]
    {:cloudfront-key-pair-id cdn-access-key-id
     :cloudfront-policy (safe-base64-encode (.getBytes policy))
     :cloudfront-signature (sign cdn-private-key policy)}))
