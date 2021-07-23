(ns com.verybigthings.commons.aws
  (:require [clojure.string :as s]
            [buddy.core.keys :as bk]
            [tick.alpha.api :as t])
  (:import (java.util Base64)
           (java.security Signature SecureRandom)
           (com.amazonaws HttpMethod)
           (com.amazonaws.regions Regions)
           (com.amazonaws.services.s3 AmazonS3ClientBuilder)
           (com.amazonaws.auth BasicAWSCredentials AWSStaticCredentialsProvider)
           (com.amazonaws.services.s3.model GeneratePresignedUrlRequest)))

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

(defn presigned-url
  ([access-key-id secret-access-key region bucket path duration]
   (presigned-url access-key-id secret-access-key region bucket path duration :put))
  ([access-key-id secret-access-key region bucket path duration method]
   (let [region (Regions/fromName region)
         method (if (= method :put) HttpMethod/PUT HttpMethod/GET)
         credentials (BasicAWSCredentials. access-key-id secret-access-key)
         s3-client (-> (AmazonS3ClientBuilder/standard)
                       (.withRegion region)
                       (.withCredentials (AWSStaticCredentialsProvider. credentials))
                       .build)
         expiration (t/inst (t/>> (t/now) (t/new-duration duration :minutes)))
         generate-presigned-url-request (doto (GeneratePresignedUrlRequest. bucket path)
                                          (.setMethod method)
                                          (.setExpiration expiration))]
     (.toString (.generatePresignedUrl s3-client generate-presigned-url-request)))))
