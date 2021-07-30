(ns com.verybigthings.commons.aws-test
  (:require [clojure.test :refer :all]
            [clojure.string :refer [includes? starts-with?]]
            [com.verybigthings.commons.aws :refer [cdn-cookies presigned-url]]
            [buddy.core.keys :as bk])
  (:import (java.util Base64)
           (java.security Signature)))

(def mock-data {:hostname "www.verybigthings.com"
                :epoch-expiry-timestamp 1908873209
                :cdn-access-key-id "APK1234567KPA7654321"
                :cdn-private-key "-----BEGIN RSA PRIVATE KEY-----\nMIICWgIBAAKBgH71nWq/7v3t+ZuHS0ZwZ9n4b52VQfZHUiIxM7VdVNs5TeW6wfPw5KcHiEhOy/xjXbU5bdbM8S9wuiHj8iRgmvUnEtFZW8fN6/5uBuIA99JhAaTqqSmn0jy1uDwURa66/7cjgYCmdDZEft8NWUR8o3EZlBIPRdUg9AQpkIZlYnwlAgMBAAECgYBeH6Pb/32kfmh+WLiC+VTU8W12a7CicE/BD68hN5dwi7Rnq6SdQKuT9W6hBOIQNG7Fq1T2Waw5mp4z//WF6KUYKHtNsvv/xqHjIdCOKnDYX8oMsOuyK2d8ag7LUVqO6zNMjenFkh4WyszxiKoOmWMZsVg2llN8ei8PJdE6nYlnBQJBANwo4nLBsoJv9sUJdY/4ZncZSRhipTk6tMZEjbJJwgBiy+H9YzPf6NS0wzp0yEQhrEypT+/3Yi6w37F3vWvkvksCQQCToKBFgGoUIvB5J75V/pxxM0Doc0FLj5IV7fcPqH7bnb9atdG77Jaw1d9UD84DRYNhJGaA+xyjY6kNFAZj3WlPAkB6kyjAy6cfiEbUHRgfOCWUWbegpAtpeY38dV6OzRH+NQNepzz8PMXc52dqvGpjxHuxkvK2n/1CEaf5nrkPwSNRAkAdTTSClwp3FLhGU+0jhHXowhOQD9BV/xkv4Ru4r+j+DkF877nBYyRF1S2OsdRiMdnx3he6Yh09EPrs7ZyXFcthAkABkMmzn9Hxs80EKKYOghtqhtIs8nPqqvh0FpDolQL6TM2rEIMPqzZTilXl8cbFKHqeP4TMHmpx2rr0A/sg7V6u\n-----END RSA PRIVATE KEY-----"
                :cdn-public-key "-----BEGIN PUBLIC KEY-----\nMIGeMA0GCSqGSIb3DQEBAQUAA4GMADCBiAKBgH71nWq/7v3t+ZuHS0ZwZ9n4b52VQfZHUiIxM7VdVNs5TeW6wfPw5KcHiEhOy/xjXbU5bdbM8S9wuiHj8iRgmvUnEtFZW8fN6/5uBuIA99JhAaTqqSmn0jy1uDwURa66/7cjgYCmdDZEft8NWUR8o3EZlBIPRdUg9AQpkIZlYnwlAgMBAAE=\n-----END PUBLIC KEY-----"
                :cloudfront-policy "{\"Statement\":[{\"Condition\":{\"DateLessThan\":{\"AWS:EpochTime\":1908873209}},\"Resource\":\"https://www.verybigthings.com/*\"}]}"
                :access-key-id "APK1234567KPA7654321"
                :secret-access-key "4524b08bd853463fa0aa0f75e3c67a0a"
                :aws-region "us-east-1"})

(deftest test-cdn-cookies
  (testing "Generate AWS CloudFront cookies"
    (let [cookies (cdn-cookies (:hostname mock-data)
                    (:epoch-expiry-timestamp mock-data)
                    (:cdn-access-key-id mock-data)
                    (:cdn-private-key mock-data))
          decoder (Base64/getUrlDecoder)
          policy-decoded (String. (.decode decoder (:cloudfront-policy cookies)))
          signature-decoded (.decode decoder (:cloudfront-signature cookies))
          cipher (doto (Signature/getInstance "SHA1withRSA")
                   (.initVerify (bk/str->public-key (:cdn-public-key mock-data)))
                   (.update (.getBytes policy-decoded)))]
      (is (= (:cloudfront-key-pair-id cookies) (:cdn-access-key-id mock-data)))
      (is (= policy-decoded (:cloudfront-policy mock-data)))
      (is (.verify cipher signature-decoded)))))

(deftest test-presigned-url
  (testing "Generate AWS S3 presigned URL"
    (let [access-key-id (:access-key-id mock-data)
          region (:aws-region mock-data)
          bucket (:hostname mock-data)
          path "test/test.jpg"
          expires 30
          url (presigned-url access-key-id (:secret-access-key mock-data) region
                bucket path expires)
          expected-url (str "https://" bucket ".s3.amazonaws.com/" path)]
      (is (starts-with? url expected-url))
      (is (includes? url region))
      (is (includes? url (str "X-Amz-Expires=" (* expires 60))))
      (is (includes? url (str "X-Amz-Credential=" access-key-id))))))
