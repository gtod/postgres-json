(in-package :postgres-json)

(local-time:set-local-time-cl-postgres-readers)

(defmethod  pomo-timestamp-to-string ((stamp local-time:timestamp))
  "Convert local-time timestamp STAMP to a string using CL-RFC3339."
  (cl-rfc3339:rfc3339-string stamp))
