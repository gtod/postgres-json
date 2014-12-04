(in-package :s-sql)

;; additional json and jsonb operators
(register-sql-operators :2+-ary :->> :#> :#>>)
