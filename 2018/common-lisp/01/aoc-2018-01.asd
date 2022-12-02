(defsystem "aoc-2018-01"
  :author "Will Clardy <will@quexxon.net>"
  :licence "Public Domain"
  :depends-on ("series")
  :components
  ((:file "packages")
   (:file "utils" :depends-on ("packages"))
   (:file "part1" :depends-on ("utils"))
   (:file "part2" :depends-on ("utils"))))
